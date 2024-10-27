# Application code for Mesha

(defn encode-window-flag
  [flag]
  (let [flags {:none 0
               :no-title-bar (blshift 1 0)
               :no-resize (blshift 1 1)
               :no-move (blshift 1 2)
               :no-scrollbar (blshift 1 3)
               :no-scroll-with-mouse (blshift 1 4)
               :no-collapse (blshift 1 5)
               :always-auto-resize (blshift 1 6)
               :no-background (blshift 1 7)
               :no-saved-settings (blshift 1 8)
               :no-mouse-inputs (blshift 1 9)
               :menu-bar (blshift 1 10)
               :horizontal-scrollbar (blshift 1 11)
               :no-focus-on-appearing (blshift 1 12)
               :no-bring-to-front-on-focus (blshift 1 13)
               :always-vertical-scrollbar (blshift 1 14)
               :always-horizontal-scrollbar (blshift 1 15)
               :no-nav-inputs (blshift 1 16)
               :no-nav-focus (blshift 1 17)
               :unsaved-document (blshift 1 18)
               :no-docking (blshift 1 19)}]
    (get flags flag)))

(defn encode-window-properties
  [props]
  (def window-properties
    {:is-open "\x00"
     :flags "\x01"
     :width "\x02"
     :height "\x03"
     :x "\x04"
     :y "\x05"})
  (let [output @""]
    (loop [[key value] :pairs props]
      (defn push
        [k v]
        (buffer/push output k)
        (if (bytes? v)
          (buffer/push output v)
          (buffer/push-uint32 output :le v)))
      (let [k (get window-properties key)]
        (match key
          :is-open (push k (if value 1 0))
          :flags (push k (reduce2 bor (map encode-window-flag value)))
          :width (push k value)
          :height (push k value)
          :x (push k value)
          :y (push k value))))
        output))

(def opcode
  {:window "\x00"
   :text "\x01"
   :checkbox "\x02"
   :slider-float "\x03"
   :button "\x04"
   :same-line "\x05"
   :formatted-text "\x06"
   :done "\xFF"})

(defn encode-view
  [obj]

  (def encoder
  @{:buffer @""
    :new
    (fn [self]
      (table/setproto @{:buffer (buffer/new 128)} self))
    :push-bytes
    (fn [self b]
      (buffer/push (get self :buffer) b))
    :push-boolean
    (fn [self b]
      (buffer/push-word (get self :buffer) (if b 1 0)))
    :push-integer
    (fn [self n]
      (buffer/push-word (get self :buffer) n))
    :push-string 
    (fn [self s]
      (buffer/push-word (get self :buffer) (length s))
      (buffer/push (get self :buffer) s "\0"))
    :push-message
    (fn [self root msg]
      (assert (or (tuple? msg) (keyword? msg)))
      (assert (tuple? root))
      (let [msg-encoder (:new self)]
        (if (keyword? msg)
          (do
            (:push-integer msg-encoder (+ (length root) 1))
            (each m root
              (:push-string msg-encoder m))
            (:push-string msg-encoder msg))
          (do
            (:push-integer msg-encoder (+ (length root) (length msg)))
            (each m root
              (:push-string msg-encoder m))
            (each m msg
              (:push-string msg-encoder m))))
        (:push-bytes self (:commit msg-encoder))))
    :push-done
    (fn [self]
      (:push-bytes self (get opcode :done)))
    :commit
    (fn [self]
      (let [buf (get self :buffer)
            output (buffer/new (+ 4 (length buf)))]
        (buffer/push-word output (length buf))
        (buffer/push output buf)
        output))})

  (defn make-encoder
   []
   (table/setproto @{:buffer (buffer/new 128)} encoder))

  (let [enc (make-encoder)]
    (defn encode-element [root element]
      (match element
        [:window props title & children]
        (do
          (:push-bytes enc (get opcode :window))
          (:push-integer enc (length props))
          (:push-bytes enc (encode-window-properties props))
          (:push-string enc title)
          (each child children
            (encode-element root child)))
        [:text text & args]
        (do
          (:push-bytes enc (get opcode :text))
          (:push-string enc text)
          (:push-integer enc (if args (length args) 0))
          (each arg args
            (if (string? arg)
              (:push-string enc arg))
            (if (number? arg)
              (:push-integer enc arg))))
        [:checkbox text value msg]
        (do
          (:push-bytes enc (get opcode :checkbox))
          (:push-string enc text)
          (:push-boolean enc value)
          (:push-message enc root msg))
        [:slider-float text msg]
        (do
          (:push-bytes enc (get opcode :slider-float))
          (:push-string enc text)
          (:push-message enc root msg))
        [:button text msg]
        (do
          (:push-bytes enc (get opcode :button))
          (:push-string enc text)
          (:push-message enc root msg))
        [:same-line]
        (:push-bytes enc (get opcode :same-line))))
    (encode-element [:ui-message (get obj :id)] (:view obj))
    (:push-done enc)
    (:commit enc)))

(def main-window
  @{:counter 0
    :f 0.0
    :show-demo-window false
    :id :main-window
    :update
    (fn [self msg]
      (match msg
        [:increment-counter] (put self :counter (+ 1 (get self :counter)))
        [[:show-demo-window] val] (put self :show-demo-window (= 1 val))
        [:f-slider] (put self :f (get msg 2))))
    :view
    (fn [self]
      [:window {:width 640
                :height 480
                :x 300
                :y 0
                :flags [:menu-bar]}
       "Mesha"
       [:text "This is some text"]
       [:checkbox "Checkbox demo" (get self :show-demo-window) :show-demo-window]
       (if (get self :show-demo-window)
        [:text "This is some dynamic updates to the window"])
       # [:slider-float "float" :f-slider]
       # [:button "Button" :increment-counter]
       # [:same-line]
       # [:text "counter = %d" (get self :counter)]
      ])})

(def views
  @{})

(defn push-view
  [view]
  (let [id (get view :id)]
    (put views id view)))

(defn do-update
  []
  (let [messages (get-messages)]
    (each msg messages
      (match msg
        :ui-ready
        (do
          (push-view main-window)
          (enqueue-command :push-view (get main-window :id) (encode-view main-window)))
        [[:ui-message & rest] payload]
        (let [id (get rest 0)
              view (get views id)
              view-msg (tuple/slice rest 1)]
          (:update view [view-msg payload])
          (enqueue-command :push-view (get main-window :id)  (encode-view main-window)))
        _
        (printf "Unknown message: %P" msg)))))

(defn main
  [args]
  "Entry point for Mesha"
  (setdyn *args* args)
  (enqueue-command :init-ui)
  (let [quit? false]
    (while (not quit?)
      (do-update))))
