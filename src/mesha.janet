# Application code for Mesha

(def f (fiber/new (fn []
                    (print "Hello")
                    (yield)
                    (print "world")
                    (yield)
                    (print "from a")
                    (yield)
                    (print "fiber!"))))

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
    {:is-open 0
     :flags 1
     :width 2
     :height 3
     :x 4
     :y 5})
  (let [output @""
        flags (map encode-window-flag (get props :flags))]
    (loop [[key value] :pairs props]
      (buffer/push output (get window-properties key))
      (match key
        :is-open (buffer/push output (if value 1 0))
        :flags (buffer/push-word output (reduce2 bor flags))
        :width (buffer/push-word output value)
        :height (buffer/push-word output value)
        :x (buffer/push-word output value)
        :y (buffer/push-word output value)))
        output))

(def opcode
  {:window 0
   :text 1
   :checkbox 2
   :slider-float 3
   :button 4
   :same-line 5
   :formatted-text 6})

(def messages
  @{})

(defn encode-view
  [view]
  (let [output @""]
    (defn encode-element [element]
      (match element
        [:window props title & children]
        (do
          (buffer/push output (get opcode :window))
          (buffer/push-word output (length props))
          (buffer/push output (encode-window-properties props))
          (buffer/push-word output (length title))
          (buffer/push output title)
          (each child children
            (encode-element child)))
        [:text text & args]
        (do
          (buffer/push output (get opcode :text))
          (buffer/push-word output (length text))
          (buffer/push output text)
          (buffer/push-word output (if args (length args) 0))
          (each arg args
            (if (string? arg)
              (do (buffer/push-word output (length arg))
                       (buffer/push output arg)))
            (if (number? arg)
              (do (buffer/push-word output arg)))))
        [:checkbox text key]
        (do
          (buffer/push output (get opcode :checkbox))
          (buffer/push-word output (length text))
          (buffer/push output text)
          (put messages (hash key) key)
          (buffer/push-word output (hash key)))
        [:slider-float text key]
        (do
          (buffer/push output (get opcode :slider-float))
          (buffer/push-word output (length text))
          (buffer/push output text)
          (put messages (hash key) key)
          (buffer/push-word output (hash key)))
        [:button text key]
        (do
          (buffer/push output (get opcode :button))
          (buffer/push-word output (length text))
          (buffer/push output text)
          (put messages (hash key) key)
          (buffer/push-word output (hash key)))
        [:same-line]
        (buffer/push output (get opcode :same-line))))
    (encode-element view)
    output))

(def main-window
  @{:counter 0
    :f 0.0
    :show-demo-window true
    :show-another-window false
    :update
    (fn [self msg]
      (match msg
        [:increment-counter] (put self :counter (+ 1 (get self :counter)))
        [:show-demo-window] (put self :show-demo-window (not (get self :show-demo-window)))
        [:show-another-window] (put self :show-another-window (not (get self :show-another-window)))
        [:f-slider] (put self :f (get msg 2))))
    :view
    (fn [self]
      [:window {:width 1280
                :height 720
                :x 0
                :y 0
                :flags [:no-resize
                        :no-title-bar
                        :menu-bar
                        :no-move
                        :no-bring-to-front-on-focus]}
       "Mesha"
       [:text "This is some useful text."]
       [:checkbox "Demo Window" :show-demo-window]
       [:checkbox "Another Window" :show-another-window]
       [:slider-float "float" :f-slider]
       [:button "Button" :increment-counter]
       [:same-line]
       [:text "counter = %d" (get self :counter)]])})

(defn main
  [args]
  "Entry point for Mesha"
  (setdyn *args* args)
  (printf "Starting UI. %v" (fiber/status f))
  (enqueue-command :init-ui)
  (resume f)
  (printf "This is the next run: %v" (fiber/status f))
  (resume f)
  (printf "One more run: %v" (fiber/status f))
  (resume f)
  (printf "One more run: %v" (fiber/status f))
  (resume f)
  (printf "One more run: %v" (fiber/status f)))
