# Application code for Mesha

(import ./application/view)

(def main-window
  @{:counter 0
    :f 0.0
    :show-demo-window false
    :id :main-window
    :update
    (fn [self msg]
      (printf "Updating view: %q" msg)
      (match msg
        [[:increment-counter]] (put self :counter (+ 1 (get self :counter)))
        [[:show-demo-window] [val]] (put self :show-demo-window (= 1 val))
        [[:f-slider] [val]] (put self :f val)))
    :view
    (fn [self]
      [:window {:width 640
                :height 480
                :x 300
                :y 0
                :flags [:menu-bar]}
       "Mesha"
       [:text (string/format "This is some text: %d" (get self :counter))]
       [:same-line]
       [:button "Button" :increment-counter]
       [:checkbox "Checkbox demo" (get self :show-demo-window) :show-demo-window]
       (if (get self :show-demo-window)
        [:text "This is some dynamic updates to the window"])
       # [:slider-float "float" :f-slider]
      ])})

(defn do-update
  []
  (var should-continue true)
  (let [msg (get-message)]
    (printf "Got message %q" msg)
    (match msg
      :ui-ready
      (do
        (view/submit main-window)
        (enqueue-command :push-view (get main-window :id) (view/encode main-window)))
      :quit
      (set should-continue false)
      [[:ui-message & rest] payload]
      (let [id (get rest 0)
            view-msg (tuple/slice rest 1)]
        (view/do-update id view-msg payload))
      _
      (printf "Unknown message: %q" msg)))
  should-continue)

(defn main
  [args]
  "Entry point for Mesha"
  (setdyn *args* args)
  (enqueue-command :init-ui)
  (forever
    (if (not (do-update))
      (break))))
