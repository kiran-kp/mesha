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

(defn encode-windows-properties
  [props]
  (def window-properties
    {:is-open 0
     :flags 1
     :width 2
     :height 3
     :x 4
     :y 5})
  (let [output @[]
        flags (map encode-window-flag (get props :flags))]
    (loop [[key value] :pairs props]
      (array/push output (get window-properties key))
      (match key
        :is-open (array/push output (if value 1 0))
        :flags (array/concat output (reduce bitor flags))
        :width (array/push output value)
        :height (array/push output value)
        :x (array/push output value)
        :y (array/push output value)))))

(defn encode-view
  [view]
  (match view
    [:window props title & children]
    (let [props (encode-props props)
          children (map encode-view children)]
      (list :window props title children))
    [:text text]
    (list :text text)
    [:checkbox text key]
    (list :checkbox text key)
    [:slider-float text key]
    (list :slider-float text key)
    [:button text key]
    (list :button text key)
    [:same-line]
    [:same-line]
    [:text fmt & args]
    (list :text fmt args)))

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
