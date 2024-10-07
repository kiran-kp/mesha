# Application code for Mesha

(def f (fiber/new (fn []
                    (print "Hello")
                    (yield)
                    (print "world")
                    (yield)
                    (print "from a")
                    (yield)
                    (print "fiber!"))))

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
       [:window {:title "Mesha"
                 :width 1280
                 :height 720
                 :x 0
                 :y 0
                 :flags [:no-resize
                         :no-title-bar
                         :menu-bar
                         :no-move
                         :no-bring-to-front-on-focus]}
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
