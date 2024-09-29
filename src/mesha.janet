# Application code for Mesha

(def f (fiber/new (fn []
                    (print "Hello")
                    (yield)
                    (print "world")
                    (yield)
                    (print "from a")
                    (yield)
                    (print "fiber!"))))

(defn main
  [args]
  
  (setdyn *args* args)
  (printf "Hello world from scripts! %v" (fiber/status f))
  (resume f)
  (printf "This is the next run: %v" (fiber/status f))
  (resume f)
  (printf "One more run: %v" (fiber/status f))
  (resume f)
  (printf "One more run: %v" (fiber/status f))
  (resume f)
  (printf "One more run: %v" (fiber/status f)))
