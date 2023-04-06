(in-package #:mesha)

(defparameter +screen-width+ 800)
(defparameter +screen-height+ 600)

(defun main-loop ()
  (raylib:begin-drawing)
  (raylib:clear-background raylib:+black+)
  (raylib:draw-fps 20 20)
  (raylib:draw-text "Congrats! Mesha can draw now!" 190 200 20 raylib:+lightgray+)
  (raylib:end-drawing))

(defun start-game ()
  (log:info "Mesha starting up")
  (raylib:init-window +screen-width+ +screen-height+ "Mesha")
  (raylib:set-target-fps 60)
  (raylib:set-exit-key 0)

  (loop while (not (raylib:window-should-close))
        do (main-loop))

  (raylib:close-window)
  (log:info "Exited cleanly"))

(defun main ()
  (start-game))
