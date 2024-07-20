(in-package #:mesha)

(defparameter *should-re-init-table* t)

(define-condition unknown-command-error (error)
  ((command :initarg :command :reader command)))


(defun update (app)
  (declare (optimize (debug 3) (speed 0)))
  (process-commands app))

(defun point-in-rectp (point r)
  (v<= (vxy r) point (v+ (vxy r) (vzw r))))

(defun render (app renderer)
  (declare (optimize (speed 0) (debug 3)))
  (let ((rects (table-render (slot-value app 'document)))
        (box (sdl2:make-rect 0 0 0 0)))
    (multiple-value-bind (mx my) (sdl2:mouse-state)
      (loop for r in rects
            do (setf (sdl2:rect-x box) (round (vx r))
                     (sdl2:rect-y box) (round (vy r))
                     (sdl2:rect-width box) (round (vz r))
                     (sdl2:rect-height box) (round (vw r)))
               (if (point-in-rectp (vec mx my) r)
                 (sdl2:set-render-draw-color renderer 0 0 255 255)
                 (sdl2:set-render-draw-color renderer 0 255 0 255))
               (sdl2:render-draw-rect renderer box)))))

(defun init (app)
  (v:info :application "Queuing startup commands")
  (when *should-re-init-table*
    (enqueue-command app '(:make-table))))

(defun main ()
  (cffi:reload-foreign-libraries)
  (application-run)
  (v:info :application "Bye"))

#+nil
(main)

