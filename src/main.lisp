(in-package #:mesha)

(defparameter *application* (make-instance 'application
                                           :viewport (vec 800.0 600.0)))
(defparameter *should-init-application* t)

(define-condition unknown-command-error (error)
  ((command :initarg :command :reader command)))

(defun enqueue-command (app command)
  (with-slots (commands) app
    (unless (keywordp (first command))
      (error 'unknown-command-error :command command))
    (qpush commands command)))

(defun process-commands (app)
  (with-slots (commands) app
    (let ((cmd (qpop commands)))
      (v:debug :commands "Processing ~a" (first cmd))
      (match (first cmd)
        (:make-table
         (with-slots (document) app
           (setf document (make-table 3 5 nil))))
        (:set-cell-text
         #+nil
         (let ((c (gethash (second cmd) *cells*))
               (text (third cmd)))
           (setf (slot-value c 'content) text
                 (vx (slot-value c 'size)) (raylib:measure-text text 18)))
         t)))))

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
  (enqueue-command app '(:make-table)))

(defun main ()
  (application-run *application*
                   (lambda (a)
                     (when *should-init-application*
                       (init a)))
                   (lambda (a) (update a))
                   (lambda (a r) (render a r)))
  (v:info :application "Bye"))

#+nil
(main)
