(in-package #:mesha)

(defparameter *application* (make-instance 'application
                                           :context (make-instance 'context
                                                                   :viewport (vec 800.0 600.0))))
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
      (match (first cmd)
        (:make-table
         (with-slots (document) app
           (log:info "Making table")
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

(defun render (app renderer)
  (declare (optimize (speed 0) (debug 3)))
  (sdl2:set-render-draw-color renderer 0 255 0 255)
  (let ((rects (table-render (slot-value app 'document)))
        (box (sdl2:make-rect 0 0 0 0)))
    (loop for r in rects
          do (setf (sdl2:rect-x box) (round (vx r))
                   (sdl2:rect-y box) (round (vy r))
                   (sdl2:rect-width box) (round (vz r))
                   (sdl2:rect-height box) (round (vw r)))
             (sdl2:render-draw-rect renderer box))))

(defun init (app)
  (log:info "Queuing command")
  (enqueue-command app '(:make-table)))

(defun main ()
  (application-run *application*
                   (lambda (a) (when *should-init-application*
                                 (init a)))
                   (lambda (a) (update a))
                   (lambda (a r) (render a r)))
  (log:info "Bye"))

#+nil
(main)
