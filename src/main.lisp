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
        (:load-font
         (load-font)
         t)
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

(defun render (app))

(defun init (app)
  (enqueue-command app '(:make-table)))

(defun main ()
  (application-run *application*
                   (lambda (a) (when *should-init-application*
                                 (init a)))
                   (lambda (a) (update a))
                   (lambda (a) (render a)))
  (log:info "Bye"))

(main)
