(in-package #:mesha)

(defparameter *should-re-init-table* t)

(define-condition unknown-command-error (error)
  ((command :initarg :command :reader command)))

(defun update (app)
  (declare (optimize (debug 3) (speed 0)))
  (process-commands app))

(defun point-in-rectp (point r)
  (v<= (vxy r) point (v+ (vxy r) (vzw r))))

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

