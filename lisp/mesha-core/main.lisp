(in-package #:mesha-core)

(defclass application ()
  ((models :initform (make-hash-table))))

(defun start-slynk-server ()
  (let ((slynk-init "../extern/sly/slynk/start-slynk.lisp"))
    (load slynk-init)))

(defun initialize ()
  (pushnew "/home/kiran/projects/mesha/lisp" asdf:*central-registry*))

(defun update (msg model-id)
  '(:command-none))
