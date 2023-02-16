(in-package #:mesha-bootstrap)

(defun get-sum (a b)
  (+ a b b))

(defun start-server ()
  (let ((slynk-init "../extern/sly/slynk/start-slynk.lisp"))
    (load slynk-init)))
