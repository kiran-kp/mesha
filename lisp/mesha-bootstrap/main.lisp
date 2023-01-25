(in-package #:mesha-bootstrap)

(defun get-sum (a b)
  (+ a b))

(defun start-server ()
  (let ((slynk-init "/home/kiran/projects/loom/extern/sly/slynk/start-slynk.lisp"))
    (load slynk-init)))
