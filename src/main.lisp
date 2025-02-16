(in-package #:mesha)

(defclass node ()
  ((id :initarg :id)
   (content :initform nil :initarg :content)))

(defclass ring-buffer ()
  ((items :initarg :items)
   (first :initform 0)
   (last :initform 0)))

(defun make-ring-buffer (size)
  (make 'ring-buffer :items (make-array size :initial-element nil)))

(defun ring-buffer-size (rb)
  (declare (optimize (speed 0) (debug 3)))
  (with-slots (items first last) rb
    (let ((n (length items)))
      (+ 1
         (if (>= last first)
             (- last first)
             (+ last (- n first)))))))

(defun ring-buffer--increment (x size)
  (mod (+ x 1) size))

(defun ring-buffer-push (rb val)
  (declare (optimize (speed 0) (debug 3)))
  (with-slots (items first last) rb
    (setf last (ring-buffer--increment last (length items))
          (elt items last) val
          first (if (= first last) (ring-buffer--increment first (length items)) first))))

(defun ring-buffer-pop-first (rb)
  (declare (optimize (speed 0) (debug 3)))
  (with-slots (items first last) rb
    (assert (> (ring-buffer-size rb) 0))
    (let ((val (elt items first)))
      (setf (elt items first) nil
            last (if (= first last) (ring-buffer--increment last (length items)) last)
            first (ring-buffer--increment first (length items)))
      val)))

(defparameter *rb* (make-ring-buffer 3))

(defparameter *n* (make 'node
                        :id 10
                        :content "Test content"))

(defun main ()
  (format t "Mesha init"))
