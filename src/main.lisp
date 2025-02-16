(in-package #:mesha)

(defclass node ()
  ((id :initarg :id)
   (content :initform nil :initarg :content)))

(defclass ring-buffer ()
  ((items :initarg :items)
   (start :initform 0)
   (end :initform 0)))

(defun make-ring-buffer (size)
  (make 'ring-buffer :items (make-array size :initial-element nil)))

(defun ring-buffer-size (rb)
  (declare (optimize (speed 0) (debug 3)))
  (with-slots (items start end) rb
    (let ((n (length items)))
      (+ 1
         (if (>= end start)
             (- end start)
             (+ end (- n start)))))))

(defun ring-buffer--increment (x size)
  (mod (+ x 1) size))

(defun ring-buffer-push (rb val)
  (declare (optimize (speed 0) (debug 3)))
  (with-slots (items start end) rb
    (setf end (ring-buffer--increment end (length items))
          (elt items end) val
          start (if (= start end) (ring-buffer--increment start (length items)) start))))

(defun ring-buffer-pop-first (rb)
  (declare (optimize (speed 0) (debug 3)))
  (with-slots (items start end) rb
    (assert (> (ring-buffer-size rb) 0))
    (let ((val (elt items start)))
      (setf (elt items start) nil
            end (if (= start end) (ring-buffer--increment end (length items)) end)
            start (ring-buffer--increment start (length items)))
      val)))

(defparameter *rb* (make-ring-buffer 3))

(defparameter *n* (make 'node
                        :id 10
                        :content "Test content"))

(defun main ()
  (format t "Mesha init"))
