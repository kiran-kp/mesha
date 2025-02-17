(in-package #:mesha)

(defclass ring-buffer ()
  ((items :initarg :items)
   (start :initform 0)
   (end :initform 0))
  (:documentation "ring-buffer can hold at most (- (length items) 1) items
The slot at `end` is not considered valid and we set it to nil
when `end` is updated in push"))

(defun make-ring-buffer (size)
  (make 'ring-buffer :items (make-array size :initial-element nil)))

(defun ring-buffer-length (rb)
  (declare (optimize (speed 0) (debug 3)))
  (with-slots (items start end) rb
    (let ((n (length items)))      
      (if (>= end start)
          (- end start)
          (+ end (- n start))))))

(defun ring-buffer--increment (x size)
  (mod (+ x 1) size))

(defun ring-buffer-push (rb val)
  (with-slots (items start end) rb
    (setf (elt items end) val
          end (ring-buffer--increment end (length items))
          start (if (= start end) (ring-buffer--increment start (length items)) start)
          (elt items end) nil)))

(defun ring-buffer-pop-first (rb)
  (with-slots (items start end) rb
    (when (> (ring-buffer-length rb) 0)
      (let ((val (elt items start)))
        (setf (elt items start) nil
              start (ring-buffer--increment start (length items)))
        val))))

(defclass node ()
  ((id :initarg :id)
   (parent :initarg :parent)
   (content :initform nil :initarg :content)))

(defparameter *rb* (make-ring-buffer 20))

(defun main ()
  (format t "Mesha init"))
