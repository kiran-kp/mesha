(in-package #:mesha)

(defparameter *default-column-width* 50.0)
(defparameter *default-row-height* 30.0)

(defclass cell ()
  ((content :initarg :content)))

(defclass column ()
  ((cells :initform nil :initarg :cells)
   (width :initform *default-column-width*)
   (table :initarg :table)))

(defclass table ()
  ((columns :initform nil :initarg :columns)
   (row-sizes :initform nil :initarg :row-sizes)
   (parent :initarg :parent)))

(defun make-column (num-rows tbl)
  (make-instance 'column
                 :cells (loop for i from 0 to num-rows
                              collect (make-instance 'cell :content nil))
                 :table tbl))

(defun make-table (num-rows num-cols parent)
  (let* ((tbl (make-instance 'table
                             :row-sizes (loop for i from 0 to num-rows
                                                 collect *default-row-height*)
                             :parent parent))
         (cols (loop for i from 0 to num-cols
                     collect (make-column num-rows tbl))))
    (setf (slot-value tbl 'columns) cols)
    tbl))

(defun column-iterate (col fn)
  (with-slots (cells width table) col
    (with-slots (row-sizes) table
      (loop for c in cells
            for height in row-sizes
            do (funcall fn width height (slot-value c 'content))))))

(defun table-iterate (tbl fn)
  (with-slots (columns) tbl
    (loop for col in columns
          do (funcall fn col (slot-value col 'width)))))

(defun table-update (tbl msg)
  (v:info :cell "Received message: ~a" msg))

(defun column-render (col start-x start-y row-sizes)
  (with-slots (cells width) col
    (loop for c in cells
          for height in row-sizes
          with x = start-x
          collect (let ((rect (vec x start-y width height)))
                    (incf x height)
                    rect))))

(defun table-render (tbl)
  (declare (optimize (debug 3) (speed 0)))
  (when tbl
    (with-slots (columns row-sizes) tbl
      (loop for c in columns
            for i from 0
            with rects = nil
            with y = 0
            do (setf rects (append rects (column-render c 0 y row-sizes)))
               (incf x (slot-value c 'width))
            finally (return rects)))))
