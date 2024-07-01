(in-package #:mesha)

(defparameter *default-column-width* 50.0)
(defparameter *default-row-height* 30.0)

(defclass cell ()
  ((content :initarg :content)))

(defclass row ()
  ((cells :initform nil :initarg :cells)
   (height :initform *default-row-height*)
   (table :initarg :table)))

(defclass table ()
  ((rows :initform nil :initarg :rows)
   (column-sizes :initform nil :initarg :column-sizes)
   (parent :initarg :parent)))

(defun make-row (num-cols tbl)
  (make-instance 'row
                 :cells (loop for i from 0 to num-cols
                              collect (make-instance 'cell :content nil))
                 :table tbl))

(defun make-table (num-rows num-cols parent)
  (let* ((tbl (make-instance 'table
                             :column-sizes (loop for i from 0 to num-cols
                                                 collect *default-column-width*)
                             :parent parent))
         (rows (loop for i from 0 to num-rows
                     collect (make-row num-cols tbl))))
    (setf (slot-value tbl 'rows) rows)
    tbl))

(defun table-update (tbl msg)
  (log:info "Received message: ~a" msg))

(defun row-render (r start-x start-y column-sizes)
  (with-slots (cells height) r
    (loop for c in cells
          for width in column-sizes
          with x = start-x
          collect (let ((rect (vec x start-y width height)))
                    (incf x width)
                    rect))))

(defun table-render (tbl)
  (declare (optimize (debug 3) (speed 0)))
  (when tbl
    (with-slots (rows column-sizes) tbl
      (loop for r in rows
            for i from 0
            with rects = nil
            with y = 0
            do (setf rects (append rects (row-render r 0 y column-sizes)))
               (incf y (slot-value r 'height))
            finally (return rects)))))
