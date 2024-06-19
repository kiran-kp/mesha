(in-package #:mesha)

(defclass cell ()
  ((content :initarg :content)))

(defclass row ()
  ((cells :initform nil)
   (table :initarg :table)))

(defclass table ()
  ((rows :initform nil :initarg :rows)
   (column-sizes :initform nil :initarg :column-sizes)
   (parent :initarg :parent)))

(defun make-row (num-cols tbl)
  (loop for i from 0 to num-cols
        collect (make-instance 'cell :content nil :table tbl)))

(defun make-table (num-rows num-cols parent)
  (let* ((tbl (make-instance 'table
                             :column-sizes (loop for i from 0 to num-cols
                                                 collect 10.0)
                             :parent parent))
         (rows (loop for i from 0 to num-rows
                     collect (make-row num-cols tbl))))
    (setf (slot-value tbl 'rows) rows)
    tbl))

(defun table-update (ctx tbl msg)
  (log:info "Received message: ~a" msg))

(defun table-render (ctx tbl)
  (log:info "Rendering table"))
