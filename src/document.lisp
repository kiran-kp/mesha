(in-package #:mesha)

(defparameter *default-column-width* 50.0)
(defparameter *default-row-height* 30.0)

(defclass document ()
  ((connection :initarg :connection)
   (row-heights :initform (make-hash-table))
   (column-widths :initform (make-hash-table))))

(defparameter *doc* (make-instance 'document :connection (dbi:connect :sqlite3 :database-name "/home/kiran/notes.mesha")))

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

(defparameter +create-notes-table-statement+
  (sxql:create-table :notes
      ((id :type 'integer :primary-key t :auto-increment t)
       (mparent :type 'integer)
       (mrow :type 'integer)
       (mcolumn :type 'integer)
       (mcontent :type 'blob)
       (mcreated :type 'text)
       (mupdated :type 'text))
    (sxql:foreign-key '(:mparent) :references '(:notes :id))
    (sxql:unique-key '(:mparent :mrow :mcolumn))))

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

