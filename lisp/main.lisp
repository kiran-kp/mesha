(in-package #:mesha)

(defparameter +screen-width+ 800)
(defparameter +screen-height+ 600)

(defstruct text
  data
  (tags nil))

(defstruct image
  data
  type)

(defstruct cell
  content
  (parent nil)
  (child nil))

(defstruct cell-render-context
  parent-grid
  x-index
  y-index
  x-pos
  y-pos
  width
  height)

(defstruct grid-render-context
  x-pos
  y-pos)

(defstruct grid
  owner
  cells
  row-heights
  column-widths)

(defparameter *doc* (make-grid :cells (list
                                       (list (make-cell :content (make-text :data "Hello world - row 1"))
                                             (make-cell :content (make-text :data "Hello world - row 2"))
                                             (make-cell :content (make-text :data "Hello world - row 3")))
                                       (list (make-cell :content (make-text :data "Goodbye world - row 1"))
                                             (make-cell :content (make-text :data "Goodbye world - row 2"))
                                             (make-cell :content (make-text :data "Goodbye world - row 3"))))
                               :row-heights (list (vector 10 10 10))
                               :column-widths (list (vector 50))))

(defparameter *font* nil)

(defun load-font ()
  (setf *font*
        (raylib:load-font (namestring (asdf:system-relative-pathname 'mesha "assets/fonts/OpenSans-Regular.ttf")))))

(defun unload-font ()
  (raylib:unload-font *font*)
  (setf *font* nil))

(defgeneric render (item ctx))
(defgeneric get-bounds (item))

(defmethod get-bounds ((item text))
  (raylib:measure-text-ex *font* (text-data item) 32.0 1.0))

(defmethod render ((item text) (ctx cell-render-context))
  (raylib:draw-text-ex *font*
                       (text-data item)
                       (vec (cell-render-context-x-pos ctx)
                            (cell-render-context-y-pos ctx))
                       32.0
                       1.0
                       raylib:+white+))

(defmethod render ((item cell) (ctx cell-render-context))
  (render (cell-content item) ctx))

(defmethod render ((item grid) (ctx grid-render-context))
  (let ((cell-ctx (make-cell-render-context :parent-grid item
                                            :x-index 0
                                            :y-index 0
                                            :x-pos 200
                                            :y-pos 250
                                            :width 20
                                            :height 20)))
    (dolist (row (grid-cells item))
      (dolist (c row)
        (render c cell-ctx)
        (incf (cell-render-context-y-index cell-ctx))
        (incf (cell-render-context-y-pos cell-ctx) 30))
      (incf (cell-render-context-x-index cell-ctx))
      (setf (cell-render-context-y-index cell-ctx) 0)
      (setf (cell-render-context-y-pos cell-ctx) 250)
      (incf (cell-render-context-x-pos cell-ctx) 250))))

(defun main-loop ()
  (unwind-protect
       (progn
         (raylib:begin-drawing)
         (raylib:clear-background raylib:+black+)
         (raylib:draw-fps 20 20)
         (render *doc*
                 (make-grid-render-context :x-pos 0
                                           :y-pos 0)))
    (raylib:end-drawing)))

(defun main ()
  (unwind-protect
       (progn
         (log:info "Mesha starting up")
         (raylib:init-window +screen-width+ +screen-height+ "Mesha")
         (raylib:set-target-fps 60)
         (raylib:set-exit-key 0)
         (load-font)

         (loop while (not (raylib:window-should-close))
               do (main-loop)))
    (progn
      (unload-font)
      (raylib:close-window)
      (log:info "Exited cleanly"))))
