(in-package #:mesha)

(defparameter +screen-width+ 800)
(defparameter +screen-height+ 600)

(defstruct text
  data
  (tags nil))

(defstruct text-render-context
  font
  position
  size
  spacing
  color)

(defstruct cell
  content
  (parent nil)
  (child nil))

(defstruct cell-render-context
  parent-grid
  row
  column
  position
  size)

(defstruct grid-render-context
  position)

(defstruct grid
  owner
  cells
  row-heights
  column-widths)

(defparameter *doc* (make-grid :cells (list
                                       (list (make-cell :content (make-text :data "Apple"))
                                             (make-cell :content (make-text :data "Watermelon"))
                                             (make-cell :content (make-text :data "Kiwi"))
                                             (make-cell :content (make-text :data "Orange")))
                                       (list (make-cell :content (make-text :data "10"))
                                             (make-cell :content (make-text :data "20"))
                                             (make-cell :content (make-text :data "30"))
                                             (make-cell :content (make-text :data "40")))
                                       (list (make-cell :content (make-text :data "Red"))))
                               :row-heights (list 50 50 50 50)
                               :column-widths (list 250 250 250)))

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

(defmethod get-bounds ((item cell))
  (v+ (vec 10.0 5.0) (get-bounds (cell-content item))))

(defmethod render ((item text) (ctx text-render-context))
  (raylib:draw-text-ex (text-render-context-font ctx)
                       (text-data item)
                       (text-render-context-position ctx)
                       (text-render-context-size ctx)
                       (text-render-context-spacing ctx)
                       (text-render-context-color ctx)))

(defmethod render ((item cell) (ctx cell-render-context))
  (let ((bounds (cell-render-context-size ctx))
        (pos (cell-render-context-position ctx)))
    (raylib:draw-rectangle-v pos bounds raylib:+gray+)
    (raylib:draw-rectangle-lines (round (vx pos)) (round (vy pos)) (round (vx bounds)) (round (vy bounds)) raylib:+black+)
    (render (cell-content item)
            (make-text-render-context :font *font*
                                      :position (v+ pos (vec 5 5))
                                      :size 32.0
                                      :spacing 1.0
                                      :color raylib:+white+))))

(defun grid-iterate-cells (g fn)
  (let ((col 0))
    (dolist (column-content (grid-cells g))
      (let ((row 0))
        (dolist (cell column-content)
          (funcall fn row col cell)
          (incf row)))
      (incf col))))

(defun grid-get-row-height (g row)
  (let ((row-heights (grid-row-heights g)))
    (nth row row-heights)))

(defun grid-get-column-width (g col)
  (let ((col-widths (grid-column-widths g)))
    (nth col col-widths)))

(defmethod render ((item grid) (ctx grid-render-context))
  (let ((cell-ctx (make-cell-render-context :parent-grid item
                                            :row 0
                                            :column 0
                                            :position (vcopy (grid-render-context-position ctx))
                                            :size (vec 0 0))))
    (grid-iterate-cells item
                        (lambda (row col c)
                          (when (equalp row 0)
                            (setf (vy (cell-render-context-position cell-ctx))
                                  (vy (grid-render-context-position ctx)))
                            (unless (equalp col 0)
                              (incf (vx (cell-render-context-position cell-ctx))
                                    (grid-get-column-width item (- col 1)))))

                          (setf (cell-render-context-size cell-ctx) (vec (grid-get-column-width item col) (grid-get-row-height item row))
                                (cell-render-context-row cell-ctx) row
                                (cell-render-context-column cell-ctx) col)

                          (render c cell-ctx)

                          (incf (vy (cell-render-context-position cell-ctx)) (grid-get-row-height item row))))))

(defun main-loop ()
  (unwind-protect
       (progn
         (raylib:begin-drawing)
         (raylib:clear-background raylib:+black+)
         (raylib:draw-fps 20 20)
         (render *doc*
                 (make-grid-render-context :position (vec 100.0 200.0))))
    (raylib:end-drawing)))

(defun main ()
  (unwind-protect
       (progn
         (log:info "Mesha starting up")
         (raylib:init-window +screen-width+ +screen-height+ "Mesha")
         (raylib:set-target-fps 60)
         (raylib:set-exit-key 0)
         (load-font)

         ;; (main-loop)
         (loop while (not (raylib:window-should-close))
               do (main-loop))
         )
    (progn
      (unload-font)
      (raylib:close-window)
      (log:info "Exited cleanly"))))
