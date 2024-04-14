(in-package #:mesha)

(defstruct viewport
  width
  height)

(defstruct text
  data
  (tags nil)
  (edit nil))

(defstruct text-render-context
  font
  position
  size
  spacing
  color)

(defstruct cellv1
  content
  (parent nil)
  (child nil))

(defstruct cellv1-render-context
  parent-grid
  row
  column
  position
  size)

(defstruct grid-render-context
  position)

(defstruct grid
  owner
  cellv1s
  row-heights
  column-widths)

(defclass cell ()
  ((parent :initarg :parent :initform 0)
   (children :initarg :children :initform nil)
   (content :initarg :content)))

(defclass context ()
  ((visible-cells :initarg :visible-cells :initform nil)
   (viewport :initarg :viewport)
   (style :initform (make-hash-table))
   (messages :initform (queues:make-queue :simple-queue))))

(defclass application ()
  ((context :initarg :context)
   (commands :initform (queues:make-queue :simple-cqueue))
   (fonts :initform (make-hash-table))))

(defparameter *application*
  (make-instance 'application
                 :context (make-instance 'context
                                         :visible-cells (alexandria:alist-hash-table (list (cons 1 (make-instance 'cell :content "Test"))
                                                                                           (cons 2 (make-instance 'cell :content 123))
                                                                                           (cons 3 (make-instance 'cell :children (list 4 5) :content "Parent"))
                                                                                           (cons 4 (make-instance 'cell :parent 3 :content 1337))
                                                                                           (cons 5 (make-instance 'cell :parent 3 :content "Kiran"))
                                                                                           (cons 6 (make-instance 'cell :children (list 7 8 9 10 11) :content "List"))
                                                                                           (cons 7 (make-instance 'cell :parent 6 :content 1))
                                                                                           (cons 8 (make-instance 'cell :parent 6 :content 2))
                                                                                           (cons 9 (make-instance 'cell :parent 6 :content 3))
                                                                                           (cons 10 (make-instance 'cell :parent 6 :content 4))))
                                         :viewport (make-viewport :width 800 :height 600))))


;; Root is 0. Rows are children of 0 by default
(defparameter *cells* )

(defparameter *font* nil)

(defparameter *messages* nil)

(define-condition unknown-command-error (error)
  ((command :initarg :command :reader command)))

(defun enqueue-command (command)
  (with-slots (commands) *application*
    (unless (keywordp (first command))
      (error 'unknown-command-error :command command))
    (qpush commands (cons command args))))

(defun process-commands ()
  (with-slots (commands) *application*
    (let ((cmd (qpop commands)))
      (match (first cmd)
        (:load-font
         (load-font))))))

(defun load-font ()
  (setf *font*
        (raylib:load-font (namestring (asdf:system-relative-pathname 'mesha "assets/fonts/OpenSans-Regular.ttf")))))

(defun unload-font ()
  (raylib:unload-font *font*)
  (setf *font* nil))

(defgeneric update (item msg))
(defgeneric render (item ctx))
(defgeneric get-bounds (item))

(defmethod get-bounds ((item text))
  (raylib:measure-text-ex *font* (text-data item) 32.0 1.0))

(defmethod get-bounds ((item cellv1))
  (v+ (vec 10.0 5.0) (get-bounds (cellv1-content item))))

(defmethod render ((item text) (ctx text-render-context))
  (raylib:draw-text-ex (text-render-context-font ctx)
                       (text-data item)
                       (text-render-context-position ctx)
                       (text-render-context-size ctx)
                       (text-render-context-spacing ctx)
                       (if (text-edit item) :red (text-render-context-color ctx))))

(defmethod update ((item cellv1) msg)
  (match msg
    ('enter-edit-mode
     (setf (text-edit (cellv1-content item)) t))
    ('exit-edit-mode
     (setf (text-edit (cellv1-content item)) nil))
    ('received-input-backspace
     (setf (text-data (cellv1-content item))
           (str:fit (- (length (text-data (cellv1-content item))) 1)
                    (text-data (cellv1-content item))
                    :ellipsis nil)))
    ((list 'received-input-codepoint c)
     (setf (text-data (cellv1-content item)) (str:concat (text-data (cellv1-content item)) (format nil "~a" (code-char c)))))))

(defmethod render ((item cellv1) (ctx cellv1-render-context))
  (let* ((bounds (cellv1-render-context-size ctx))
         (pos (cellv1-render-context-position ctx))
         (rect (ui:make-rectangle-v pos bounds))
         (is-hovering (raylib:check-collision-point-rec (raylib:get-mouse-position) rect))
         (is-clicked (and is-hovering (raylib:is-mouse-button-pressed :mouse-button-left))))
    (raylib:draw-rectangle-v pos bounds :gray)
    (raylib:draw-rectangle-lines (round (vx pos))
                                 (round (vy pos))
                                 (round (vx bounds))
                                 (round (vy bounds))
                                 (if is-hovering :white :black))
    (when is-clicked
      (push (list item 'enter-edit-mode) *messages*))

    (when (text-edit (cellv1-content item))
      (let ((k (raylib:get-key-pressed)))
        (when (or (equal k :key-escape)
                  (equal k :key-enter))
          (push (list item 'exit-edit-mode) *messages*))
        (when (equal k :key-backspace)
          (push (list item 'received-input-backspace) *messages*)))
      (let ((c (raylib::get-char-pressed)))
        (when (not (equal 0 c))
          (push (list item (list 'received-input-codepoint c)) *messages*))))
    
    (render (cellv1-content item)
            (make-text-render-context :font *font*
                                      :position (v+ pos (vec 5 5))
                                      :size 32.0
                                      :spacing 1.0
                                      :color :white))))

(defun grid-iterate-cellv1s (g fn)
  (let ((col 0))
    (dolist (column-content (grid-cellv1s g))
      (let ((row 0))
        (dolist (cellv1 column-content)
          (funcall fn row col cellv1)
          (incf row)))
      (incf col))))

(defun grid-get-row-height (g row)
  (let ((row-heights (grid-row-heights g)))
    (nth row row-heights)))

(defun grid-get-column-width (g col)
  (let ((col-widths (grid-column-widths g)))
    (nth col col-widths)))

(defmethod render ((item grid) (ctx grid-render-context))
  (let ((cellv1-ctx (make-cellv1-render-context :parent-grid item
                                            :row 0
                                            :column 0
                                            :position (vcopy (grid-render-context-position ctx))
                                            :size (vec 0 0))))
    (grid-iterate-cellv1s item
                        (lambda (row col c)
                          (when (equalp row 0)
                            (setf (vy (cellv1-render-context-position cellv1-ctx))
                                  (vy (grid-render-context-position ctx)))
                            (unless (equalp col 0)
                              (incf (vx (cellv1-render-context-position cellv1-ctx))
                                    (grid-get-column-width item (- col 1)))))

                          (setf (cellv1-render-context-size cellv1-ctx) (vec (grid-get-column-width item col) (grid-get-row-height item row))
                                (cellv1-render-context-row cellv1-ctx) row
                                (cellv1-render-context-column cellv1-ctx) col)

                          (render c cellv1-ctx)

                          (incf (vy (cellv1-render-context-position cellv1-ctx)) (grid-get-row-height item row))))))

(defun main-loop ()
  (process-commands)

  (dolist (msg-params *messages*)
    (update (nth 0 msg-params) (nth 1 msg-params)))

  (raylib:with-drawing
    (raylib:clear-background :black)
    (raylib:draw-fps 20 20)

    (setf *messages* nil)
    
    (render *doc*
            (make-grid-render-context :position (vec 100.0 200.0)))))

(defun main ()
  (log:info "Mesha starting up")
  (raylib:with-window ((viewport-width *viewport*) (viewport-height *viewport*) "Mesha")
    (unwind-protect
         (progn
           (raylib:set-target-fps 60)
           (raylib:set-exit-key 0)
           (enqueue-command :load-font)
           (loop while (not (raylib:window-should-close))
                 do (main-loop)))
      (progn
        (unload-font)
        (log:info "Exited cleanly")))))
