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

(defclass cell ()
  ((parent :initarg :parent :initform 0)
   (children :initarg :children :initform nil)
   (content :initarg :content)))

(defstruct visible-cell
  top-left
  size
  id)

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
                                         :visible-cells (list
                                                         (make-visible-cell :top-left (vec 200.0 50.0) :size (vec 400.0 100.0) :id 1)
                                                         (make-visible-cell :top-left (vec 200.0 150.0) :size (vec 400.0 100.0) :id 2)
                                                         (make-visible-cell :top-left (vec 200.0 250.0) :size (vec 200.0 100.0) :id 3)
                                                         (make-visible-cell :top-left (vec 400.0 250.0) :size (vec 200.0 50.0) :id 4)
                                                         (make-visible-cell :top-left (vec 400.0 300.0) :size (vec 200.0 50.0) :id 5)
                                                         (make-visible-cell :top-left (vec 200.0 350.0) :size (vec 200.0 100.0) :id 6)
                                                         (make-visible-cell :top-left (vec 400.0 350.0) :size (vec 50.0 100.0) :id 7)
                                                         (make-visible-cell :top-left (vec 450.0 350.0) :size (vec 50.0 100.0) :id 8)
                                                         (make-visible-cell :top-left (vec 500.0 350.0) :size (vec 50.0 100.0) :id 9)
                                                         (make-visible-cell :top-left (vec 550.0 350.0) :size (vec 50.0 100.0) :id 10))
                                         :viewport (make-viewport :width 800 :height 600))))


(defparameter *font* nil)

(defparameter *cells* (alexandria:alist-hash-table (list (cons 1 (make-instance 'cell :content "Test"))
                                                         (cons 2 (make-instance 'cell :content 123))
                                                         (cons 3 (make-instance 'cell :children (list 4 5) :content "Parent"))
                                                         (cons 4 (make-instance 'cell :parent 3 :content 1337))
                                                         (cons 5 (make-instance 'cell :parent 3 :content "Kiran"))
                                                         (cons 6 (make-instance 'cell :children (list 7 8 9 10 11) :content "List"))
                                                         (cons 7 (make-instance 'cell :parent 6 :content 1))
                                                         (cons 8 (make-instance 'cell :parent 6 :content 2))
                                                         (cons 9 (make-instance 'cell :parent 6 :content 3))
                                                         (cons 10 (make-instance 'cell :parent 6 :content 4)))))

(defparameter *messages* nil)

(define-condition unknown-command-error (error)
  ((command :initarg :command :reader command)))

(defun enqueue-command (command)
  (with-slots (commands) *application*
    (unless (keywordp (first command))
      (error 'unknown-command-error :command command))
    (qpush commands command)))

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

(defun main-loop ()
  (process-commands)

  (raylib:with-drawing
    (raylib:clear-background :black)
    (raylib:draw-fps 20 20)

    (setf *messages* nil)
    
    (with-slots (context) *application*
      (with-slots (visible-cells) context
        (let ((hovering nil))
          (loop for c in visible-cells
                do
                   (let ((rect (ui:make-rectangle-v (visible-cell-top-left c) (visible-cell-size c)))
                         (cursor (raylib:get-mouse-position)))
                     (when (raylib:check-collision-point-rec cursor rect)
                       (setf hovering rect))
                     (raylib:draw-rectangle-lines-ex rect 1.0 :red)))
          (when hovering
            (raylib:draw-rectangle-lines-ex hovering 2.5 :blue)))))))

(defun main ()
  (log:info "Mesha starting up")
  (raylib:with-window (800 600 "Mesha")
    (unwind-protect
         (progn
           (raylib:set-target-fps 60)
           (raylib:set-exit-key 0)
           (enqueue-command '(:load-font))
           (loop while (not (raylib:window-should-close))
                 do (main-loop)))
      (progn
        (unload-font)
        (log:info "Exited cleanly")))))
