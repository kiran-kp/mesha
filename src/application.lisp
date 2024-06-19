(in-package #:mesha)

(defclass context ()
  ((viewport :initarg :viewport)
   (style :initform (make-hash-table))
   (messages :initform (queues:make-queue :simple-queue))))

(defclass application ()
  ((document :initform nil)
   (context :initarg :context)
   (commands :initform (queues:make-queue :simple-cqueue))
   (fonts :initform (make-hash-table))))

(defun get-viewport (app)
  (slot-value (slot-value app 'context) 'viewport))

(defun application-run (app init-fn update-fn render-fn)
  (log:info "Starting application loop")
  (let ((view (get-viewport app)))
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title "Mesha" :flags '(:shown))
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (sdl2:with-event-loop (:method :poll)
            (:idle
             ()
             (funcall update-fn app)
             (funcall render-fn app)
             (sdl2:set-render-draw-color renderer 0 255 0 255)
             (sdl2:render-draw-rect renderer (sdl2:make-rect 200 200 35 35))
             (sdl2:render-present renderer)
             (sdl2:delay 33))
            (:quit
             ()
             (log:info "Exiting event loop")
             t)))))))

