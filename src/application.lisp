(in-package #:mesha)

(defparameter *application* nil)

(defclass application ()
  ((document :initform nil)
   (client :initform nil)
   (messages :initform (sb-concurrency:make-mailbox :name "messages"))
   (context :initarg :context)
   (commands :initform (sb-concurrency:make-mailbox :name "commands"))
   (fonts :initform (make-hash-table))
   (exiting :initform nil)))

#+nil
(defun application-run (app init-fn update-fn render-fn)
  (v:info :application "Starting application loop")
  (let ((view (get-viewport app)))
    (sdl2:with-init (:everything)
      (let ((ctx (mesha-native:create-context)))
        (assert ctx)
        (setf (slot-value app 'context) ctx))
      (funcall init-fn app)
      (sdl2:with-window (win :title "Mesha" :flags '(:shown) :w (round (vx view)) :h (round (vy view)))
        (sdl2:with-renderer (renderer win :flags '(:accelerated :presentvsync))
          (sdl2:with-event-loop (:method :poll)
            (:idle
             ()
             (sdl2:set-render-draw-color renderer 0 0 0 255)
             (sdl2:render-clear renderer)
             (funcall update-fn app)
             (funcall render-fn app renderer)
             (sdl2:render-present renderer))
            (:quit
             ()
             ()
             (v:info :application "Exiting event loop")
             t)))))))

(defparameter *rect-x* 1)
(defparameter *rect-y* 1)
(defparameter *move-x* 1)
(defparameter *move-y* 1)

(defun render (body canvas ctx)
  (clog:clear-rect ctx 0 0 (clog:width canvas) (clog:height canvas))
  (setf (clog:stroke-style ctx) :black)

  (incf *rect-x* (* 5 *move-x*))
  (incf *rect-y* (* 5 *move-y*))

  (unless (< 0 *rect-x* 1820)
    (setf *move-x* (* -1 *move-x*)))
  (unless (< 0 *rect-y* 1000)
    (setf *move-y* (* -1 *move-y*)))

  (setf (clog:fill-style ctx) :red)
  (clog:fill-rect ctx *rect-x* *rect-y* 100 50))

(defun enqueue-command (command)
  (with-slots (commands) *application*
    (unless (keywordp (first command))
      (error 'unknown-command-error :command command))
    (sb-concurrency:send-message commands command)))

(defun process-commands (app)
  (with-slots (commands) app
    (let ((cmd (sb-concurrency:receive-message-no-hang commands)))
      (when cmd
        (v:debug :commands "Processing ~a" (first cmd))
        (match (first cmd)
          (:exit
           (setf (slot-value *application* 'exiting) t))
          (:make-table
           (with-slots (document) app
             (setf document (make-table 3 5 nil))))
          (:draw-cells
           (with-slots (client) app
             (let ((y-start 10)
                   (x 10)
                   (y 10))
               (clog:js-execute client "clear_cells();")
               (table-iterate (slot-value *application* 'document)
                              (lambda (col width)
                                (column-iterate col
                                                (lambda (width height content)
                                                  (declare (ignore content)
                                                           (optimize (debug 3) (speed 0)))
                                                  (clog:js-execute client
                                                                   (format nil
                                                                           "mesha.cells.push({ rect: new PIXI.Rectangle(~a, ~a, ~a, ~a) });"
                                                                           x
                                                                           y
                                                                           width
                                                                           height))
                                                  (incf y height)))
                                (incf x width)
                                (setf y y-start)))
               (clog:js-execute client "redraw_cells();"))))
          (:set-cell-text
           #+nil
           (let ((c (gethash (second cmd) *cells*))
                 (text (third cmd)))
             (setf (slot-value c 'content) text
                   (vx (slot-value c 'size)) (raylib:measure-text text 18)))
           t))))))

(defun on-new-window (body)
  (unless *application*
    (setf *application* (make-instance 'application)))
  (setf (clog:connection-data-item body "app-data") *application*)
  (clog:load-script (clog:html-document body) "https://pixijs.download/release/pixi.js")
  (clog:load-script (clog:html-document body) "js/mesha.js")
  (setf (clog:title (clog:html-document body)) "Mesha")
  (clog-gui:clog-gui-initialize body)

  (setf (slot-value *application* 'client) body)
  (loop while (not (slot-value *application* 'exiting))
        do (process-commands *application*)))

(defun application-run ()
  (clog:initialize 'on-new-window
                   :static-root (merge-pathnames "./www/"
                                                 (asdf:system-source-directory :mesha)))
  (clog:open-browser))
