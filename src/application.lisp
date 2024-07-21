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

(defparameter *rect-x* 1)
(defparameter *rect-y* 1)
(defparameter *move-x* 1)
(defparameter *move-y* 1)

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
           (with-output-to-string (cmds)
             (flet ((push-rect (x y w h content) (format cmds "mesha.cells.push({ rect: new PIXI.Rectangle(~a, ~a, ~a, ~a), content: \"~a\" });" x y w h content)))
               (with-slots (client) app
                 (let ((y-start 10)
                       (x 10)
                       (y 10))
                   (format cmds "clear_cells();")
                   (table-iterate (slot-value *application* 'document)
                                  (lambda (col width)
                                    (column-iterate col
                                                    (lambda (width height content)
                                                      (push-rect x y width height content)
                                                      (incf y height)))
                                    (incf x width)
                                    (setf y y-start)))
                   (format cmds "redraw_cells();")
                   (clog:js-execute client (get-output-stream-string cmds)))))))
          (:set-cell-text
           (let ((c (gethash (second cmd) *cells*))
                 (text (third cmd)))
             (setf (slot-value c 'content) text
                   (vx (slot-value c 'size)) (raylib:measure-text text 18)))
           t))))))

(defun on-new-window (body)
  (unless *application*
    (setf *application* (make-instance 'application))
    (enqueue-command '(:make-table)))
  (setf (clog:connection-data-item body "app-data") *application*)
  (clog:load-script (clog:html-document body) "https://pixijs.download/release/pixi.js")
  (clog:load-script (clog:html-document body) "js/mesha.js")
  (setf (clog:title (clog:html-document body)) "Mesha")
  (clog-gui:clog-gui-initialize body)

  (setf (slot-value *application* 'client) body)
  (enqueue-command '(:draw-cells))
  (loop while (not (slot-value *application* 'exiting))
        do (process-commands *application*)))

(defun application-run ()
  (clog:initialize 'on-new-window
                   :static-root (merge-pathnames "./www/"
                                                 (asdf:system-source-directory :mesha)))
  (clog:open-browser))
