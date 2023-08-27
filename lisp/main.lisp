(in-package #:mesha)

(defclass document ()
  ((filepath :initarg :filepath)
   (connection :initform nil)))

(defclass client ()
  ((connection :initarg :connection)))

(defvar *clients* (make-hash-table))
(defvar *server* nil)

(defvar *current-client* nil)

(defparameter *blocks* (alexandria:plist-hash-table '(1 "Test"
                                                      2 123
                                                      3 "A longer line of text"
                                                      4 "https://www.google.com/"
                                                      5 (1 2 3 4))))

(setf yason:*symbol-encoder* #'yason:encode-symbol-as-lowercase
      yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase)

(defun handle-new-connection (connection)
  (log:info "Client connected!")
  (setf (gethash connection *clients*) (make-instance 'client :connection connection)
        *current-client* (gethash connection *clients*)))

(defun handle-close-connection (connection)
  (log:info "Client disconnected")
  (let ((client (gethash connection *clients*)))
    (remhash connection *clients*)
    (when (equal client *current-client*)
      (setf *current-client* nil))))

(defun handle-message-received (connection msg)
  (declare (ignore connection))
  (log:info "Received message: ~a" (with-input-from-string (s msg) (read s))))

(defun setup-mesha-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda () (handle-new-connection ws)))
    (websocket-driver:on :message ws
                         (lambda (msg) (handle-message-received ws msg)))
    (websocket-driver:on :error ws
                         (lambda (err) (log:error "~a" err)))
    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws))))

(defun send-message (client msg)
  (websocket-driver:send (slot-value client 'connection) msg))

(defun send-block-update (client block-id)
  (let* ((val (gethash block-id *blocks*))
         (message (alexandria:plist-hash-table
                   `(:operation "update-block"
                     :id ,block-id
                     :value ,val))))
    (send-message client (yason:with-output-to-string* () (yason:encode message)))))

(defun main ()
  (log:info "Starting mesha server")
  (setf *server*
        (clack:clackup
         (lambda (env)
           (setup-mesha-server env))
         :port 13307)))

(defun shutdown ()
  (clack:stop *server*)
  (setf *server* nil
        *current-client* nil))
