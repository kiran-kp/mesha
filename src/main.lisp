(in-package #:mesha)

(defclass client ()
  ((connection :initarg :connection)))

(defclass cell ()
  ((parent :initarg :parent :initform 0)
   (children :initarg :children :initform nil)
   (content :initarg :content)))

(defclass document ()
  ((filepath :initarg :filepath)
   (connection :initform nil)))

(defvar *clients* (make-hash-table))
(defvar *server* nil)

(defvar *current-client* nil)

;; Root is 0. Rows are children of 0 by default
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

(defun send-websocket-message (client msg)
  (websocket-driver:send (slot-value client 'connection) msg))

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

(defun handle-client-message (connection msg-str)
  (declare (optimize (debug 3) (speed 0)))
  (log:info "Received message: ~a" msg-str)
  (let ((msg (with-input-from-string (s msg-str) (read s)))
        (client (gethash connection *clients*)))
    (ccase (car msg)
      (:get-block (send-block-update client (getf msg :id)))
      (:set-viewport (log:info "Setting viewport")))))

(defun setup-websocket (env)
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda () (handle-new-connection ws)))
    (websocket-driver:on :message ws
                         (lambda (msg) (handle-client-message ws msg)))
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
  (websocket-driver:send-text (slot-value client 'connection) msg))

(defun send-block-update (client block-id)
  (let* ((val (gethash block-id *blocks*))
         (message (alexandria:plist-hash-table
                   `(:operation "set-block"
                     :id ,block-id
                     :value ,(format nil "~a" val)))))
    (send-message client (yason:with-output-to-string* () (yason:encode message)))))

(defun main ()
  (log:info "Starting mesha server")
  
  (setf yason:*symbol-encoder* #'yason:encode-symbol-as-lowercase
        yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase)

  (setf *server* (clack:clackup (lambda (env) (funcall 'setup-websocket env))
                                          ;; :address "0.0.0.0"
                                          :port 13330)))

(defun shutdown ()
  (clack:stop *server*)
  (setf *current-client* nil))
