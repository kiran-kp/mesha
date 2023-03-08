(in-package #:net)

(defclass server ()
  ((listener :initform nil)
   (client-connection :initform nil)))

(defstruct message
  (ptr nil)
  (type nil))

(defparameter *message-types*
  (list :none
        :c2s-greeting
        :s2c-greeting-response))

(defun make-server (port)
  (let ((srv (make-instance 'server)))
    (with-slots (listener) srv
      (setf listener (net-sys:create-server "0.0.0.0" port))
      (log:info "Listening on port ~a" port))
    srv))

(defun shutdown-server (srv)
  (with-slots (listener) srv
    (net-sys:close-listener listener)
    (setf listener nil)))

(defmacro with-server ((srv) &body body)
  `(let ((,srv (make-server 10988)))
     (unwind-protect (progn ,@body)
       (shutdown-server ,srv))))

(defun wait-for-client-connection (srv)
  (log:info "Waiting for client to connect")
  (with-slots (listener client-connection) srv
    (let ((conn (net-sys:accept-connection listener)))
      (log:info "Client connected")
      (setf client-connection conn))))

(defun close-client-connection (srv)
  (with-slots (client-connection) srv
    (net-sys:close-connection client-connection)
    (setf client-connection nil)))

(defmacro with-connection ((srv) &body body)
  `(progn
     (wait-for-client-connection ,srv)
     (unwind-protect (progn ,@body)
       (close-client-connection ,srv))))

(defun read-message (srv)
  (declare (optimize (debug 3) (speed 0)))
  (with-slots (client-connection) srv
    (let ((msg (net-sys:read-message client-connection)))
      (cond
        ((equal msg 0)
         (log:info "Got timeout")
         :none)
        ((equal msg #xFFFFFFFFFFFFFFFF)
         (log:info "Got error")
         :none)
        (t
         (log:info "Got message")
         (make-message :ptr msg
                       :type (nth (net-sys:get-message-type msg) *message-types*)))))))

(defun free-message (msg)
  (net-sys:free-message (message-ptr msg)))

(defun send-server-greeting-response (srv)
  (with-slots (client-connection) srv
    (net-sys:send-s2cgreeting-response client-connection "ACK")))
