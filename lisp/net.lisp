(in-package #:net)

(defclass server ()
  ((listener :initform nil)
   (client-connection :initform nil)))

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

(defun read-message (srv)
  (with-slots (client-connection) srv
    (let ((msg (net-sys:read-message client-connection)))
      (unwind-protect
           (progn
             (log:info "Received ~a" (net-sys:c2sgreeting-get-secret msg)))
        (progn
          (net-sys:free-message msg))))))

(defun send-server-greeting (srv)
  (with-slots (client-connection) srv
    (net-sys:send-s2cgreeting-response client-connection "ACK")))
