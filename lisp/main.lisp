(in-package #:mesha)

(defclass application ()
  ((listen-socket :initform nil)))

(defvar *app-state* nil)

(defun create-server (port)
  (with-slots (listen-socket) *app-state*
    (setf listen-socket (usocket:socket-listen "0.0.0.0" port :reuse-address t))
    (let ((connection (usocket:socket-accept listen-socket :element-type 'character)))
      (unwind-protect
           (progn
             (usocket:wait-for-input connection)
             (log:info "Connected. Waiting for message")
             (format t "Received: ~a~%" (read-line (usocket:socket-stream connection)))
             (log:info "Sending data")
             (format (usocket:socket-stream connection) "Hello World~%")
             (force-output (usocket:socket-stream connection))
             (log:info "Sent data"))
        (progn
          (log:info "Closing sockets")
          (usocket:socket-close connection)
          (usocket:socket-close listen-socket))))))

(defun main ()
  (log:info "Starting Mesha")
  (setf *app-state* (make-instance 'application))
  (create-server 10977))
