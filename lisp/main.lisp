(in-package #:mesha)

(defclass application ()
  ((listen-socket :initform nil)
   (connection-socket :initform nil)))

(defvar *app-state* nil)

(cffi:define-foreign-library libmesha-networking
  (:unix "/home/kiran/projects/mesha/build/libmesha_networking.so")
  (t (:default "libmesha_networking")))

(cffi:use-foreign-library libmesha-networking)

(cffi:defcfun ("mesha_networking_create_server" mesha-networking-create-server) :uintptr
  (host :string)
  (port :uint16))

(cffi:defcfun ("mesha_networking_accept_connection" mesha-networking-accept-connection) :uintptr
  (listener :uintptr))

(cffi:defcfun ("mesha_networking_close_listener" mesha-networking-close-listener) :uintptr
  (listener :uintptr))

(cffi:defcfun ("mesha_networking_close_connection" mesha-networking-close-connection) :uintptr
  (connection :uintptr))

(cffi:defcfun ("mesha_networking_read_message" mesha-networking-read-message) :uintptr
  (connection :uintptr))

(cffi:defcfun ("mesha_networking_free_message" mesha-networking-free-message) :void
  (connection :uintptr))

(cffi:defcfun ("mesha_networking_get_message_type" mesha-networking-get-message-type) :uint8
  (msg :uintptr))

(cffi:defcfun ("mesha_networking_send_s2cgreeting_response" mesha-networking-send-s2cgreeting-response) :void
  (connection :uintptr)
  (secret :string))

(cffi:defcfun ("mesha_networking_c2sgreeting_get_secret" mesha-networking-c2sgreeting-get-secret) :string
  (msg :uintptr))

(defun create-server (port)
  (with-slots (listen-socket) *app-state*
    (setf listen-socket (mesha-networking-create-server "0.0.0.0" port))
    (let* ((connection (mesha-networking-accept-connection listen-socket))
           (msg (mesha-networking-read-message connection)))
      (unwind-protect
           (progn
             (log:info "Received ~a" (mesha-networking-c2sgreeting-get-secret msg))
             (mesha-networking-send-s2cgreeting-response connection "ACK"))
        (progn
          (log:info "Closing sockets")
          (mesha-networking-free-message msg)
          (mesha-networking-close-connection connection)
          (mesha-networking-close-listener listen-socket))))))

(defun main ()
  (log:info "Starting Mesha")
  (setf *app-state* (make-instance 'application))
  (create-server 10977))
