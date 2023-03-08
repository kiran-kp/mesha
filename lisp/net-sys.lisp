(in-package #:net-sys)

(cffi:define-foreign-library libmesha-networking
    (:unix "build/libmesha_networking.so")
  (t (:default "build/libmesha_networking")))

(cffi:use-foreign-library libmesha-networking)

(cffi:defcfun ("mesha_networking_create_server" create-server) :uintptr
  (host :string)
  (port :uint16))

(cffi:defcfun ("mesha_networking_accept_connection" accept-connection) :uintptr
  (listener :uintptr))

(cffi:defcfun ("mesha_networking_close_listener" close-listener) :uintptr
  (listener :uintptr))

(cffi:defcfun ("mesha_networking_close_connection" close-connection) :uintptr
  (connection :uintptr))

(cffi:defcfun ("mesha_networking_read_message" read-message) :uintptr
  (connection :uintptr))

(cffi:defcfun ("mesha_networking_free_message" free-message) :void
  (msg :uintptr))

(cffi:defcfun ("mesha_networking_get_message_type" get-message-type) :uint8
  (msg :uintptr))

(cffi:defcfun ("mesha_networking_send_s2cgreeting_response" send-s2cgreeting-response) :void
  (connection :uintptr)
  (secret :string))

(cffi:defcfun ("mesha_networking_c2sgreeting_get_secret" c2sgreeting-get-secret) :string
  (msg :uintptr))
