(in-package #:mesha)

(defvar *app-state* nil)

(defun main ()
  (log:info "Mesha starting up")
  (let ((srv (net:make-server 10987)))
    (net:wait-for-client-connection srv)
    (net:read-message srv)
    (net:send-server-greeting srv)
    (net:close-client-connection srv)
    (net:shutdown-server srv)))
