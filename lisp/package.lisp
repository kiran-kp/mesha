(defpackage :net-sys
  (:use #:cl)
  (:export #:create-server
           #:accept-connection
           #:close-listener
           #:close-connection
           #:read-message
           #:free-message
           #:get-message-type
           #:send-s2cgreeting-response
           #:c2sgreeting-get-secret))

(defpackage :net
  (:use #:cl)
  (:export #:make-server
           #:shutdown-server
           #:wait-for-client-connection
           #:close-client-connection
           #:read-message
           #:send-server-greeting))

(defpackage :mesha
  (:use #:cl)
  (:export #:main))
