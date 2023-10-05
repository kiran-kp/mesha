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

(defun handle-message-received (connection msg-str)
  (log:info "Received message: ~a" msg-str)
  (let ((msg (with-input-from-string (s msg-str) (read s)))
        (client (gethash connection *clients*)))
    (ccase (getf msg :operation)
      (:get-block (send-block-update client (getf msg :id)))
      (:set-block (setf (gethash (getf msg :id) *blocks*) (getf msg :value))))))

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

(defun respond-ok (output &optional (output-type "text/html"))
  `(200
    (list :content-type ,output-type)
    (,output)))

(defun respond-404 (output &optional (output-type "text/html"))
  `(404
    (list :content-type ,output-type)
    (,output)))

(deftag nav-button (text attrs)
  `(:button
    ,@attrs
    :type "button"
    :class "text-gray-300 hover:bg-gray-700 hover:text-white rounded-md px-3 py-2 text-sm font-medium"
    ,text))

(deftag nav-button-active (text attrs)
  `(:button
     ,@attrs
     :type "button"
     :class "bg-gray-900 text-white rounded-md px-3 py-2 text-sm font-medium"
     :aria-current "page"
     ,text))

(deftag nav-bar (title attrs)
  `(:nav :class "bg-gray-800" ,@attrs
    (:div :class "mx-auto px-4 sm:px-6 px:px-8"
     (:div :class "flex h-16 items-center justify-between"
      (:div :class "flex items-center"
       (:div :class "flex-shrink-0 w-max"
        (:h1 :class "text-2xl text-white font-mono font-bold" ,title))
       (:div :class "hidden md:block"
        (:div :class "ml-10 flex items-baseline space-x-4"
         (nav-button-active "Home" nil)
         (nav-button "Draw" nil))))))))

(defparameter *main-view*
  (with-html-string
    (:doctype)
    (:html
     (:head
      (:title "Mesha")
      (:meta :charset"UTF-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:script :src "https://unpkg.com/htmx.org@1.9.6")
      (:script :src "https://cdn.tailwindcss.com"))
     (:body :class "bg-gray-300 dark:bg-gray-700"
            (:div :class "flex flex-col h-screen"
                  (nav-bar "Mesha" nil)
                  (:main :class "flex flex-grow"
                         (:div :class "mx-auto max-w-full flex flex-grow py-6 sm:px-6 lg:px-8"
                               (:div :id "content" :class "mx-auto max-w-full flex flex-grow py-6 sm:px-6 lg:px-8 bg-gray-600"
                                     (:p "Hello there")
                                     (:br)
                                     (:button :hx-post "/clicked" :hx-swap "innerHTML" "Click Me")))))))))

(defun handler (env)
  (declare (optimize (debug 3) (speed 0)))
  (let ((path (getf env :path-info)))
    (match path
      ("/" (respond-ok *main-view*))
      ("/hello" (respond-ok (with-html-string (:h1 "Test a route"))))
      ("/clicked" (respond-ok (with-html-string (:h1 "HTMX Works"))))
      (_ (respond-404 (with-html-string (:div (:h1 "Error 404")
                                              (:p "Content not found"))))))))

(defun main ()
  (log:info "Starting mesha server")
  (pushnew "hx-" *unvalidated-attribute-prefixes* :test #'equal)
  (setf *server*
        (clack:clackup (lambda (env) (funcall 'handler env))
                       :address "0.0.0.0"
                       :port 13333)))

(defun shutdown ()
  (clack:stop *server*)
  (setf *server* nil
        *current-client* nil))

(main)
