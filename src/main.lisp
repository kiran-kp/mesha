(in-package #:mesha)

(defclass document ()
  ((filepath :initarg :filepath)
   (connection :initform nil)))

(defclass client ()
  ((connection :initarg :connection)))

(defvar *clients* (make-hash-table))
(defvar *server* nil)
(defvar *websocket-server* nil)

(defvar *current-client* nil)

(defparameter *blocks* (alexandria:plist-hash-table '(1 "Test"
                                                      2 123
                                                      3 "A longer line of text"
                                                      4 "https://www.google.com/"
                                                      5 (1 2 3 4))))

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
    (ccase (getf msg :operation)
      (:get-block (send-block-update client (getf msg :id)))
      (:set-block (setf (gethash (getf msg :id) *blocks*) (getf msg :value))))))

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
                   `(:operation "update-block"
                     :id ,block-id
                     :value ,(format nil "~a" val)))))
    (send-message client (yason:with-output-to-string* () (yason:encode message)))))

(defun button (text &rest attrs)
  (with-html
    (:button :type "button"
             :class "text-white bg-blue-950 shadow-lg hover:bg-blue-700 hover:text-white rounded-md px-3 py-2 text-sm font-bold"
             :attrs attrs
             text)))

(defun get-mesha-script ()
  (parenscript:ps
    (defvar *rich-text-editor* nil)
    (defvar *websocket* nil)

    (defun handle-server-message (msg-event)
      (let* ((obj (parenscript:chain +JSON+ (parse (parenscript:@ msg-event data))))
             (op (parenscript:@ obj operation))
             (val (parenscript:@ obj value)))
        (parenscript:case op
          ("update-block" (when (parenscript:stringp val)
                            (parenscript:chain *rich-text-editor* (set-text val))))))
      nil)
    
    (defun init-mesha-client ()
      ;; (parenscript:chain -Mousetrap (bind "ctrl+enter" (lambda () (parenscript:chain *rich-text-editor* (focus)))))
      (setf *websocket* (parenscript:new (-Web-Socket "ws://localhost:13330")))
      (setf (parenscript:@ *websocket* onopen) (lambda (open-event)
                                                 (setf (parenscript:@ *websocket* onmessage) #'handle-server-message)
                                                 nil))
      (when (equal *rich-text-editor* nil)
        (setf *rich-text-editor* (parenscript:new (-Quill "#editor-container" (parenscript:create theme "bubble")))))
      nil)))

(defun get-nav-bar ()
  (flet ((button (text &rest attrs)
           (with-html (:button :type "button"
                               :class "text-gray-300 hover:bg-gray-700 hover:text-white rounded-md px-3 py-2 text-sm font-medium"
                               :attrs attrs
                               text)))
         (active-button (text &rest attrs)
           (with-html (:button :type "button"
                               :class "bg-gray-900 text-white rounded-md px-3 py-2 text-sm font-medium"
                               :attrs attrs
                               text))))
    (with-html
      (:nav :class "bg-gray-800"
            (:div :class "mx-auto px-4 sm:px-6 px:px-8"
                  (:div :class "flex h-16 items-center justify-between"
                        (:div :class "flex items-center"
                              (:div :class "flex-shrink-0 w-max"
                                    (:h1 :class "text-2xl text-white font-mono font-bold" "Mesha"))
                              (:div :class "hidden md:block"
                                    (:div :class "ml-10 flex items-baseline space-x-4"
                                          (active-button "Home")
                                          (button "Draw"))))))))))

(defun get-main-view ()
  (with-html-string
    (:doctype)
    (:html
     (:head
      (:title "Mesha")
      (:meta :charset"UTF-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:link :href "https://cdn.quilljs.com/1.3.6/quill.bubble.css" :rel "stylesheet")
      (:script :src "https://unpkg.com/htmx.org@1.9.6")
      (:script :src "https://unpkg.com/alpinejs" :defer "")
      (:script :src "https://cdn.tailwindcss.com")
      (:script :src "https://cdn.quilljs.com/1.3.6/quill.js")
      (:script :src "https://cdnjs.cloudflare.com/ajax/libs/mousetrap/1.6.5/mousetrap.min.js")
      (:script
       (:raw (get-mesha-script))))
     (:body :class "bg-gray-300 dark:bg-gray-700"
            (:div :class "flex flex-col h-screen"
                  (get-nav-bar)
                  (:main :class "h-full"
                         (:div :class "w-full h-full grow flex px-6 py-6"
                               (:div :id "content" :class "bg-gray-600 w-full px-6 py-6"
                                     (button "Click me" :hx-post "/clicked" :hx-swap "innerHTML")
                                     (:div :class "rounded-xl bg-gray-200 shadow-lg p-5"
                                           (:div :id "editor-container" :x-init (parenscript:ps (init-mesha-client)) :class "max-h-full"))))))))))

(defparameter *button-state* nil)

(defun handler (env)
  (declare (optimize (debug 3) (speed 0)))
  (flet ((respond-ok (output &optional (output-type "text/html"))
           `(200
             (list :content-type ,output-type)
             (,output)))
         (respond-404 (output &optional (output-type "text/html"))
           `(404
             (list :content-type ,output-type)
             (,output))))
    (let ((path (getf env :path-info)))
      (match path
        ("/" (respond-ok (get-main-view)))
        ("/clicked" (progn
                      (setf *button-state* (not *button-state*))
                      (if *button-state*
                          (respond-ok (with-html-string (:div :x-init (parenscript:ps (parenscript:chain *websocket* (send "(:operation :get-block :id 1)")))
                                                              "Get block 1")))
                          (respond-ok (with-html-string (:div :x-init (parenscript:ps (parenscript:chain *websocket* (send "(:operation :get-block :id 2)")))
                                                              "Get block 2"))))))
        (_ (respond-404 (with-html-string (:div (:h1 "Error 404")
                                                (:p "Content not found")))))))))

(defun main ()
  (log:info "Starting mesha server")
  
  (setf yason:*symbol-encoder* #'yason:encode-symbol-as-lowercase
        yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase)

  ;; ignore htmx attributesp
  (pushnew "hx-" *unvalidated-attribute-prefixes* :test #'equal)
  ;; ignore alpine.js attributes
  (pushnew "x-" *unvalidated-attribute-prefixes* :test #'equal)
  (pushnew "@" *unvalidated-attribute-prefixes* :test #'equal)

  (setf *server* (clack:clackup (lambda (env) (funcall 'handler env))
                                ;; :address "0.0.0.0"
                                :port 13333)
        *websocket-server* (clack:clackup (lambda (env) (funcall 'setup-websocket env))
                                          ;; :address "0.0.0.0"
                                          :port 13330)))

(defun shutdown ()
  (clack:stop *server*)
  (setf *server* nil
        *current-client* nil))

(main)
