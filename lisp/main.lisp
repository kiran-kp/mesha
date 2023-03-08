(in-package #:mesha)

(defclass application ()
  ((event-loop :initarg :event-loop)))

(defparameter *app-state* nil)

(defstruct event
  (from nil)
  (data nil))

(defclass event-loop ()
  ((events :initform nil)
   (events-lock :initform (bt:make-lock))
   (should-exitp :initform nil)))

(defun run-loop (event-loop)
  (log:info "Starting event loop")
  (with-slots (events events-lock should-exitp) event-loop
    (loop while (not should-exitp)
          do (let ((evt (bt:with-lock-held (events-lock)
                          (pop events))))
               ;; todo: actually check if event is available here
               (if evt
                   (progn
                     (log:info "Received event from: ~a" (event-from evt)))
                   (sleep 5/1000))))))

(defun add-event (event-loop evt)
  (with-slots (events events-lock) event-loop
    (bt:with-lock-held (events-lock)
      (push evt events))))

(defun network-thread (event-loop)
  (net:with-server (srv)
    (with-slots (should-exitp) event-loop
      (loop while (not should-exitp)
            do (net:with-connection (srv)
                 (let ((msg (net:read-message srv)))
                   (when (not (equal msg :none))
                     (when (equal :c2s-greeting (net:message-type msg))
                       (net:send-server-greeting-response srv)
                       (loop while (not should-exitp)
                             do (let ((msg (net:read-message srv)))
                                  (when (not (equal msg :none))
                                    (add-event event-loop msg))))))))))))

(defun main ()
  (log:info "Mesha starting up")
  (let* ((event-loop (make-instance 'event-loop))
         (app-state (make-instance 'application
                                   :event-loop event-loop)))
    (setf *app-state* app-state)
    (bt:make-thread (lambda () (network-thread event-loop)))
    (run-loop event-loop))
  (setf *app-state* nil))
