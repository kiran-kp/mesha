(in-package #:mesha)

;; <2025-03-01 Sat>
;;
;; Core of the tool is to store a tree of "content" nodes in a database and be able to
;; visualize it in a keyboard friendly UI.
;;
;; I've waffled between GUI solutions a lot and even considered switching away from Common
;; Lisp at one point. I think this was a mistake though.  I like the CL developer
;; experience a lot more than anything else I've tried.
;;
;; I've also restarted the project several times so far. The basic idea has remained the
;; same but my thoughts on how visualization and editing should work have changed. I
;; started off thinking that a spreadsheet editor like Treesheets is what I would want to
;; interact with but the implementation is lot more complicated for the feature set I want
;; and I dont' have the time to maintain something like that. It will also require a lot
;; of time spent on design which I simply do not think I will actually do.
;;
;; What I have settled on instead is a continuous document that displays the nodes as a
;; tree you can expand as necessary. My default use case is a journal so I think the top
;; level notes will be dates and children will be headings under that. I think making this
;; configurable somehow will cover a lot of the same ground that my original vision of a
;; general spreadsheet would have.
;;
;; For the GUI I am more confident in my abilities now and think I will implement an
;; immediate mode GUI myself in pure lisp. SDL3's GPU API makes this easier to do
;; cross-platform. I know this will make the overall project take longer but I think the
;; long term benefits are pretty good. Being immediate mode also means reactivity is
;; easier to implement.

(defclass node ()
  ((id :initarg :id)
   (parent :initarg :parent)
   (content :initform nil :initarg :content)))

(defvar *db* nil)

(defun id-to-str (id)
  (ctypecase id
      (null "0")
      (fuuid:uuid (fuuid:to-string id))))

(defun str-to-id (id-str)
  (if (string-equal id-str "0")
      nil
      (fuuid:from-string id-str)))

(defun create-notes-table ()
  (let* ((stmt (sxql:create-table :notes
                   ((id :type 'text :primary-key t)
                    (mparent :type 'text)
                    (mcreated :type 'integer)
                    (mtype :type 'text)
                    (mcontent :type 'blob))))
         (query (dbi:prepare *db* (sxql:yield stmt))))
    (dbi:execute query)))

(defun add-note (parent-id type content)
  (let* ((id (fuuid:make-v4))
         (parent-id-str (id-to-str parent-id))
         (stmt (sxql:insert-into :notes
                 (:id :mparent :mcreated :mtype :mcontent)
                 (list (fuuid:to-string id) parent-id-str (get-universal-time) (format nil "~a" type) content))))
    (multiple-value-bind (query vals) (sxql:yield stmt)
      (let ((query (dbi:prepare *db* query)))
        (dbi:execute query vals)))
    id))

(defun set-note (id type content)
  (let* ((id-str (id-to-str id))
         (stmt (sxql:update :notes
                 (sxql:set= :mtype (format nil "~a" type) :mcontent content)
                 (sxql:where (:= :id id-str)))))
    (multiple-value-bind (query vals) (sxql:yield stmt)
      (let ((query (dbi:prepare *db* query)))
        (dbi:execute query vals)))))

(defun save-notes ()
  (dbi:commit *db*))

(defun get-children (parent-id)
  (let* ((parent-id-str (id-to-str parent-id))
         (stmt (sxql:select :* (sxql:from :notes) (sxql:where (:= :mparent parent-id-str)))))
    (multiple-value-bind (query vals) (sxql:yield stmt)
      (let* ((query (dbi:prepare *db* query))
             (query (dbi:execute query vals)))
        (loop for row = (dbi:fetch query)
              while row
              collect (list :id (str-to-id (getf row :|id|))
                            :parent (str-to-id (getf row :|mparent|))
                            :type (getf row :|mtype|)
                            :content (getf row :|mcontent|)))))))

(defclass app ()
  ((window :initarg :window :accessor window)
   (renderer :initarg :renderer :accessor renderer)
   (state :initarg :state :accessor state)
   (last-step :initarg :last-step :accessor last-step)))

(defparameter *app* nil)

(defun assert-ret (return-value)
  (when (null return-value)
    (error "Error: ~a" (sdl3:get-error))))

(defun init ()
  (assert-ret (sdl3:set-app-metadata "Mesha" "1.0" "com.kirankp.mesha"))
  (loop for (key value) on (list sdl3:+app-url-string+  "https://www.kirankp.com/"
                                 sdl3:+app-creator-string+  "Kiran"
                                 sdl3:+app-copyright-string+  ""
                                 sdl3:+app-type-string+  "application")
          by #'cddr do
            (assert-ret (sdl3:set-app-metadata-property key value)))

  (assert-ret (sdl3:init :video))

  ;; Destroy old windows
  ;; Useful during development where old crashed windows need cleanup
  (loop for window in (sdl3:get-windows)
        for renderer = (sdl3:get-renderer window)
        unless (or (cffi:null-pointer-p window)
                   (cffi:null-pointer-p renderer))
          do (assert-ret (sdl3:destroy-renderer renderer))
             (assert-ret (sdl3:destroy-window window)))

  (multiple-value-bind (ret window renderer)
      (sdl3:create-window-and-renderer
       "Mesha"
       800
       600
       nil)
    (assert-ret ret)

    (setf *app* (make-instance 'app
                               :window window
                               :renderer renderer
                               :state nil
                               :last-step (sdl3:get-ticks))))
  :continue)

(defun handle-event (event)
  (typecase event
    (sdl3:quit-event :success)
    (sdl3:keyboard-event
     (let* ((key (slot-value event 'sdl3:%key)))
       (if (eql key :q)
           :success
           :continue)))
    (t :continue)))

(defun iterate ()
  :continue)

(defun handle-quit ()
  (sdl3:destroy-renderer (renderer *app*))
  (sdl3:destroy-window (window *app*))
  (sdl3:pump-events)
  (sdl3:quit-sub-system :video)
  (sdl3:quit))

(sdl3:def-app-init demo-init (argc argv)
  (declare (ignore argc argv))
  (init))

(sdl3:def-app-iterate demo-iterate ()
  (iterate))

(sdl3:def-app-event demo-event (type event)
  (declare (ignorable type))
  (handle-event (sdl3:event-unmarshal event)))

(sdl3:def-app-quit demo-quit (result)
  (declare (ignore result))
  (handle-quit))

(defun main ()
  (sdl3:enter-app-main-callbacks 'demo-init 'demo-iterate 'demo-event 'demo-quit))

;; (defun main ()
;;   (format t "Mesha init")
;;   (setf *db*
;;         (dbi:connect :sqlite3
;;                      :database-name (asdf:system-relative-pathname 'mesha "notes.mesha")))
;;   (init-sdl))
