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

(defclass ring-buffer ()
  ((items :initarg :items)
   (start :initform 0)
   (end :initform 0))
  (:documentation "ring-buffer can hold at most (- (length items) 1) items
The slot at `end` is not considered valid and we set it to nil
when `end` is updated in ring-buffer-push"))

(defun make-ring-buffer (size)
  (make 'ring-buffer :items (make-array size :initial-element nil)))

(defun ring-buffer-length (rb)
  (declare (optimize (speed 0) (debug 3)))
  (with-slots (items start end) rb
    (let ((n (length items)))
      (if (>= end start)
          (- end start)
          (+ end (- n start))))))

(defun ring-buffer--increment (x size)
  (mod (+ x 1) size))

(defun ring-buffer-push (rb val)
  (with-slots (items start end) rb
    (setf (elt items end) val
          end (ring-buffer--increment end (length items))
          start (if (= start end) (ring-buffer--increment start (length items)) start)
          (elt items end) nil)))

(defun ring-buffer-pop-first (rb)
  (with-slots (items start end) rb
    (when (> (ring-buffer-length rb) 0)
      (let ((val (elt items start)))
        (setf (elt items start) nil
              start (ring-buffer--increment start (length items)))
        val))))

(defclass node ()
  ((id :initarg :id)
   (parent :initarg :parent)
   (content :initform nil :initarg :content)))

(defparameter *rb* (make-ring-buffer 20))

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

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(defun main ()
  (format t "Mesha init")
  (setf *db*
        (dbi:connect :sqlite3
                     :database-name (asdf:system-relative-pathname 'mesha "notes.mesha")))
  (tmt:with-body-in-main-thread (:blocking t)
    (format t "Main thread: ~a" (sdl::is-main-thread))
    (sdl3:init :video)
    (let ((window (sdl::create-window "Test" 800 600 :high-pixel-density)))
      (sdl::flash-window window :until-focused))))
