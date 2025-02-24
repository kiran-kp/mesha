(in-package #:mesha)

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

(defun main ()
  (format t "Mesha init")
  (setf *db* (dbi:connect :sqlite3 :database-name (asdf:system-relative-pathname 'mesha "notes.mesha"))))
