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

(defparameter *step-rate-in-ms* 125)
(defparameter *snake-block-size-in-pixels* 48)
(defparameter *snake-game-width* 10)
(defparameter *snake-game-height* 10)

(deftype cell-value ()
  `(member :empty :food :left :right :up :down))

(defclass game-state ()
  ((cells :accessor cells)
   (head-pos :accessor head-pos)
   (tail-pos :accessor tail-pos)
   (next-dir :accessor next-dir)
   (occupied-cells :initform 0 :accessor occupied-cells)))

(defun cell (cells pos)
  (aref cells (first pos) (second pos)))

(defun (setf cell) (newval cells pos)
  (setf (aref cells (first pos) (second pos)) newval))


(defun place-food (state)
  (with-slots (occupied-cells cells) state
    (unless (= occupied-cells (* *snake-game-height* *snake-game-width*))
      (loop for x = (random *snake-game-width*)
            for y = (random *snake-game-height*)
            when (eql (aref cells x y) :empty)
              do (return (setf (aref cells x y) :food))))))

(defun initialize-game-state (state)
  (with-slots (cells head-pos tail-pos next-dir occupied-cells) state
    (let* ((mid-x (floor *snake-game-width* 2))
           (mid-y (floor *snake-game-height* 2)))
      (setf head-pos (list mid-x mid-y)
            tail-pos (list (- mid-x 3) mid-y)
            next-dir :right
            cells (make-array (list *snake-game-width* *snake-game-height*)
                              :initial-element :empty
                              :element-type 'cell-value)
            occupied-cells 4)
      (loop for x from mid-x downto (- mid-x 3) do
        (setf (aref cells x mid-y) :right))
      (loop repeat 4 do (place-food state)))
    state))

(defun next-position (pos dir)
  (destructuring-bind (x y) pos
    (ecase dir
      (:right (values (mod (+ x 1) *snake-game-width*) y))
      (:left (values (mod (- x 1) *snake-game-width*) y))
      (:up (values x (mod (- y 1) *snake-game-height*)))
      (:down (values x (mod (+ y 1) *snake-game-height*))))))

(defun opposite-dir (dir)
  (ecase dir
    (:up :down)
    (:down :up)
    (:left :right)
    (:right :left)))

(defun game-step (state)
  (declare (optimize (debug 3)))
  (with-slots (cells head-pos tail-pos next-dir occupied-cells) state
    (when (eql (cell cells head-pos) (opposite-dir next-dir))
      (setf next-dir (cell cells head-pos)))
    (multiple-value-bind (x y) (next-position head-pos next-dir)
      (flet ((move-head ()
               (setf (aref cells (first head-pos) (second head-pos)) next-dir)
               (setf (first head-pos) x
                     (second head-pos) y)
               (setf (aref cells x y) next-dir))

             (move-tail ()
               (let ((tail-dir (cell cells tail-pos)))
                 (setf (cell cells tail-pos) :empty)
                 (multiple-value-bind (x y)
                     (next-position tail-pos tail-dir)
                   (setf (first tail-pos) x
                         (second tail-pos) y)))))

        (case (aref cells x y)
          (:empty
           (move-head)
           (move-tail))
          (:food
           (move-head)
           (place-food state)
           (incf occupied-cells))
          (t ;; collision with self
           (initialize-game-state state)))))))

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
  (assert-ret (sdl3:set-app-metadata "Example Snake game" "1.0" "com.example.Snake"))
  (loop for (key value) on (list sdl3:+app-url-string+  "https://examples.libsdl.org/SDL3/demo/01-snake/"
                                 sdl3:+app-creator-string+  "SDL team"
                                 sdl3:+app-copyright-string+  "Placed in the public domain"
                                 sdl3:+app-type-string+  "game")
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
       "test/demo/snake"
       (* *snake-block-size-in-pixels* *snake-game-width*)
       (* *snake-block-size-in-pixels* *snake-game-height*)
       nil)
    (assert-ret ret)

    (setf *app* (make-instance 'app
                               :window window
                               :renderer renderer
                               :state (initialize-game-state (make-instance 'game-state))
                               :last-step (sdl3:get-ticks))))
  :continue)

(defun handle-event (event)
  (typecase event
    (sdl3:quit-event :success)
    (sdl3:keyboard-event
     (let* ((key (slot-value event 'sdl3:%key))
            (scancode (slot-value event 'sdl3:%scancode)))
       (when (member key '(:up :down :left :right))
         (setf (next-dir (state *app*)) key))
       (if (eql key :q)
           :success
           :continue)))
    (t :continue)))

(defun iterate ()
  (let ((now (sdl3:get-ticks))
        (rect (make-instance 'sdl3:frect
                             :%h (coerce *snake-block-size-in-pixels* 'float)
                             :%w (coerce *snake-block-size-in-pixels* 'float))))
    (with-slots (renderer last-step state) *app*
      ;; Run physics updates
      (loop while (> (- now last-step)
                     *step-rate-in-ms*)
            do
               (incf last-step *step-rate-in-ms*)
               (game-step state))

      ;; Sleep until UI update time
      (when (< (- now last-step) *step-rate-in-ms*)
        (sleep (/ (- now last-step) 1000 5)))

      ;; Render game board
      (sdl3:set-render-draw-color renderer 0 0 0 255)
      (sdl3:render-clear renderer)

      (flet ((fill-rect (x y r g b)
               (sdl3:set-render-draw-color renderer r g b 255)
               (setf (slot-value rect 'sdl3:%x) (* 1.0 x *snake-block-size-in-pixels*)
                     (slot-value rect 'sdl3:%y) (* 1.0 y *snake-block-size-in-pixels*))
               (sdl3:render-fill-rect renderer rect)))
        (loop for x from 0 below *snake-game-width* do
          (loop for y from 0 below *snake-game-height*
                for cell = (aref (cells state) x y) do
                  (case cell
                    (:empty
                     t)
                    (:food
                     (fill-rect x y 80 80 255))
                    (t
                     (fill-rect x y 0 128 0)))))

        (destructuring-bind (x y) (head-pos state)
          (fill-rect x y 255 255 0))
        (sdl3:render-present renderer)))
    :continue))

(defun handle-quit ()
  (sdl3:destroy-renderer (renderer *app*))
  (sdl3:destroy-window (window *app*))
  (sdl3:pump-events)
  (sdl3:quit-sub-system :video)
  (sdl3:quit))

;;;;; Example of how to run app by writing you own event and ui loop
(defun run-snake% ()
  (init)
  (unwind-protect
       (loop named outer do
         (loop named event-loop
               for event = (sdl3:poll-event*)
             while event
             do
                (let* ((next (handle-event event)))
                  (unless (eql next :continue)
                    (return-from outer))))
         (iterate))
    (handle-quit)))

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
