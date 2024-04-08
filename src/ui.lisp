(in-package #:ui)

(defparameter *current-msgs* '())

(defvar *model* nil)
(defvar *update-fn* nil)
(defvar *draw-fn* nil)

;; Raylib wrappers

(defun make-rectangle-v (pos size)
  (raylib:make-rectangle :x (vx pos) :y (vy pos) :width (vx size) :height (vy size)))

(defun is-point-in-rect? (point rect)
  (raylib:check-collision-point-rec point rect))

(defun is-point-in-rect-v? (point rect-position rect-size)
  (is-point-in-rect? point (make-rectangle-v rect-position rect-size)))

(defun measure-text (text)
  (let* ((text-height (get-style-value :text-height)))
    (raylib:measure-text-ex *default-font* text text-height (get-style-value :text-spacing))))

(defun render-text (text position size color)
  (raylib:draw-text-ex *default-font*
                       text
                       position
                       (get-style-value :text-height)
                       (get-style-value :text-spacing)
                       (get-style-value color)))

(defun render-rect (position size color)
  (let ((rect (make-rectangle-v position size)))
    (raylib:draw-rectangle-rec rect (get-style-value color))))

(defun render-frame (position size color draw-border?)
  (let ((rect (make-rectangle-v position size)))
    (raylib:draw-rectangle-rec rect (get-style-value color))
    (when draw-border?
      (raylib:draw-rectangle-lines-ex rect 1.0 '(10 10 10 255)))))

;; Input

(defclass input ()  
  ((mouse-button-left :initform :up)
   (mouse-button-right :initform :up)
   (mouse-button-middle :initform :up)
   (mouse-position :initform (vec 0 0)))
  (:documentation "Valid states are (:up :pressed :down :released). (:pressed :released) typically only last a single frame."))
(export 'input)
(export 'mouse-position)
(export 'mouse-button-left)

(defvar *input* nil)
(export '*input*)

(defun update-input ()
  (flet ((get-mouse-button-state (button)
           (cond
             ((raylib:is-mouse-button-pressed button) :pressed)
             ((raylib:is-mouse-button-down button) :down)
             ((raylib:is-mouse-button-released button) :released)
             ((raylib:is-mouse-button-up button) :up)
             (t :unknown))))
    (with-slots (mouse-button-left mouse-button-right mouse-button-middle mouse-position) *input*
      (setf mouse-button-left (get-mouse-button-state :mouse-button-left)
            mouse-button-right (get-mouse-button-state :mouse-button-right)
            mouse-button-middle (get-mouse-button-state :mouse-button-middle)
            mouse-position (raylib:get-mouse-position)))))

(defun get-mouse-button-state (button)
  (with-slots (mouse-button-left mouse-button-right mouse-button-middle) *input*
    (case button
      (:left mouse-button-left)
      (:right mouse-button-right)
      (:middle mouse-button-middle))))
(export 'get-mouse-button-state)

(defun get-mouse-position ()
  (with-slots (mouse-position) *input*
    mouse-position))

(defun is-mouse-over-rect? (rect)
  (with-slots (mouse-position) *input*
    (is-point-in-rect? mouse-position rect)))

(defun is-mouse-over-rect-v? (pos size)
  (is-mouse-over-rect? (make-rectangle-v pos size)))

;; Style

;; https://tailwindcolor.com/
(defparameter +red+ (raylib:make-rgba 127 29 29 255))
(defparameter +orange+ (raylib:make-rgba 124 45 18 255))
(defparameter +amber+ (raylib:make-rgba 120 53 15 255))
(defparameter +yellow+ (raylib:make-rgba 113 63 18 255))
(defparameter +lime+ (raylib:make-rgba 54 83 20 255))
(defparameter +green+ (raylib:make-rgba 20 83 45 255))
(defparameter +emerald+ (raylib:make-rgba 6 78 59 255))
(defparameter +teal+ (raylib:make-rgba 19 78 74 255))
(defparameter +cyan+ (raylib:make-rgba 22 78 99 255))
(defparameter +light-blue+ (raylib:make-rgba 12 74 110 255))
(defparameter +blue+ (raylib:make-rgba 30 58 138 255))
(defparameter +indigo+ (raylib:make-rgba 49 46 129 255))
(defparameter +violet+ (raylib:make-rgba 76 29 149 255))
(defparameter +purple+ (raylib:make-rgba 88 28 135 255))
(defparameter +fuchsia+ (raylib:make-rgba 112 26 117 255))
(defparameter +pink+ (raylib:make-rgba 131 24 67 255))
(defparameter +rose+ (raylib:make-rgba 131 24 67 255))
(defparameter +warm-gray+ (raylib:make-rgba 28 25 23 255))
(defparameter +true-gray-0+ (raylib:make-rgba 23 23 23 255))
(defparameter +true-gray-1+ (raylib:make-rgba 250 250 250 255))
(defparameter +gray+ (raylib:make-rgba 24 24 27 255))
(defparameter +cool-gray+ (raylib:make-rgba 17 24 39 255))
(defparameter +blue-gray+ (raylib:make-rgba 15 23 42 255))
(defparameter +blue-gray+ (raylib:make-rgba 15 23 42 255))
(defparameter +white+ (raylib:make-rgba 255 255 255 255))

(defparameter *default-style*
  (alexandria:plist-hash-table
   `(:window-title-bar-height 16.0
     :window-title-bar-active-color ,+light-blue+
     :window-title-bar-inactive-color ,+blue-gray+
     :window-bg-color (23 23 23 210) ;; +true-gray-0+ with transparency

     :text-height 16.0
     :text-spacing 1.0
     :text-color ,+true-gray-1+

     :button-padding 10
     :button-color ,+light-blue+
     :button-pressed-color ,+blue+
     :button-hover-color ,+light-blue+

     :label-padding 10)))

(defparameter *temp-style* (make-hash-table))

(defun get-style-value (key)
  (if (keywordp key)
      (or (gethash key *temp-style*)
          (gethash key *default-style*))
      key))

(defvar *default-font* nil)

;; Containers
(defvar *container-hierarchy* nil)

(defgeneric get-id (container))
(defgeneric add-widget (container width height))
(defgeneric get-draw-list (container))
(defgeneric add-to-draw-list (container cmd))

(defun add-rect (position size color)
  (add-to-draw-list (first *container-hierarchy*) `(:command :rect :position ,position :size ,size :color ,color)))

(defun add-frame (position size color draw-border?)
  (add-rect position size color)
  (when draw-border?
    (add-rect position size color)))

(defun add-text (text position size color)
  (add-to-draw-list (first *container-hierarchy*) `(:command :text :position ,position :size ,size :color ,color :text ,text)))

(defclass window ()
  ((name :initarg :name)
   (id :initarg :id)
   (position :initform (vec 100 100))
   (size :initform (vec 600 300))
   (mouse-offset :initform (vec 0 0))
   (is-being-dragged? :initform nil)
   (transient-data :initform nil)))

(defclass window-transient-data ()
  ((cursor :initform (vec 0 (get-style-value :window-title-bar-height)))
   (current-line-height :initform 0)
   (previous-line-height :initform 0)
   (previous-line-cursor :initform (vec 0 0))
   (draw-list :initform nil)))

(defvar *windows* nil)

(defun is-window-active? (wnd)
  (let ((active-id (slot-value (first *windows*) 'id))
        (window-id (slot-value wnd 'id)))
    (equal active-id window-id)))

(defun set-active-window (wnd)
  (setf *windows* (delete wnd *windows*))
  (push wnd *windows*))

(defun create-id-in-current-hierarchy (name)
  (let ((parents (mapcar #'get-id *container-hierarchy*)))
    (push (sxhash name) parents)
    (sxhash parents)))

(defun find-or-create-window (name)
  (let ((id (sxhash name)))
    (flet ((create-window ()
             (let ((wnd (make-instance 'window :name name
                                               :id id)))
               (log:info "Creating window: ~A" name)
               (set-active-window wnd)
               wnd)))
      (or (first (member-if (lambda (x) (equal id (slot-value x 'id))) *windows*))
          (create-window)))))

(defun get-first-window-at-point (point)
  (loop for wnd in *windows*
        do (with-slots (position size) wnd
             (when (is-point-in-rect-v? point position size)
               (return wnd)))))

(defun render-window-and-decorations (wnd)
  (with-slots (name id position size mouse-offset is-being-dragged?) wnd
    (let ((click-state (get-mouse-button-state :left))
          (mouse-position (get-mouse-position)))
      (when (is-mouse-over-rect-v? position size)
        (case click-state
          (:pressed
           (when (equal wnd (get-first-window-at-point mouse-position))
             (set-active-window wnd)
             (setf mouse-offset (v- mouse-position position)
                   is-being-dragged? t)))))
      (when is-being-dragged?
        (case click-state
          (:down
           (when (is-window-active? wnd)
             (setf position (v- mouse-position mouse-offset))))
          (:released
           (setf is-being-dragged? nil)))))
    
    (let ((title-text-size (measure-text name))
          (title-bar-color (if (is-window-active? wnd)
                               :window-title-bar-active-color
                               :window-title-bar-inactive-color))
          (title-bar-offset (vec 0 (get-style-value :window-title-bar-height))))
      (add-frame position (vec (vx size) (vy title-text-size)) title-bar-color t)
      (add-text name (v+ position (vec 5 0)) title-text-size :text-color)
      (add-frame (v+ position title-bar-offset)
                 (v- size title-bar-offset)
                 :window-bg-color
                 t))))

(defun push-window (name)
  (let ((wnd (find-or-create-window name)))
    (push wnd *container-hierarchy*)
    (with-slots (position size transient-data) wnd
      (setf transient-data (make-instance 'window-transient-data))
      (render-window-and-decorations wnd))))
(export 'push-window)

(defun pop-window ()
  (pop *container-hierarchy*))
(export 'pop-window)

(defmethod get-id ((container window))
  (slot-value container 'id))

(defmethod get-draw-list ((container window))
  (let ((transient-data (slot-value container 'transient-data)))
    (when transient-data
      (slot-value transient-data 'draw-list))))

(defmethod add-to-draw-list ((container window) cmd)
  (with-slots (transient-data) container
    (with-slots (draw-list) transient-data
      (push cmd draw-list))))

(defmethod add-widget ((container window) width height)
  (with-slots (transient-data position size) container
    (with-slots (cursor current-line-height previous-line-cursor previous-line-height) transient-data
      (let ((current-pos cursor))
        (nv+ cursor (vec width 0))
        (setf current-line-height (max current-line-height height))        
        (setf previous-line-cursor cursor
              previous-line-height current-line-height)
        (nv+ cursor (vec 0 current-line-height))
        (setf (vx cursor) 0
              current-line-height 0)
        (v+ current-pos position)))))

(defun add-widget-to-current-container (size)
  (let ((container (first *container-hierarchy*)))
    (add-widget container (vx size) (vy size))))

(defun button (text on-click &optional (on-held nil on-held-supplied-p))
  (let* ((mouse-button-left (get-mouse-button-state :left))
         (is-active? (equal (first *container-hierarchy*) (first *windows*)))
         (is-pressed? (and is-active? (equal mouse-button-left :pressed)))
         (is-held? (and is-active? (equal mouse-button-left :down))))
    (let* ((text-size (measure-text text))
           (button-padding (get-style-value :button-padding))
           (widget-size (v+ text-size (vec button-padding 0)))
           (pos (add-widget-to-current-container widget-size))
           (is-hovering? (and is-active? (is-mouse-over-rect-v? pos widget-size)))
           (color (if is-hovering?
                      (if (or is-held? is-pressed?)
                          :button-pressed-color
                          :button-hover-color)
                      :button-color)))
      (add-frame pos widget-size color t)
      (add-text text (v+ pos (vec (/ button-padding 2) 0)) text-size :text-color)
      (when (and on-held-supplied-p is-hovering? is-held?)
        (break)
        (add-message on-held))
      (when (and is-hovering? is-pressed?)
        (add-message on-click)))))
(export 'button)

(defun label (text)
  (let* ((text-size  (measure-text text))
         (label-padding (get-style-value :label-padding))
         (widget-size (v+ text-size (vec label-padding 0)))
         (pos (add-widget-to-current-container widget-size)))
    (add-text text (v+ pos (vec (/ label-padding 2) 0)) text-size :text-color)))
(export 'label)

;; Engine interface

(defun init (model update-fn draw-fn)
  (setf *model* model
        *update-fn* update-fn
        *draw-fn* draw-fn
        *input* (make-instance 'input)
        *default-font* (raylib:load-font-ex "assets/fonts/Inter-V.ttf" 16 (cffi:null-pointer) 250)))
(export 'init)

(defun shutdown ()
  (raylib:unload-font *default-font*)
  (setf *model* nil
        *update-fn* nil
        *draw-fn* nil
        *container-hierarchy* nil
        *input* nil
        *default-font* nil
        *windows* nil))
(export 'shutdown)

(defun update ()
  (update-input)
  (dolist (msg *current-msgs*)
    (funcall *update-fn* msg *model*)))
(export 'update)

(defun draw ()
  (setf *current-msgs* nil)
  (funcall *draw-fn* *model*)
  (setf *current-msgs* (reverse *current-msgs*))
  (dolist (wnd (reverse *windows*))
    (dolist (command-info (reverse (get-draw-list wnd)))
      (let ((cmd (getf command-info :command)))
        (case cmd
          (:rect
           (render-rect (getf command-info :position)
                        (getf command-info :size)
                        (getf command-info :color)))
          (:text
           (render-text (getf command-info :text)
                        (getf command-info :position)
                        (getf command-info :size)
                        (getf command-info :color))))))
    (with-slots (transient-data) wnd
      (setf transient-data nil))))
(export 'draw)

(defun add-message (msg)
  (push msg *current-msgs*))
