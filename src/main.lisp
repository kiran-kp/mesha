(in-package #:mesha)

(defvar *font* nil)

(defparameter *start-pos* (vec 200.0 50.0))
(defparameter *row-size* 100.0)

(defun update ())

(defun render ())

#+nil
(list
 ;; Text label at origin
 (widget
  (text "Hello world!"))

 ;; Text label at position
 (screen
  (at (vec 100 120)
      (text "Hello world!")))

  ;; Styled text label
 (widget
  (text (bold "This " (italic "is ")) "some " (color :red "styled ") (underline "text.")))

 ;; Draw some lines
 (screen
  (color :gray
         (thickness 10
                    (lines (list (vec 0 10) (vec 1080 10)
                                 (vec 0 20) (vec 1080 20)
                                 (vec 0 30) (vec 1080 30))))))

 (widget
  (button "Start Local Game" :on-click '(start-game :local))))

(cffi:define-foreign-library libmesha
  (:unix "libmesha.so"))

(cffi:use-foreign-library libmesha)

(cffi:defcfun "mesha_initialize_gui" :void)

(defvar *should-quit* nil)
(setf *should-quit* t)

(defun main-loop ()
  (update))

(defun main ()
  (setf *should-quit* nil)
  (v:info :application "Scribble starting up: ~a"
          (trivial-main-thread:with-body-in-main-thread (:blocking t)
            (+ 1 1)))
  (let ((thread (bt:make-thread (lambda ()
                                  (loop while (not *should-quit*)
                                        do (main-loop))
                                  (v:info :application "Exiting")))))
    (trivial-main-thread:with-body-in-main-thread (:blocking nil)
      (mesha-initialize-gui)
      (v:info :application "Closed GUI")
      (setf *should-quit* t))

    (v:info :application "Waiting for gui to close")
    (bt:join-thread thread)))
