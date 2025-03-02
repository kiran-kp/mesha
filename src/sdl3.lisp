(defpackage :sdl3
  (:nicknames sdl)
  (:use #:cl
        #:3d-vectors)
  (:export #:init
           #:quit))

(in-package :sdl3)

(cffi:define-foreign-library sdl3
  (:darwin "libSDL3.dylib")
  (:unix "libSDL3.so"))

(cffi:use-foreign-library sdl3)

;; Init

(cffi:defcenum (init-flags :uint)
  (:audio    #x00000010)
  (:video    #x00000020)
  (:joystick #x00000200)
  (:haptic   #x00001000)
  (:gamepad  #x00002000)
  (:events   #x00004000)
  (:sensor   #x00008000)
  (:camera   #x00010000))

(cffi:defcfun ("SDL_Init" init) :bool
  (flags init-flags))

(cffi:defcfun ("SDL_IsMainThread" is-main-thread) :bool)

(cffi:defcfun ("SDL_Quit" quit) :void)


;; Window

(cffi:defctype window-ptr :pointer)

(cffi:defcenum (window-flags :uint64)
  (:fullscreen           #x0000000000000001)
  (:opengl               #x0000000000000002)
  (:occluded             #x0000000000000004)
  (:hidden               #x0000000000000008)
  (:borderless           #x0000000000000010)
  (:resizable            #x0000000000000020)
  (:minimized            #x0000000000000040)
  (:maximized            #x0000000000000080)
  (:mouse-grabbed        #x0000000000000100)
  (:input-focus          #x0000000000000200)
  (:mouse-focus          #x0000000000000400)
  (:external             #x0000000000000800)
  (:modal                #x0000000000001000)
  (:high-pixel-density   #x0000000000002000)
  (:mouse-capture        #x0000000000004000)
  (:mouse-relative-mode  #x0000000000008000)
  (:always-on-top        #x0000000000010000)
  (:utility              #x0000000000020000)
  (:tooltip              #x0000000000040000)
  (:popup-menu           #x0000000000080000)
  (:keyboard-grabbed     #x0000000000100000)
  (:vulkan               #x0000000010000000)
  (:metal                #x0000000020000000)
  (:transparent          #x0000000040000000)
  (:not-focusable        #x0000000080000000))

(cffi:defcfun ("SDL_CreateWindow" create-window) window-ptr
  (title :string)
  (width :int)
  (height :int)
  (flags window-flags))

(cffi:defcenum flash-operation
  :cancel
  :briefly
  :until-focused)

(cffi:defcfun ("SDL_FlashWindow" flash-window) :bool
  (window window-ptr)
  (operation flash-operation))
