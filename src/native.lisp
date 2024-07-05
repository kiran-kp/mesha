(defpackage #:mesha-native
  (:use #:cl)
  (:export #:create-context
           #:destroy-context))

(in-package #:mesha-native)

(cffi:define-foreign-library blend2d-lib
  (:darwin (:or "libblend2d.dylib" "libblend2d"))
  (:unix (:or "libblend2d.so" "libblend2d" "blend2d"))
  (t (:default "libblend2d")))

(cffi:use-foreign-library blend2d-lib)

(cffi:define-foreign-library mesha-lib
  (:unix "libmesha.so"))

(cffi:use-foreign-library mesha-lib)

;; Returns an opaque pointer to a MeshaContext that stores global state
(cffi:defcfun ("MeshaCreateContext" create-context) :pointer)
(cffi:defcfun ("MeshaDestroyContext" destroy-context) :void
  (app :pointer))

