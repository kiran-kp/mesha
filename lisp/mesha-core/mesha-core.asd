;;;; mesha-core.asd

(asdf:defsystem #:mesha-core
  :description "Lisp bootstrap code for Mesha and C++ interface."
  :author "Kiran <projects@kirankp.com>"
  :license  ""
  :version "0.0.1"
  :serial t
  :build-operation monolithic-dll-op 
  :components ((:file "package")
               (:file "main"))
  :depends-on (#:asdf))
