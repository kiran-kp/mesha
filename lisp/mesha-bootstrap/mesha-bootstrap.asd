;;;; mesha-bootstrap.asd

(asdf:defsystem #:mesha-bootstrap
  :description "Lisp bootstrap code for Mesha"
  :author "Kiran <projects@kirankp.com>"
  :license  ""
  :version "0.0.1"
  :serial t
  :build-operation monolithic-dll-op 
  :components ((:file "package")
               (:file "main"))
  :depends-on (#:asdf))
