;;;; mesha.asd

(asdf:defsystem #:mesha
  :description "A productivity tool"
  :author "Kiran <projects@kirankp.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "lisp/"
  :build-operation "program-op"
  :build-pathname "../mesha"
  :entry-point "mesha:main"
  :components ((:file "package")
               (:file "main"))
  :depends-on (#:asdf
               #:bordeaux-threads
               #:cffi
               #:log4cl))
