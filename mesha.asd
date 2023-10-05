;;;; mesha.asd

;; Use the loader.lisp to initialize the repl

(asdf:defsystem #:mesha
  :description "A productivity tool"
  :author "Kiran <projects@kirankp.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :build-operation "program-op"
  :build-pathname "../mesha"
  :entry-point "mesha:main"
  :components ((:file "package")
               (:file "main"))
  :depends-on (#:cffi
               #:log4cl
               #:str
               #:clack
               #:websocket-driver
               #:alexandria
               #:mito
               #:spinneret
               #:trivia
               #:yason))
