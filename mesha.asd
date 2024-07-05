;;;; mesha.asd

;; add the extern folder to asdf registry before loading mesha
;; todo: what is the right way to do this?
;; (pushnew #P"/home/kiran/projects/mesha/extern/" asdf:*central-registry*)

;; add mesha directory to cffi load path 
;; (pushnew #P"/home/kiran/projects/mesha/" cffi:*foreign-library-directories*)

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
               (:file "native")
               (:file "application")
               (:file "cell")
               (:file "main"))
  :depends-on (#:alexandria
               #:serapeum
               #:3d-vectors
               #:cffi
               #:sdl2
               #:verbose
               #:trivia
               #:queues
               #:queues.simple-queue
               #:queues.simple-cqueue))
