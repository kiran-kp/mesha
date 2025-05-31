;;;; mesha.asd

;; elisp code to start slime version specific to this project
;; (add-to-list 'load-path (expand-file-name "~/projects/mesha/systems/sly-20250207-c48defcf"))
;; (load-file (expand-file-name "~/projects/mesha/systems/sly-20250207-c48defcf/sly.el"))

;; (asdf:load-system :mesha)

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
  :depends-on (#:alexandria
               #:serapeum
               #:cffi
               #:3d-vectors
               #:bordeaux-threads
               #:str
               #:trivia
               #:cl-dbi
               #:frugal-uuid
               #:sxql
               #:trivial-main-thread
               #:slynk
               #:sdl3))
