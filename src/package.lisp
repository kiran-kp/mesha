(defpackage :ui
  (:use #:cl
        #:3d-vectors)
  (:export #:with-layout
           #:make-flexbox
           #:make-rectangle-v
           #:button
           #:label
           #:init
           #:update
           #:draw))

(defpackage :mesha
  (:use #:cl
        #:3d-vectors
        #:queues)
  (:import-from :serapeum #:dict)
  (:import-from :trivia #:match)
  (:local-nicknames
   (#:v #:org.shirakumo.verbose))
  (:export #:main))
