(defpackage :mesha
  (:use #:cl
        #:3d-vectors)
  (:import-from :serapeum #:dict)
  (:import-from :trivia #:match)
  (:local-nicknames
   (#:v #:org.shirakumo.verbose))
  (:export #:main))
