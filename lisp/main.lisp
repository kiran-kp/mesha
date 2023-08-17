(in-package #:mesha)

(defparameter *doc* (make-instance 'table
                                   :parent nil
                                   :cells (list #("Apples" 10 "Walmart")
                                                #("Oranges" 5 "Target"))
                                   :num-rows 2
                                   :num-columns 3
                                   :row-heights (list 30 30)
                                   :column-widths (list 100 50 150)))

