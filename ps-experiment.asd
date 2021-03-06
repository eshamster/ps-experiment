#|
  This file is a part of ps-experiment project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

#|
  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem ps-experiment
  :version "0.1"
  :author "eshamster"
  :license "LLGPL"
  :class :package-inferred-system
  :depends-on (:parenscript
               :metabang-bind
               :alexandria
               :anaphora
               "ps-experiment/main")
  :description "This is the experimental utils for parenscript"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op "ps-experiment/t"))))

(defsystem ps-experiment/t
  :class :package-inferred-system
  :depends-on (:ps-experiment
               :parenscript
               :alexandria
               :anaphora
               :cl-js
               :rove
               "ps-experiment/t/base"
               "ps-experiment/t/util-sorter"
               "ps-experiment/t/package"
               "ps-experiment/t/defines"
               "ps-experiment/t/defines/defmethod"
               "ps-experiment/t/ps-macros-for-compatibility"
               "ps-experiment/t/common-macros"
               )
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
