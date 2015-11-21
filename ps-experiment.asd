#|
  This file is a part of ps-experiment project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

#|
  Author: eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage ps-experiment-asd
  (:use :cl :asdf))
(in-package :ps-experiment-asd)

(defsystem ps-experiment
  :version "0.1"
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:parenscript
               :alexandria
               :anaphora)
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils-common")
                 (:file "package")
                 (:file "defines")
                 (:file "utils")
                 (:file "ps-experiment"))))
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
  :in-order-to ((test-op (test-op ps-experiment-test))))
