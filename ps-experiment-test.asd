#|
  This file is a part of ps-experiment project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage ps-experiment-test-asd
  (:use :cl :asdf))
(in-package :ps-experiment-test-asd)

(defsystem ps-experiment-test
  :author "eshamster"
  :license ""
  :depends-on (:ps-experiment
               :parenscript
               :cl-js
               :prove)
  :components ((:module "t"
                :components
                ((:file "test-utils")
                 (:test-file "utils-common")
                 (:test-file "utils-func" :depends-on ("test-utils"))
                 (:test-file "utils" :depends-on ("test-utils"))
                 (:test-file "ps-experiment"))))
  :description "Test system for ps-experiment"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
