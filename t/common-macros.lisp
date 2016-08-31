(in-package :cl-user)
(defpackage ps-experiment-test.common-macros
  (:use :cl
        :ps-experiment.common-macros
        :ps-experiment-test.test-utils
        :parenscript
        :prove)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :ps-experiment.package
                :unintern-all-ps-symbol))
(in-package :ps-experiment-test.common-macros)

(plan 2)

(subtest
    "Test setf-with"
  (is-expand (setf-with obj
               x 100
               y 200
               z 300)
             (with-slots (x y z) obj
               (setf x 100
                     y 200
                     z 300))))

(subtest
    "Test with-slots-pair"
  (is-expand (with-slots-pair ((a (x b)) obj1
                               (c d e) obj2)
               (print (+ a x c d e)))
             (with-slots (a (x b)) obj1
               (with-slots (c d e) obj2
                 (print (+ a x c d e))))))

(finalize)
