(in-package :cl-user)
(defpackage ps-experiment-test.utils
  (:use :cl
        :ps-experiment
        :parenscript
        :prove))
(in-package :ps-experiment-test.utils)

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
    "Test push"
  (is (ps (push a b))
      "b.push(a);"
      :test #'equal))

(finalize)
