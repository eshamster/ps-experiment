(in-package :cl-user)
(defpackage ps-experiment-test.package
  (:use :cl
        :ps-experiment.package
        :parenscript
        :ps-experiment-test.test-utils
        :prove))
(in-package :ps-experiment-test.package)

(plan nil)

(pass "This is tested at utils.func")

(finalize)
