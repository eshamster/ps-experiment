(defpackage ps-experiment-tests-for-utils/main
  (:nicknames :ps-experiment-tests-for-utils)
  (:use :cl
        :rove)
  (:export :main))
(in-package :ps-experiment-tests-for-utils/main)

(defun main ()
  (let ((rove:*report-stream* (make-broadcast-stream)))
    (assert (run :ps-experiment-tests-for-utils/success))
    (assert (not (run :ps-experiment-tests-for-utils/failure)))))
