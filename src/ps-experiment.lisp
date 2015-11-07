(in-package :cl-user)
(defpackage ps-experiment
  (:use :cl
        :parenscript)
  (:import-from :ps-experiment.utils
                :defun.ps
                :with-import-ps-func))
(in-package :ps-experiment)

(defun.ps f1 (a b)
  (+ a b))

(defun.ps f2 (b)
  (f1 10 b))

(maphash (lambda (k v)
           (print k)
           (print v))
         ps-experiment.utils.func::*ps-func-store*)

(print
 (with-import-ps-func (f1 f2)
   (f2 100)))
