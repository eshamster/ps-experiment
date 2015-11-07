(in-package :cl-user)
(defpackage ps-experiment
  (:use :cl
        :parenscript)
  (:import-from :ps-experiment.utils
                :setf-with
                :load-ps
                :defun+ps
                :defun.ps
                :with-use-ps-pack
                :ps.
                :defmacro.ps)
  (:export :setf-with
           :load-ps
           :defun+ps
           :defun.ps
           :with-use-ps-pack
           :ps.
           :defmacro.ps))
(in-package :ps-experiment)

(defun.ps f1 (a b)
  (+ a b))

(defun.ps f2 (b)
  (f1 10 b))

(defun.ps f3 ()
  (+ 1 2 3))

(maphash (lambda (k v)
           (print k)
           (print v))
         ps-experiment.utils.func::*ps-func-store*)

(print
 (with-use-ps-pack (ps-experiment.sample-pack
                    this)
   (fs1 200)
   (f2 100)))
