(in-package :cl-user)
(defpackage ps-experiment.utils.func
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :defun.ps)
  (:import-from :ps-experiment.package
                :make-ps-definer)
  (:import-from :ps-experiment.utils.common
                :replace-dot-in-tree))
(in-package :ps-experiment.utils.func)

(defmacro defun.ps (name args &body body)
  (make-ps-definer
   'defun name
   `(defun ,name ,args
      ,@body)))
