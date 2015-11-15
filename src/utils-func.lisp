(in-package :cl-user)
(defpackage ps-experiment.utils.func
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :defun+ps
           :defun.ps)
  (:import-from :ps-experiment.package
                :register-ps-func)
  (:import-from :ps-experiment.utils.common
                :replace-dot-in-tree))
(in-package :ps-experiment.utils.func)

(defun intern-ub (sym)
  (intern (format nil "~A_" (symbol-name sym))))

(defmacro defun+ps (name args &body body)
  (let ((name_ (intern-ub name)))
    (register-ps-func name_)
    `(defun ,name_ ()
       (ps
         (defun ,name ,args
           ,@body)))))

(defmacro defun.ps (name args &body body)
  `(defun+ps ,name ,args
     ,@(replace-dot-in-tree body)))
