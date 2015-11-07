(in-package :cl-user)
(defpackage ps-experiment.utils.func
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:shadow :sb-debug
           :var)
  (:import-from :ps-experiment.utils.common
                :replace-dot-in-tree)
  (:export :defun+ps
           :defun.ps
           :with-import-ps-func))
(in-package :ps-experiment.utils.func)

(defparameter *ps-func-store* (make-hash-table))

(defmacro register-ps-func (name_)
  `(symbol-macrolet ((target-lst (gethash *package* *ps-func-store*)))
     (unless (find ,name_ target-lst)
       (push ,name_ target-lst))))

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

(defun interleave (lst delim)
  (labels ((rec (result rest)
             (if (null rest)
                 result
                 (rec (append result (list (car rest) delim))
                      (cdr rest)))))
    (rec nil lst)))

(defmacro with-import-ps-func (ps-lst &body body)
  `(concatenate 'string
                ,@ (interleave (mapcar (lambda (elem) (list (intern-ub elem)))
                                       ps-lst)
                                                             "
")
                                   (ps ,@body)))
