(in-package :cl-user)
(defpackage ps-experiment.defines
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :defvar.ps
           :defstruct.ps)
  (:import-from :alexandria
                :symbolicate)
  (:import-from :ps-experiment.utils.func
                :defun.ps)
  (:import-from :ps-experiment.utils.common
                :replace-dot-in-tree)
  (:import-from :ps-experiment.package
                :register-ps-func))
(in-package :ps-experiment.defines)

(defmacro defvar.ps (name initial-value)
  (let ((register-name (symbolicate '_defvar_ name)))
    (register-ps-func register-name)
    `(defun ,register-name ()
       (ps (defvar ,name ,(replace-dot-in-tree initial-value))))))

(defmacro defstruct.ps (name &rest name-and-options)
  (let ((register-name (symbolicate '_defstruct_ name)))
    (register-ps-func register-name)
    `(progn
       (defun ,register-name ()
         (ps
           (defun ,name ()
             ,@(mapcar (lambda (elem)
                         (if (consp elem)
                             `(setf (@ this ,(car elem)) ,(replace-dot-in-tree (cadr elem)))
                             `(setf (@ this ,elem) nil)))
                       name-and-options))))
       (defun.ps ,(symbolicate name 'p) (obj)
         (instanceof obj ,name))
       '(:struct ,name))))
