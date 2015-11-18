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

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun parse-defstruct-name (name)
    (if (symbolp name)
        name
        (error 'type-error :expected-type 'symbol :datum name)))
  
  (defun parse-defstruct-options (options)
    (error "TODO: Not implemented yet")
    options)
  
  (defun parse-defstruct-name-and-options (name-and-options)
    (if (listp name-and-options)
        (values (parse-defstruct-name (car name-and-options))
                (parse-defstruct-options (cadr name-and-options)))
        (values (parse-defstruct-name name-and-options) nil))))

(defmacro defvar.ps (name initial-value)
  (let ((register-name (symbolicate '_defvar_ name)))
    (register-ps-func register-name)
    `(defun ,register-name ()
       (ps (defvar ,name ,(replace-dot-in-tree initial-value))))))

(defmacro defstruct.ps (name-and-options &rest slot-description)
  "This is the tiny subset of defsturt in terms of syntax.
    name-and-options::= structure-name | (structure-name (:include included-structure-name))
    slot-description::= slot-name | (slot-name slot-init-form)"
  (multiple-value-bind (name parent)
      (parse-defstruct-name-and-options name-and-options)
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
                         slot-description))))
         (defun.ps ,(symbolicate name '-p) (obj)
           (instanceof obj ,name))
         '(:struct ,name)))))
