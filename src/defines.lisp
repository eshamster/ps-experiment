(in-package :cl-user)
(defpackage ps-experiment.defines
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :defvar.ps
           :defun.ps
           :defstruct.ps)
  (:import-from :alexandria
                :symbolicate)
  (:import-from :ps-experiment.utils.common 
                :ps.)
  (:import-from :ps-experiment.package
                :make-ps-definer
                :register-ps-func))
(in-package :ps-experiment.defines)

(defmacro defun.ps (name args &body body)
  (make-ps-definer
   'defun name
   `(defun ,name ,args
      ,@body)))

(defmacro defvar.ps (name initial-value)
  (make-ps-definer
   'defvar name
   `(defvar ,name ,initial-value)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun parse-defstruct-name (name)
    (if (symbolp name)
        name
        (error 'type-error :expected-type 'symbol :datum name)))
  
  (defun parse-defstruct-options (options)
    (unless (eq (car options) :include)
      (error "unknown DEFSTRUCT.PS option:~% ~S" options))
    (unless (symbolp (cadr options))
      (error 'type-error :expected-type 'symbol :datum (cadr options)))
    (cadr options))
  
  (defun parse-defstruct-name-and-options (name-and-options)
    (if (listp name-and-options)
        (values (parse-defstruct-name (car name-and-options))
                (parse-defstruct-options (cadr name-and-options)))
        (values (parse-defstruct-name name-and-options) nil))))

;; We refered goog.inherits for the inheritance code
;; https://github.com/google/closure-library/blob/master/closure/goog/base.js#L2170
(defmacro defstruct.ps (name-and-options &rest slot-description)
  "This is the tiny subset of defsturt in terms of syntax.
    name-and-options::= structure-name | (structure-name (:include included-structure-name))
    slot-description::= slot-name | (slot-name slot-init-form)"
  (multiple-value-bind (name parent)
      (parse-defstruct-name-and-options name-and-options)
    `(progn
       (defun.ps ,name ()
         ,(when parent
                `((@ ,parent call) this))
         ,@(mapcar (lambda (elem)
                     (if (consp elem)
                         `(setf (@ this ,(car elem)) ,(cadr elem))
                         `(setf (@ this ,elem) nil)))
                   slot-description))
       (defun.ps ,(symbolicate name '-p) (obj)
         (instanceof obj ,name))
       ,(when parent
              (make-ps-definer
               'defvar-inheritance name
               `(funcall (lambda ()
                           (defun temp-ctor ())
                           (setf (@ temp-ctor prototype) (@ ,parent prototype))
                           (setf (@ ,name super-class_) (@ ,parent prototype))
                           (setf (@ ,name prototype) (new (temp-ctor)))
                           (setf (@ ,name prototype constructor) ,name)))))
       '(:struct ,name))))
