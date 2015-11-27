(in-package :cl-user)
(defpackage ps-experiment.defines
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :defvar.ps
           :defun.ps
           :defstruct.ps)
  (:import-from :anaphora
                :aif
                :it)
  (:import-from :alexandria
                :symbolicate)
  (:import-from :ps-experiment.base
                :ps.)
  (:import-from :ps-experiment.package
                :make-ps-definer
                :add-unintern-all-ps-symbol-hook))
(in-package :ps-experiment.defines)

(defmacro defun.ps (name args &body body)
  (make-ps-definer
   :defun name
   `(defun ,name ,args
      ,@body)))

(defmacro defvar.ps (name initial-value)
  (make-ps-definer
   :defvar name
   `(defvar ,name ,initial-value)))


;; ----- defstruct ----- ;;

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
      (values (parse-defstruct-name name-and-options) nil)))

(defun parse-defstruct-slot-description (slot-description)
  (let ((result (mapcar (lambda (slot)
                          (if (consp slot)
                              slot
                              (list slot nil)))
                        slot-description)))
    (if (every (lambda (slot) (symbolp (car slot))) result)
        result
        (error 'type-error :expected-type 'symbol :datum slot-description))))

(defvar *ps-struct-slots* (make-hash-table)
  "Store slots of each structure made by defstruct.ps
key = structure-name
value = ({(slot-name slot-init-form}*)")

(add-unintern-all-ps-symbol-hook
 (lambda () (setf *ps-struct-slots* (make-hash-table))))
  
(defun find-defstruct-slots (parent)
  (aif (gethash parent *ps-struct-slots*)
       it
       (error 'unbound-variable :name parent)))
  
(defun merge-defstruct-slots (parent slots)
  (if (null parent)
      slots
      (let ((merged-slots (append (find-defstruct-slots parent)
                                  slots)))
        (if (= (length merged-slots)
               (length (remove-duplicates merged-slots)))
            merged-slots
            (error 'simple-error "duplicate slot name")))))

(defun register-defstruct-slots (name slots)
  (setf (gethash name *ps-struct-slots*) slots))

;; We refered goog.inherits for the inheritance code
;; https://github.com/google/closure-library/blob/master/closure/goog/base.js#L2170
(defmacro defstruct.ps (name-and-options &rest slot-description)
  "This is the tiny subset of defsturt in terms of syntax.
    name-and-options::= structure-name | (structure-name (:include included-structure-name))
    slot-description::= slot-name | (slot-name slot-init-form)"
  (bind:bind (((:values name parent)
               (parse-defstruct-name-and-options name-and-options))
              (slots
               (parse-defstruct-slot-description slot-description)))
    (setf slots (merge-defstruct-slots parent slots))
    (register-defstruct-slots name slots)
    `(progn
       (defun.ps ,name ()
         ,@(mapcar (lambda (slot)
                     `(setf (@ this ,(car slot)) ,(cadr slot)))
                   slots))
       (defun.ps ,(symbolicate 'make- name) (&key ,@slots)
         (let ((result (new (,name))))
           ,@(mapcar (lambda (elem)
                       `(setf (@ result ,(car elem)) ,(car elem)))
                     slots)
           result))
       ,@(mapcar (lambda (slot)
                   `(defpsmacro ,(symbolicate name '- (car slot)) (obj)
                      `(@ ,obj ,',(car slot))))
                 slots)
       (defun.ps ,(symbolicate name '-p) (obj)
         (instanceof obj ,name))
       ,(when parent
              (make-ps-definer
               :defvar-inheritance name
               `(funcall (lambda ()
                           (defun temp-ctor ())
                           (setf (@ temp-ctor prototype) (@ ,parent prototype))
                           (setf (@ ,name super-class_) (@ ,parent prototype))
                           (setf (@ ,name prototype) (new (temp-ctor)))
                           (setf (@ ,name prototype constructor) ,name)))))
       '(:struct ,name))))
