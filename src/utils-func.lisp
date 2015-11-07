(in-package :cl-user)
(defpackage ps-experiment.utils.func
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:shadow :sb-debug
           :var)
  (:import-from :alexandria
                :flatten)
  (:import-from :anaphora
                :aif
                :it)
  (:import-from :ps-experiment.utils.common
                :replace-dot-in-tree)
  (:export :defun+ps
           :defun.ps
           :with-use-ps-pack))
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
                ,@ (interleave (mapcar (lambda (elem) (list elem))
                                       ps-lst)
                                                             "
")
                                   (ps ,@body)))

(defmacro with-use-ps-pack (pack-sym-lst &body body)
  (let* ((pack-lst (mapcar (lambda (sym)
                             (let ((name (symbol-name sym)))
                               
                               (if (equal name "THIS")
                                   *package*
                                   (aif (find-package name)
                                        it
                                        (error "There is no package named \"~A\"." name)))))
                           pack-sym-lst))
         (func-lst (flatten
                    (mapcar (lambda (pack)
                              (reverse (gethash pack *ps-func-store*)))
                            pack-lst))))
    `(with-import-ps-func ,func-lst
       ,@body)))
