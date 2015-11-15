(in-package :cl-user)
(defpackage ps-experiment.package
  (:use :cl
        :parenscript)
  (:export :register-ps-func
           :with-use-ps-pack)
  (:import-from :alexandria
                :flatten
                :hash-table-keys
                :with-gensyms)
  (:import-from :anaphora
                :aif
                :it)
  (:import-from :ps-experiment.utils.common
                :replace-dot-in-tree))
(in-package :ps-experiment.package)

(defparameter *ps-func-store* (make-hash-table))

(defun register-ps-func (name_)
  (symbol-macrolet ((target-lst (gethash *package* *ps-func-store*)))
    (unless (find name_ target-lst)
       (push name_ target-lst))))

(defun interleave (lst delim)
  (labels ((rec (result rest)
             (if (null rest)
                 result
                 (rec (append result (list (car rest) delim))
                      (cdr rest)))))
    (rec nil lst)))

(defun import-ps-funcs (ps-lst ps-body)
  (apply #'concatenate 'string
         (append
          (interleave (mapcar (lambda (elem) (funcall elem))
                              ps-lst)
                      "
")
          (list ps-body))))

(defmacro with-use-ps-pack (pack-sym-lst &body body)
  (with-gensyms (pack-lst func-lst)
    `(let* ((,pack-lst (if (equal (symbol-name (car ',pack-sym-lst)) "ALL")
                           (hash-table-keys *ps-func-store*)
                           (mapcar (lambda (sym)
                                     (let ((name (symbol-name sym))) 
                                       (if (equal name "THIS")
                                           ,*package*
                                           (aif (find-package name)
                                                it
                                                (error "There is no package named \"~A\"." name)))))
                                   ',pack-sym-lst)))
            (,func-lst (flatten
                        (mapcar (lambda (pack)
                                  (reverse (gethash pack *ps-func-store*)))
                                ,pack-lst))))
       (import-ps-funcs ,func-lst (ps ,@(replace-dot-in-tree body))))))
