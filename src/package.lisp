(in-package :cl-user)
(defpackage ps-experiment.package
  (:use :cl
        :parenscript)
  (:export :register-ps-func
           :make-ps-definer
           :with-use-ps-pack
           :find-ps-symbol
           :unintern-all-ps-symbol)
  (:import-from :alexandria
                :flatten
                :hash-table-keys
                :once-only
                :with-gensyms
                :symbolicate)
  (:import-from :anaphora
                :acond
                :aif
                :it)
  (:import-from :ps-experiment.utils.common
                :ps.))
(in-package :ps-experiment.package)

(defparameter *ps-func-store* (make-hash-table))

(defun register-ps-func (name_sym)
  (symbol-macrolet ((target-lst (gethash *package* *ps-func-store*)))
    (unless (find name_sym target-lst)
       (push name_sym target-lst))))

(defun make-ps-definer (kind name body) 
  (let ((register-name (symbolicate '_ kind '_ name)))
    (register-ps-func register-name)
    `(defun ,register-name ()
       (ps. ,body))))

(defun find-ps-symbol (string &optional (package (package-name *package*)))
  (let ((found-package (find-package package)))
    (acond ((null found-package) (values nil nil))
           ((find-if (lambda (sym) (equal (symbol-name sym) string))
                     (gethash found-package *ps-func-store*))
            (values it (if (eq found-package *package*)
                           :internal
                           :external)))
           (t (values nil nil)))))

(defun unintern-all-ps-symbol ()
  (setf *ps-func-store* (make-hash-table)))

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
       (import-ps-funcs ,func-lst (ps. ,@body)))))
