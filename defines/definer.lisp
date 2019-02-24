(in-package :cl-user)
(defpackage ps-experiment/defines/definer
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:export :*ps-func-store*
           :copy-ps-func-store
           :find-ps-func
           :def-ps-definer
           :make-ps-definer

           :ps-func
           :make-ps-func
           :ps-func-name-keyword
           :ps-func-func
           :ps-func-body
           :ps-func-require-exporting-p

           :*ps-type-store*
           :register-ps-type
           :ps-type-p)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :ps-experiment/base
                :ps.))
(in-package :ps-experiment/defines/definer)

;; --- ps-func-store --- ;;

(defstruct ps-func name-keyword (func (lambda () "")) body (require-exporting-p t))

(defparameter *ps-func-store* (make-hash-table)
  "Key: package, Value: list of ps-func")

(defun copy-ps-func-store (&optional (store *ps-func-store*))
  "Copy *ps-func-store*.
Note: ps-func in a list of the (hash) value is not copied. So although you can edit list,
you cannot edit ps-func itself."
  (let ((result (make-hash-table)))
    (maphash (lambda (pack ps-func-lst)
               (setf (gethash pack result)
                     (copy-list ps-func-lst)))
             store)
    result))

;; Manage symbols in the package as a list but not a hash because
;; we want to remain the definition order.

(defun find-ps-func (symbol-name &optional (package *package*))
  (let ((key (make-keyword symbol-name))
        (ps-func-lst (gethash package *ps-func-store*)))
    (find-if (lambda (ps-func)
               (eq key (ps-func-name-keyword ps-func)))
             ps-func-lst)))

(defun register-ps-func (ps-func package)
  (check-type ps-func ps-func)
  (check-type (ps-func-name-keyword ps-func) keyword)
  (symbol-macrolet ((target-lst (gethash package *ps-func-store*)))
    (setf target-lst
          (delete ps-func target-lst
                  :test (lambda (a b) (eq (ps-func-name-keyword a)
                                          (ps-func-name-keyword b)))))
    (push ps-func target-lst)))

(defun make-ps-definer (&key name before body require-exporting-p)
  `(progn ,before
          (register-ps-func
           (make-ps-func :name-keyword (make-keyword ',name)
                         :func (lambda () (ps. ,body))
                         :body ',body
                         :require-exporting-p ,require-exporting-p)
           ,*package*)))

(defun parse-name (name)
  (if (consp name)
      (parse-name (car name))
      name))

(defmacro def-ps-definer (def-name (name &rest rest-args) (&key before (require-exporting-p t))
                          &body body)
  `(defmacro ,def-name (,name ,@rest-args)
     (make-ps-definer :name (parse-name ,name)
                      :before ,before
                      :require-exporting-p ,require-exporting-p
                      :body (progn ,@body))))

;; --- ps type store --- ;;

(defparameter *ps-type-store* (make-hash-table))

(defun register-ps-type (type-specifier)
  (check-type type-specifier symbol)
  (setf (gethash type-specifier *ps-type-store*) t))

(defun ps-type-p (symbol)
  (gethash symbol *ps-type-store*))
