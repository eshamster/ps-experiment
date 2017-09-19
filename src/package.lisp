(in-package :cl-user)
(defpackage ps-experiment.package
  (:use :cl
        :parenscript)
  (:export :register-ps-func
           :def-ps-definer
           :def-top-level-form.ps
           :with-use-ps-pack
           :find-ps-symbol
           :unintern-all-ps-symbol
           :add-unintern-all-ps-symbol-hook)
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
  (:import-from :ps-experiment.base
                :ps.)
  (:import-from :ps-experiment.util-sorter
                :get-node-name
                :node-equalp
                :get-children
                :sort-tree-node))
(in-package :ps-experiment.package)

(defparameter *ps-func-store* (make-hash-table))

;; --- Define methods to sort packages --- ;;

(defmethod get-node-name ((node package))
  (package-name node))

(defmethod get-children ((node package))
  (package-use-list node))

(defmethod node-equalp ((node1 package) (node2 package))
  (eq node1 node2))

;; --- ;;

(defun register-ps-func (name_sym)
  (symbol-macrolet ((target-lst (gethash *package* *ps-func-store*)))
    (unless (find name_sym target-lst)
       (push name_sym target-lst))))

(defun make-ps-definer (&key kind name before body) 
  (let ((register-name (symbolicate '_ kind '_ name)))
    `(progn ,before
            (register-ps-func ',register-name)
            (defun ,register-name ()
              (ps. ,body)))))

(defun parse-name (name)
  (if (consp name)
      (parse-name (car name))
      name))

(defmacro def-ps-definer (def-name (name &rest rest-args) (&key before) &body body)
  `(defmacro ,def-name (,name ,@rest-args)
     (make-ps-definer :kind ',def-name
                      :name (parse-name ,name)
                      :before ,before
                      :body (progn ,@body))))

(def-ps-definer def-top-level-form.ps (id-name &body body) ()
  `(progn ,@body))

(defun find-ps-symbol (string &optional (package (package-name *package*)))
  (let ((found-package (find-package package)))
    (acond ((null found-package) (values nil nil))
           ((find-if (lambda (sym) (equal (symbol-name sym) string))
                     (gethash found-package *ps-func-store*))
            (values it (if (eq found-package *package*)
                           :internal
                           :external)))
           (t (values nil nil)))))

(defvar *unintern-all-ps-symbol-hook* nil)

(defun unintern-all-ps-symbol ()
  (setf *ps-func-store* (make-hash-table))
  (dolist (hook *unintern-all-ps-symbol-hook*)
    (funcall hook)))

(defun add-unintern-all-ps-symbol-hook (hook)
  (if (functionp hook)
      (push hook *unintern-all-ps-symbol-hook*)
      (error 'type-error :expected-type 'function :datum hook)))

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

;; The reverse is heuristic to sort packages according to the order of input
;; as far as possible.
;; The sort-tree-node doesn't guarantee order between independent nodes.
(defun make-package-list-with-depend (package-lst)
  (sort-tree-node (reverse package-lst)))

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
                                (make-package-list-with-depend ,pack-lst)))))
       (import-ps-funcs ,func-lst (ps. ,@body)))))
