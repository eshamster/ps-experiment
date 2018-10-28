(in-package :cl-user)
(defpackage ps-experiment/package
  (:use :cl
        :parenscript)
  (:export :def-top-level-form.ps
           :def-top-level-form.ps+
           :defun.ps-only
           :with-use-ps-pack
           :unintern-all-ps-symbol
           :register-ps-type
           :add-unintern-all-ps-symbol-hook)
  (:import-from :alexandria
                :flatten
                :hash-table-keys
                :once-only
                :with-gensyms
                :symbolicate
                :make-keyword)
  (:import-from :anaphora
                :acond
                :aif
                :it)
  (:import-from :ps-experiment/base
                :ps.
                :*original-package*)
  (:import-from :ps-experiment/defines/definer
                :*ps-func-store*
                :copy-ps-func-store
                :def-ps-definer
                :ps-func-func
                :ps-func-name-keyword
                :ps-func-require-exporting-p
                :find-ps-func
                :*ps-type-store*
                :ps-type-p)
  (:import-from :ps-experiment/util-sorter
                :get-node-name
                :node-equalp
                :get-children
                :sort-tree-node))
(in-package :ps-experiment/package)

;; --- Define methods to sort packages --- ;;

(defstruct package-dependency pack depending-packs)

;; Note: Because get-children is very frequently called when sorting package,
;; we calculate all dependencies in advance.
(defun init-package-dependency (pack)
  (let ((cache (make-hash-table)))
    (labels ((get-ps-package (pack)
               (gethash pack *ps-func-store*))
             (push-new-package (dependencies current-pack target-pack)
               (when (and (not (eq current-pack target-pack))
                          (get-ps-package target-pack))
                 (pushnew (aif (gethash target-pack cache)
                               it
                               (rec target-pack))
                          (package-dependency-depending-packs dependencies))))
             (rec (current-pack)
               (let ((result (make-package-dependency :pack current-pack)))
                 (setf (gethash current-pack cache) result)
                 (do-symbols (sym current-pack)
                   (let ((target-pack (symbol-package sym)))
                     (push-new-package result current-pack target-pack)))
                 (dolist (target-pack (package-use-list current-pack))
                   (push-new-package result current-pack target-pack))
                 result)))
      (rec pack))))

(defmethod get-node-name ((node package-dependency))
  (package-name (package-dependency-pack node)))

(defmethod get-children ((node package-dependency))
  (package-dependency-depending-packs node))

(defmethod node-equalp ((node1 package-dependency) (node2 package-dependency))
  (eq (package-dependency-pack node1)
      (package-dependency-pack node2)))

;; --- ;;

;; Dirty solution to print only not-imported symbol with package prefix...

(defvar *internal-symbol-prefix* "_internal")

(defun package-to-js-string (pack)
  (ps:symbol-to-js-string
    (intern (ppcre:regex-replace-all "[\\./]"
                                     (package-name pack)
                                     "_")
            "KEYWORD")))

(defun ps:symbol-to-js-string (symbol &optional (mangle-symbol-name? t))
  (let* ((symbol-name (symbol-name (ps::maybe-obfuscate-symbol symbol)))
         (identifier (if mangle-symbol-name?
                         (ps::encode-js-identifier symbol-name)
                         symbol-name))
         (package (symbol-package symbol))
         (same-name-symbol (when *original-package*
                             (find-symbol (symbol-name symbol) *original-package*))))
    (if *original-package*
        (if (and (not (eq *original-package* package))
                 ;; Check if it is imported
                 (or (null same-name-symbol)
                     (not (eq symbol same-name-symbol)))
                 ;; Check if it is registered as a ps-function
                 (find-ps-func symbol package))
            (let ((*original-package* nil)
                  (package-name (package-to-js-string package)))
              (if (eq (get-symbol-status symbol package) :external)
                  (concatenate 'string package-name "." identifier)
                  (concatenate 'string package-name "." *internal-symbol-prefix* "." identifier)))
            identifier)
        (aif (ps-package-prefix (symbol-package symbol))
             (concatenate 'string it identifier)
             identifier))))

;; If a symbol is registered as a type-specifier, quote before it is removed.
;; In Common Lisp, type-specifier can be specified using symbol, but it is
;; converted to just a string by Parenscript. In other words, the information of
;; package is lost. So in other package, we can no longer extract the type.
(ps::define-expression-operator quote (x)
  (flet ((quote% (expr) (when expr `',expr)))
    (ps::compile-expression
     (typecase x
       (cons `(array ,@(mapcar #'quote% x)))
       ((or null (eql [])) '(array))
       (keyword x)
       (symbol (if (ps-type-p x)
                   x
                   (symbol-to-js-string x)))
       (number x)
       (string x)
       (vector `(array ,@(loop for el across x collect (quote% el))))))))

;; --- ;;

(def-ps-definer def-top-level-form.ps (id-name &body body) (:require-exporting-p nil)
  `(progn ,@body))

(defmacro def-top-level-form.ps+ (id-name &body body)
  `(progn (def-top-level-form.ps ,id-name ,@body)
          (progn ,@body)))

;; TODO: Consider in what package we should place defun.ps and defun.ps+
(def-ps-definer defun.ps-only (name args &body body) ()
  `(defun ,name ,args ,@body))

(defvar *unintern-all-ps-symbol-hook* nil)

(defun unintern-all-ps-symbol ()
  (setf *ps-func-store* (make-hash-table))
  (setf *ps-type-store* (make-hash-table))
  (dolist (hook *unintern-all-ps-symbol-hook*)
    (funcall hook)))

(defun add-unintern-all-ps-symbol-hook (hook)
  (if (functionp hook)
      (push hook *unintern-all-ps-symbol-hook*)
      (error 'type-error :expected-type 'function :datum hook)))

(defun get-symbol-status (sym pack)
  (multiple-value-bind (found status)
      (find-symbol (symbol-name sym) pack)
    (assert found)
    status))

#|
Create string like the following (The sort order is not stable):
var symA = packageA.symA;
var symB = packageA.symB;
var symC = packageB.symC;
|#
(defun make-imported-js-symbols (pack)
  (let ((imported-lst nil))
    (do-symbols (sym pack)
      (let* ((target-pack (symbol-package sym))
             (ps-func (find-ps-func sym target-pack)))
        (when (and (not (eq target-pack pack))
                   ps-func
                   (ps-func-require-exporting-p ps-func)) 
          (push sym imported-lst))))
    (format nil "~{~{var ~A = ~A.~A;~}~%~}"
            (mapcar (lambda (sym)
                      (let ((js-sym (symbol-to-js-string (make-keyword sym))))
                        (list js-sym
                              (package-to-js-string (symbol-package sym))
                              js-sym)))
                    imported-lst))))

#|
Create string like the following (The sort order is not stable):
return {
  'externalSymA': externalSymA,
  'externalSymB': externalSymB,
  _internal: {
    'internalSymA': internalSymA,
    'internalSymB': internalSymB,
  }
};
|#
(defun make-exported-js-symbols (pack)
  (let ((extern-lst nil)
        (internal-lst nil))
    (flet ((keyword-to-js-string (key)
             (check-type key keyword)
             (symbol-to-js-string key)))
      (let ((ps-func-lst (gethash pack *ps-func-store*)))
        (dolist (ps-func ps-func-lst)
          (when (ps-func-require-exporting-p ps-func)
            (let ((key (ps-func-name-keyword ps-func)))
              (if (eq (get-symbol-status key pack) :external)
                  (push (keyword-to-js-string key) extern-lst)
                  (push (keyword-to-js-string key) internal-lst)))))))
    (format nil
            "return {
~{  '~A': ~:*~A,~%~}  '~A': {
~{    '~A': ~:*~A,~%~}  }
};"
            extern-lst *internal-symbol-prefix* internal-lst)))

(defun make-packaged-js (pack)
  (let ((ps-funcs (gethash pack *ps-func-store*)))
    (unless ps-funcs
      (return-from make-packaged-js nil))
    (let ((js-pack-name (package-to-js-string pack))
          (js-body (format nil "~{~A~%~}"
                           (mapcar (lambda (ps-func)
                                     (funcall (ps-func-func ps-func)))
                                   (reverse ps-funcs)))))
      (format nil "var ~A = (function() {~%~{~A~%~}})();~%"
              js-pack-name
              (mapcar (lambda (str)
                        (ppcre:regex-replace
                         "\\s*$"
                         (ppcre:regex-replace-all (ppcre:create-scanner "^" :multi-line-mode t)
                                                  str
                                                  "  ")
                         ""))
                      (list "/* --- import symbols --- */" (make-imported-js-symbols pack)
                            "/* --- define objects --- */" js-body
                            "/* --- extern symbols --- */" (make-exported-js-symbols pack)))))))

(defun import-ps-funcs (pack-lst ps-body)
  (format nil "~{~A~%~}~A"
          (remove-if #'null (mapcar #'make-packaged-js pack-lst))
          ps-body))

;; The reverse is heuristic to sort packages according to the order of input
;; as far as possible.
;; The sort-tree-node doesn't guarantee order between independent nodes.
(defun make-package-list-with-depend (package-lst)
  (mapcar #'package-dependency-pack
          (sort-tree-node (reverse (mapcar #'init-package-dependency package-lst)))))

(defmacro with-use-ps-pack (pack-sym-lst &body body)
  (with-gensyms (pack-lst)
    `(let* ((,pack-lst ',(if (equal (symbol-name (car pack-sym-lst)) "ALL")
                             (hash-table-keys *ps-func-store*)
                             (mapcar (lambda (sym)
                                       (let ((name (symbol-name sym)))
                                         (if (equal name "THIS")
                                             *package*
                                             (aif (find-package name)
                                                  it
                                                  (error "There is no package named \"~A\"." name)))))
                                     pack-sym-lst)))
            (*ps-func-store* (copy-ps-func-store)))
       (defun.ps-only ,(intern "__PS-MAIN-FUNC__" *package*) () ,@body)
       (import-ps-funcs (make-package-list-with-depend ,pack-lst)
                        (format nil "~A.~A.__psMainFunc__();"
                                (package-to-js-string ,*package*)
                                *internal-symbol-prefix*)))))
