(in-package :cl-user)
(defpackage ps-experiment-test.test-utils
  (:use :cl
        :prove)
  (:import-from :ps-experiment
                :with-use-ps-pack
                :ps.)
  (:import-from :cl-js
                :run-js
                :js-condition
                :with-js-env
                :empty-lib
                :undefined-variable)
  (:import-from :parenscript
                :defpsmacro
                :ps)
  (:import-from :alexandria
                :with-gensyms)
  (:export :execute-js
           :prove-macro-expand-error
           :prove-psmacro-expand-error
           :prove-in-both
           :with-prove-in-both
           :is-list.ps+
           :undefined-variable
           :*enable-js-prove*))
(in-package :ps-experiment-test.test-utils)

(defvar *enable-js-prove* t)

(defun skip-js-prove ()
  (assert (not *enable-js-prove*))
  (pass "--- Skip js test ---"))

(defun execute-js (js-str)
  (with-js-env ((empty-lib))
    (run-js js-str)))

(defmacro prove-macro-expand-error (code expected-error)
  `(is-error (macroexpand-1 ',code)
             ,expected-error))

(defmacro prove-psmacro-expand-error (code expected-error)
  `(is-error (macroexpand-1 '(ps ,code))
             ,expected-error))

(defmacro prove-in-both% ((cl-prove)
                          ((js-code js-body) js-prove)
                          &key (use '(:this)) (prints-js nil))
  `(progn
     (princ "Common Lisp: ")
     (fresh-line)
     ,cl-prove
     (princ "JavaScript: ")
     (fresh-line)
     (if *enable-js-prove*
         (let ((,js-code (with-use-ps-pack ,use ,js-body)))
           (when ,prints-js
             (print ,js-code))
           ,js-prove)
         (skip-js-prove))
     (princ "------")
     (fresh-line)))

(defmacro prove-in-both ((prove body &rest rest) &key (use '(:this)) (prints-js nil))
  `(prove-in-both%
    ((,prove ,body ,@rest))
    ((js ,body) ,(if (eq prove 'prove:is-error)
                     `(,prove (run-js js) 'js-condition)
                     `(,prove (run-js js) ,@rest)))
    :use ,use
    :prints-js ,prints-js))

(defun js-array-to-list (js-array)
  (let ((result nil))
    (dotimes (i (cl-js:js-array-length js-array))
      (push (cl-js:js-aref js-array i) result))
    (nreverse result)))

(defmacro is-list.ps+ (got expected &key (use '(:this)) (prints-js nil))
  (if (not (listp expected))
      (error 'type-error :expected-type 'list :datum expected))
  (with-gensyms (js-got js-expected)
    `(prove-in-both%
      ((is ,got ,expected :test #'equalp))
      ((js ,got) (let ((js-got (cl-js:run-js js))
                       (js-expected (cl-js:run-js (ps. ,expected))))
                   (is js-got js-expected :test #'equalp
                       (format nil "~A is expected to be ~A"
                               (js-array-to-list js-got)
                               (js-array-to-list js-expected)))))
      :use ,use
      :prints-js ,prints-js)))

;; --- prove for let --- ;;

;; [WIP]
;; TODO: (re)use implementations of 'prove'.

(defstruct ps-prove-definition
  name-symbol
  arg-list
  arg-list-to-call
  tester
  success-printer
  failure-printer)

(defvar *ps-prove-table* '())

(defun add-ps-prove-definition (&key name-symbol
                                  arg-list
                                  arg-list-to-call
                                  tester
                                  success-printer
                                  failure-printer)
  (let ((def (make-ps-prove-definition
              :name-symbol name-symbol
              :arg-list arg-list
              :arg-list-to-call arg-list-to-call
              :tester tester
              :success-printer success-printer
              :failure-printer failure-printer)))
    (setf *ps-prove-table*
          (remove-if (lambda (old-def)
                       (eq (ps-prove-definition-name-symbol old-def)
                           name-symbol))
                     *ps-prove-table*))
    (push def *ps-prove-table*)))

(defun construct-ps-prove-definition (def)
  (check-type def ps-prove-definition)
  (with-slots (name-symbol
               arg-list
               arg-list-to-call
               tester
               success-printer
               failure-printer) def
    `(,name-symbol ,arg-list
        (if (funcall ,tester ,@arg-list-to-call)
            (print (+ "  ✓ " (funcall ,success-printer ,@arg-list-to-call)))
            (print (+ "  × " (funcall ,failure-printer ,@arg-list-to-call)))))))

;; This is for checking ps-prove-definition in REPL.
(defun test-ps-prove (prove-name &rest rest)
  (let ((def (find-if (lambda (old-def)
                        (eq (ps-prove-definition-name-symbol old-def)
                            prove-name))
                      *ps-prove-table*)))
    (assert (not (null def)))
    (cl-js:run-js (ps:ps* `(flet (,(construct-ps-prove-definition def))
                             (,prove-name ,@rest))))))

(defpsmacro with-ps-prove (() &body body)
  `(flet ,(mapcar (lambda (def) (construct-ps-prove-definition def))
                  *ps-prove-table*)
     ,@body))

(defmacro with-prove-in-both ((&key (use '(:this))) &body body)
  `(progn
     (princ "Common Lisp: ")
     (fresh-line)
     ,@body
     (princ "JavaScript: ")
     (fresh-line)
     (if *enable-js-prove*
         (run-js (with-use-ps-pack (,@use)
                   (with-ps-prove ()
                     ,@body)))
         (skip-js-prove))
     (princ "------")
     (fresh-line)))

;; - default proves - ;;

;; Note: (ps:ps (funcall eq x y)) is compiled
;;           to 'eq(1, 2)'
;;       not to '1 === 2'.
(let ((args '(got expected &key (test (lambda (a b) (eq a b))))))
  (add-ps-prove-definition
   :name-symbol 'is
   :arg-list args
   :arg-list-to-call '(got expected :test test)
   :tester `(lambda ,args (funcall test got expected))
   :success-printer `(lambda ,args (+ got " is expected to be " expected))
   :failure-printer `(lambda ,args (+ got " is expected to be " expected))))

(let ((args '(got)))
  (add-ps-prove-definition
   :name-symbol 'ok
   :arg-list args
   :arg-list-to-call args
   :tester `(lambda ,args got)
   :success-printer `(lambda ,args (+ got " is expected to be T"))
   :failure-printer `(lambda ,args (+ got " is expected to be T"))))

(let ((args '(desc)))
  (add-ps-prove-definition
   :name-symbol 'pass
   :arg-list args
   :arg-list-to-call args
   :tester `(lambda ,args t)
   :success-printer `(lambda ,args desc)
   :failure-printer `(lambda ,args (error "internal error. should pass."))))

(let ((args '(desc)))
  (add-ps-prove-definition
   :name-symbol 'fail
   :arg-list args
   :arg-list-to-call args
   :tester `(lambda ,args nil)
   :success-printer `(lambda ,args (error "internal error. should fail."))
   :failure-printer `(lambda ,args desc)))

;; TODO: check kind of error
(defpsmacro is-error (form error-kind)
  (ps:with-ps-gensyms (error-p)
    `(let ((,error-p nil))
       (ps:try ,form
               (:catch (e)
                 (setf ,error-p t))
               (:finally
                (if ,error-p
                    (pass ,(format nil "~A is expected to raise an error" form))
                    (fail ,(format nil "~A is expected to raise a condition ~D"
                                   form
                                   error-kind))))))))
