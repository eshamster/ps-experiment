(defpackage ps-experiment/t/test-utils
  (:use :cl
        :rove)
  (:import-from :ps-experiment
                :with-use-ps-pack
                :ps.
                :defvar.ps
                :defun.ps)
  (:import-from :cl-js
                :run-js
                :js-condition
                :with-js-env
                :empty-lib
                :undefined-variable)
  (:import-from :parenscript
                :defpsmacro
                :ps
                :@)
  (:import-from :alexandria
                :with-gensyms)
  (:export :execute-js
           :deftest.ps+
           :signals-when-expand
           :undefined-variable))
(in-package :ps-experiment/t/test-utils)

(defun js-prove-enable-p ()
  (not (uiop:getenvp "SKIP_JS")))

(defun execute-js (js-str)
  (with-js-env ((empty-lib))
    (run-js js-str)))

(defvar.ps *current-indent* "        ")

(defun.ps get-current-indent ()
  *current-indent*)
(defun.ps get-once-adding-indent ()
  "  ")

(defun.ps increase-current-indent ()
  (setf *current-indent*
        (+ *current-indent* (get-once-adding-indent))))
(defun.ps decrease-current-indent ()
  (setf *current-indent*
        ((@ *current-indent* substr)
         (@ (get-once-adding-indent) length)
         (1- (@ *current-indent* length)))))

(defun.ps print-for-test (text)
  (print (+ (get-current-indent) text)))

;; TODO: Adjust indent
(defmacro deftest-body.ps (&body body)
  `(run-js
    (with-use-ps-pack (:this)
      (let ((all-tests-ok-p t))
        (macrolet
            ((okng (ok-p form &optional desc)
               (declare (ignore desc))
               (ps:with-ps-gensyms (result form-string)
                 `(let ((,result ,form)
                        (,form-string ,(format nil "~A" form)))
                    (if (or (and ,ok-p ,result)
                            (and (not ,ok-p) (not ,result)))
                        (print-for-test (+ "✓ Expect " ,form-string " to be "
                                           ,(if ok-p "true" "false")))
                        (progn (print-for-test (+ "× " ,form-string))
                               (setf all-tests-ok-p false))))))
             (ok (form &optional desc)
               `(okng t ,form ,desc))
             (ng (form &optional desc)
               `(okng nil ,form ,desc))
             (expands (form expanded-form &optional env)
               ;; not implemented
               (declare (ignore form expanded-form env)))
             (signals (form &optional (condition ''error))
               ;; TODO: check kind of error
               (declare (ignore condition))
               (ps:with-ps-gensyms (error-p)
                 `(let ((,error-p nil))
                    (ps:try ,form
                            (:catch (e)
                              (setf ,error-p t))
                            (:finally ,error-p)))))
             (testing (desc &body body)
               `(unwind-protect
                     (progn (print-for-test ,desc)
                            (increase-current-indent)
                            ,@body)
                  (decrease-current-indent))))
          (declare (ignorable ok ng expands signals testing))
          ,@body
          all-tests-ok-p)))))

(defmacro deftest.ps+ (name &body body)
  "Test code as both Common Lisp and JavaScript.
If \"SKIP_JS\" environment variable is defined, tests for Javascript is skipped."
  (with-gensyms ((js-result "JS-RESULT"))
    `(deftest ,name
       (testing "---- Common Lisp ----"
         ,@body)
       (testing "---- JavaScript ----"
         (if (js-prove-enable-p)
             (let ((,js-result (deftest-body.ps ,@body)))
               (ok ,js-result))
             (skip "Skip JavaScript tests"))))))

(defun signals-when-expand-impl (code expected-error &key for-ps expand-times)
  (labels ((rec-expand (result rest-times)
             (if (> rest-times 0)
                 (rec-expand `(macroexpand-1 ,result) (1- rest-times))
                 result)))
    `(signals ,(rec-expand (if for-ps `(ps ,code) `(quote ,code)) expand-times)
              ,expected-error)))

(defmacro signals-when-expand (code expected-error &key (expand-times 1))
  (signals-when-expand-impl code expected-error
                            :for-ps nil :expand-times expand-times))
