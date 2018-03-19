(in-package :cl-user)
(defpackage ps-experiment-test.ps-macros-for-compatibility
  (:use :cl
        :ps-experiment
        :ps-experiment-test.test-utils
        :parenscript
        :prove)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :ps-experiment.package
                :unintern-all-ps-symbol))
(in-package :ps-experiment-test.ps-macros-for-compatibility)

(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

(plan 24)

(subtest
    "Test c[ad]{1-2}r (Limitation: cd[ad]*r cannot be used for setting)"
  (macrolet ((prove-setf (cxr base-lst value expected)
               `(is-list.ps+ (let ((lst ,base-lst))
                               (setf (,cxr lst) ,value)
                               lst)
                             ,expected)))
    ;; car
    (prove-in-both (is (car '(1 2 3)) 1))
    (prove-setf car (list 1 2 3) 4 '(4 2 3))
    (prove-in-both (ok (null (car '()))))
    ;; cdr
    (is-list.ps+ (cdr '(1 2 3)) '(2 3))
    (is-list.ps+ (cdr '(1)) '())
    (is-list.ps+ (cdr '()) '())
    ;; caar
    (prove-in-both (is (caar '((1) 2 3)) 1))
    (prove-setf caar (list (list 1) 2 3) 4 '((4) 2 3))
    ;; cadr
    (prove-in-both (is (cadr '(1 2 3)) 2))
    (prove-setf cadr (list 1 2 3) 4 '(1 4 3))
    ;; cdar
    (is-list.ps+ (cdar '((1 4 9) 2 3)) '(4 9))
    ;; cddr
    (is-list.ps+ (cddr '(1 2 3)) '(3))))

(subtest
    "Test nth"
  (with-prove-in-both ()
    (is (nth 2 '(1 2 3 4)) 3)
    (ok (null (nth 2 '(1 2))))))

(subtest
    "Test listp and atom"
  (subtest
      "Test listp"
    (with-prove-in-both ()
      (ok (listp nil))
      (ok (listp '()))
      (ok (listp '(1 2 3)))
      (ok (not (listp 12)))
      (ok (not (listp 'abc)))))
  (subtest
      "Test atom"
    (with-prove-in-both ()
      (ok (atom nil))
      (ok (atom '()))
      (ok (not (atom '(1 2 3))))
      (ok (atom 12))
      (ok (atom 'abc)))))

(subtest
    "Test subseq"
  (is-list.ps+ (subseq '(1 2 3) 1)
               '(2 3))
  (is-list.ps+ (subseq '(1 2 3 4) 2 3)
               '(3))
  (is-list.ps+ (let* ((x '(1 2 3 4))
                      (y (subseq x 2 3)))
                 (list x y))
               '((1 2 3 4) (3))))

(subtest
    "Test macros like push"
  (subtest
      "Test push"
    (is-list.ps+ (let ((x '()))
                   (push 1 x)
                   (push 2 x))
                 '(2 1)))
  (subtest
      "Test pushnew"
    (is-list.ps+ (let ((x '(1 2)))
                   (pushnew 3 x)
                   (pushnew 2 x))
                 '(3 1 2))
    (is-list.ps+ (let ((x '(1 2)))
                   (pushnew 12 x :test (lambda (a b) (= (/ a 6) b)))
                   (pushnew 6 x :test (lambda (a b) (= a (/ b 6)))))
                 '(6 1 2))))

(subtest
    "Test every"
  (with-prove-in-both ()
    (ok (every (lambda (x) (> x 2)) '(3 4 5)))
    (ok (not (every (lambda (x) (> x 2)) '(2 3 4))))))

(subtest
    "Test some"
  (with-prove-in-both ()
    (ok (some (lambda (x) (< x 2)) '(2 1 3)))
    (ok (not (some (lambda (x) (< x 2)) '(2 3 4))))))

(subtest
    "Test find"
  (with-prove-in-both ()
    (is (find 3 '(1 2 3 4)) 3)
    (ok (not (find 5 '(1 2 3 4))))))

(subtest
    "Test find-if"
  (with-prove-in-both ()
    (is (find-if (lambda (x) (> x 2)) '(2 1 3 4)) 3)
    (ok (not (find-if (lambda (x) (> x 10)) '(2 1 3 4))))))

(subtest
    "Test getf"
  (with-prove-in-both ()
    (is (getf (getf '(:x 1 :y (:a 3 :b 10 :c 4)) :y) :b) 10)
    (is (getf (getf '(:x 1 :y (:a 3 :b 10 :c 4)) :y)
              :not-exist
              999)
        999)))

(subtest
    "Test reduce"
  (prove-in-both (is (reduce #'(lambda (x y) (+ x y)) '(1 2 3 4))
                     10))
  (is-list.ps+ (reduce #'(lambda (x y) (list (+ (car x) (car y))
                                             (+ (cadr x) (cadr y))))
                       '((1 2) (3 4) (5 6)))
               '(9 12)))

(subtest
    "Test remove"
  (is-list.ps+ (let ((lst '(1 2 3 2 4)))
                 (remove 2 lst))
               '(1 3 4))
  (is-list.ps+ (let ((lst '(1 2 3 2 4)))
                    (remove 2 lst)
                    lst)
               '(1 2 3 2 4)))

(subtest
    "Test remove-if"
  (is-list.ps+ (let ((lst '(1 2 3 4)))
                    (remove-if (lambda (x) (> x 2)) lst))
                  '(1 2))
  (is-list.ps+ (let ((lst '(1 2 3 4)))
                    (remove-if (lambda (x) (> x 2)) lst)
                    lst)
                  '(1 2 3 4)))

(subtest
    "Test remove-if-not"
  (is-list.ps+ (let ((lst '(1 2 3 4)))
                    (remove-if-not (lambda (x) (> x 2)) lst))
                  '(3 4))
  (is-list.ps+ (let ((lst '(1 2 3 4)))
                    (remove-if-not (lambda (x) (> x 2)) lst)
                    lst)
                  '(1 2 3 4)))

(subtest
    "Test reverse and nreverse"
  (is-list.ps+ (let ((lst '(1 2 3)))
                 (reverse lst))
               '(3 2 1))
  (is-list.ps+ (let ((lst '(1 2 3)))
                 (reverse lst)
                 lst)
               '(1 2 3))
  (is-list.ps+ (let ((lst (list 1 2 3)))
                 (nreverse lst))
               '(3 2 1))
  ;; Note: The state of the lst after nreverse is not defined in CL
  )

(subtest
    "Test mapcar"
  (is-list.ps+ (mapcar #'(lambda (x) (* x 2)) '(1 2 3))
               '(2 4 6))
  (is-list.ps+ (let ((lst '(1 2 3)))
                 (mapcar #'(lambda (x) (* x 2)) lst)
                 lst)
               '(1 2 3)))

(subtest
    "Test hash table"
  ;;  (is (ps (make-hash-table)) "{};" :test #'equal)
  (is (ps (gethash key tbl)) "tbl[key];" :test #'equal)
  (is (ps (gethash 'key tbl)) "tbl['KEY'];" :test #'equal)
  (is (ps (gethash 0 tbl)) "tbl[0];" :test #'equal)
  (is (ps (gethash (+ 1 2) tbl)) "tbl[1 + 2];" :test #'equal)
  (prove-in-both (is (let ((tbl (make-hash-table)))
                       (setf (gethash 'x tbl) 100)
                       (gethash 'x tbl))
                     100))
  (subtest
      "Test maphash"
    (prove-in-both (is (let ((tbl (make-hash-table))
                             (sum-key 0)
                             (sum-value 0))
                         (setf (gethash 10 tbl) 100)
                         (setf (gethash 20 tbl) 200)
                         (setf (gethash 30 tbl) 300)
                         (maphash (lambda (k v)
                                    (incf sum-key k)
                                    (incf sum-value v))
                                  tbl)
                         ;;(+ sum-key sum-value)
                         sum-value)
                       600)
                   :prints-js t)))

(subtest
    "Test very simple format"
  ;; The current simple format output completely different string from CL's format.
  ;; So only test in PS.
  (subtest
      "Test format"
    (with-prove-in-ps ()
      (is (format nil "test") "test")
      (is (format nil "test~D~A" 1 "a") "test~D~A; 1; a")))
  (subtest
      "Test format print"
    ;; Because is-print for PS has not been implemented, only make sure that it thorw no error.
    (with-prove-in-both ()
      (ok (not (format t "test"))))))

(subtest
    "Test error"
  ;; Note: Should not use 'with-prove-in-both' in this subtest
  ;;       because it depends on 'error'.
  (prove-in-both (is-error (error 'simple-error)
                           'simple-error))
  (prove-in-both (is-error (let ((x 1))
                             (error "test ~A ~A ~A ~A" 1 "a" x 'test))
                           'simple-error))
  (prove-in-both (is-error (error 'type-error :expected-type 'fixnum :datum "abc")
                           'type-error)))

(subtest
    "Test assert"
  (prove-in-both (ok (progn (assert (= 1 1))
                            t)))
  (prove-in-both (is-error (assert (= 1 2))
                           'simple-error)))

(subtest
    "Test ecase"
  (prove-in-both (is (let ((x 2))
                       (ecase x (1 111) (2 222)))
                     222))
  (prove-in-both  (is-error (let ((x 3))
                              (ecase x (1 111) (2 222)))
                            'error)))

;; --- affect global env --- ;;
(defstruct.ps+ test1 a)
(defstruct.ps+ (test2 (:include test1)) b)
(defstruct.ps+ test3 a)

(subtest
    "Test typep"
  (with-prove-in-both ()
    (ok (typep (make-test1) 'test1))
    (ok (typep (make-test2) 'test1))
    (ok (not (typep (make-test1) 'test3)))
    (ok (let ((type 'test1))
          (typep (make-test1) type)))))

(subtest
    "Test typecase and etypecase"
  (subtest
      "Test typecase"
    (with-prove-in-both ()
      (labels ((test-case (x)
                 (typecase x
                   (test1 1)
                   (test3 2)
                   (t 99))))
        (is (test-case (make-test1)) 1)
        (is (test-case (make-test3)) 2)
        (is (test-case 111) 99))
      (labels ((test-case (x)
                 (typecase x
                   (test1 1)
                   (test3 2))))
        (is (test-case (make-test1)) 1)
        (is (test-case (make-test3)) 2)
        (ok (not (test-case 111))))))
  (subtest
      "Test etypecase"
    (with-prove-in-both ()
      (labels ((test-case (x)
                 (etypecase x
                   (test1 1)
                   (test3 2))))
        (is (test-case (make-test1)) 1)
        (is (test-case (make-test3)) 2)
        (is-error (test-case 111) 'type-error)))))

(subtest
    "Test check-type"
  (with-prove-in-both ()
    (ok (let ((obj (make-test1)))
          (check-type obj test1)
          t))
    (ok (let ((obj (make-test2)))
          (check-type obj test1)
          t))
    (is-error (let ((obj (make-test1)))
                (check-type obj test3))
              'type-error))
  (subtest
      "Test string type"
    (with-prove-in-both ()
      (ok (let ((str "abc"))
            (check-type str string)
            t))
      (is-error (let ((num 12)) (check-type num string))
                'type-error))))

(unintern-all-ps-symbol)

(finalize)
