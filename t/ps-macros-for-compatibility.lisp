(in-package :cl-user)
(defpackage ps-experiment/t/ps-macros-for-compatibility
  (:use :cl
        :ps-experiment
        :ps-experiment/t/test-utils
        :parenscript
        :rove)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :ps-experiment/package
                :unintern-all-ps-symbol))
(in-package :ps-experiment/t/ps-macros-for-compatibility)

(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

;; --- utilities --- ;;

(defun is-list (lst1 lst2)
  (equalp lst1 lst2))

(defun.ps-only is-list (lst1 lst2)
  (unless (= (length lst1) (length lst2))
    (return-from is-list nil))
  (dotimes (i (length lst1))
    ;; FIXME: The case where types of var1 and var2 is different  will be failed.
    (let ((var1 (aref lst1 i))
          (var2 (aref lst2 i)))
      (unless (if (instanceof var1 -array)
                  (is-list var1 var2)
                  (= var1 var2))
        (return-from is-list nil))))
  t)

;; test for utilities
(deftest.ps+ for-is-list
  (ok (is-list '(1 2 3) (list 1 2 3)))
  (ok (is-list '(1 (10 20) 2) (list 1 (list 10 20) 2)))
  (ng (is-list '(1 2 3) (list 1 2)))
  (ng (is-list '(1 2 3) (list 1 3 2))))

;; --- test --- ;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro.ps+ prove-setf (cxr base-lst value expected)
    `(is-list (let ((lst ,base-lst))
                (setf (,cxr lst) ,value)
                lst)
              ,expected)))

(deftest.ps+ for-list*
  (ok (is-list (list* 1 2 '(3 4))
               '(1 2 3 4)))
  (let ((x 1)
        (y 2)
        (z '(3 4)))
    (ok (is-list (list* x y z)
               '(1 2 3 4))))
  (ok (= (list* 1) 1)))

(deftest.ps+ for-cxxr
  "Test c[ad]{1-2}r (Limitation: cd[ad]*r cannot be used for setting)"
  (testing "car"
    (ok (= (car '(1 2 3)) 1))
    (ok (prove-setf car (list 1 2 3) 4 '(4 2 3)))
    (ok (null (car '()))))
  (testing "cdr"
    (ok (is-list (cdr '(1 2 3)) '(2 3)))
    (ok (is-list (cdr '(1)) '()))
    (ok (is-list (cdr '()) '())))
  (testing "caar"
    (ok (= (caar '((1) 2 3)) 1))
    (ok (prove-setf caar (list (list 1) 2 3) 4 '((4) 2 3))))
  (testing "cadr"
    (ok (= (cadr '(1 2 3)) 2))
    (ok (prove-setf cadr (list 1 2 3) 4 '(1 4 3))))
  (testing "cdar"
    (ok (is-list (cdar '((1 4 9) 2 3)) '(4 9))))
  (testing "cddr"
    (ok (is-list (cddr '(1 2 3)) '(3)))))

(deftest.ps+ for-nth
  (ok (= (nth 2 '(1 2 3 4)) 3))
  (ok (null (nth 2 '(1 2)))))

(deftest.ps+ for-listp_atom
  (testing "listp"
    (ok (listp nil))
    (ok (listp '()))
    (ok (listp '(1 2 3)))
    (ok (not (listp 12)))
    (ok (not (listp 'abc))))
  (testing "atom"
    (ok (atom nil))
    (ok (atom '()))
    (ok (not (atom '(1 2 3))))
    (ok (atom 12))
    (ok (atom 'abc))))

(deftest.ps+ for-subseq
  (ok (is-list (subseq '(1 2 3) 1)
               '(2 3)))
  (ok (is-list (subseq '(1 2 3 4) 2 3)
               '(3)))
  (ok (is-list (let* ((x '(1 2 3 4))
                      (y (subseq x 2 3)))
                 (list x y))
               '((1 2 3 4) (3)))))

(deftest.ps+ for-push_pushnew
  (testing "push"
    (ok (is-list (let ((x '()))
                   (push 1 x)
                   (push 2 x))
                 '(2 1))))
  (testing "pushnew"
    (ok (is-list (let ((x '(1 2)))
                   (pushnew 3 x)
                   (pushnew 2 x))
                 '(3 1 2)))
    (ok (is-list (let ((x '(1 2)))
                   (pushnew 12 x :test (lambda (a b) (= (/ a 6) b)))
                   (pushnew 6 x :test (lambda (a b) (= a (/ b 6)))))
                 '(6 1 2)))))

(deftest.ps+ for-every
  (ok (every (lambda (x) (> x 2)) '(3 4 5)))
  (ok (not (every (lambda (x) (> x 2)) '(2 3 4)))))

(deftest.ps+ for-some 
  (ok (some (lambda (x) (< x 2)) '(2 1 3)))
  (ok (not (some (lambda (x) (< x 2)) '(2 3 4)))))

(deftest.ps+ for-find 
  (ok (= (find 3 '(1 2 3 4)) 3))
  (ok (not (find 5 '(1 2 3 4)))))

(deftest.ps+ for-find-if
  (ok (= (find-if (lambda (x) (> x 2)) '(2 1 3 4)) 3))
  (ok (not (find-if (lambda (x) (> x 10)) '(2 1 3 4)))))

(deftest.ps+ for-getf
  (testing "get"
    (ok (= (getf (getf '(:x 1 :y (:a 3 :b 10 :c 4)) :y) :b) 10))
    (ok (= (getf (getf '(:x 1 :y (:a 3 :b 10 :c 4)) :y)
                 :not-exist
                 999)
           999))
    ;; test index 0
    (ok (= (getf (list :a 1 :b 2) :a) 1)))
  (testing "set"
    (let ((lst (list :a 1 :b 2)))
      (ok (= (setf (getf lst :a) 100) 100))
      (ok (= (getf lst :a) 100))
      (ok (= (setf (getf lst :not-exist) 200) 200))
      (ok (= (getf lst :not-exist) 200)))))

(deftest.ps+ for-remf
  (let ((lst (list :x 1 :y 2 :z 3)))
    (ok (remf lst :y))
    (ok (not (getf lst :y)))
    (ok (= (getf lst :z) 3))
    (ok (not (remf lst :not-exist)))
    ;; test index 0
    (ok (remf lst :x))))

(deftest.ps+ for-reduce
  (ok (= (reduce #'(lambda (x y) (+ x y)) '(1 2 3 4))
         10))
  (ok (is-list (reduce #'(lambda (x y) (list (+ (car x) (car y))
                                             (+ (cadr x) (cadr y))))
                       '((1 2) (3 4) (5 6)))
               '(9 12))))

(deftest.ps+ for-remove
  (ok (is-list (let ((lst '(1 2 3 2 4)))
                 (remove 2 lst))
               '(1 3 4)))
  (ok (is-list (let ((lst '(1 2 3 2 4)))
                 (remove 2 lst)
                 lst)
               '(1 2 3 2 4))))

(deftest.ps+ for-remove-if
  (ok (is-list (let ((lst '(1 2 3 4)))
                 (remove-if (lambda (x) (> x 2)) lst))
               '(1 2)))
  (ok (is-list (let ((lst '(1 2 3 4)))
                 (remove-if (lambda (x) (> x 2)) lst)
                 lst)
               '(1 2 3 4))))

(deftest.ps+ for-remove-if-not
  (ok (is-list (let ((lst '(1 2 3 4)))
                 (remove-if-not (lambda (x) (> x 2)) lst))
               '(3 4)))
  (ok (is-list (let ((lst '(1 2 3 4)))
                 (remove-if-not (lambda (x) (> x 2)) lst)
                 lst)
               '(1 2 3 4))))

(deftest.ps+ for-reverse_nreverse
  (ok (is-list (let ((lst '(1 2 3)))
                 (reverse lst))
               '(3 2 1)))
  (ok (is-list (let ((lst '(1 2 3)))
                 (reverse lst)
                 lst)
               '(1 2 3)))
  (ok (is-list (let ((lst (list 1 2 3)))
                 (nreverse lst))
               '(3 2 1)))
  ;; Note: The state of the lst after nreverse is not defined in CL
  )

(deftest.ps+ for-mapcar
  (ok (is-list (mapcar #'(lambda (x) (* x 2)) '(1 2 3))
               '(2 4 6)))
  (ok (is-list (let ((lst '(1 2 3)))
                 (mapcar #'(lambda (x) (* x 2)) lst)
                 lst)
               '(1 2 3))))

(deftest for-hash-table1
  ;;  (ok (= (ps (make-hash-table)) "{};"))
  (ok (string= (ps (gethash key tbl)) "tbl[key];"))
  (ok (string= (ps (gethash 'key tbl)) "tbl['KEY'];"))
  (ok (string= (ps (gethash 0 tbl)) "tbl[0];"))
  (ok (string= (ps (gethash (+ 1 2) tbl)) "tbl[1 + 2];")))

(deftest.ps+ for-hash-table2
  (testing "gethash"
    (ok (= (let ((tbl (make-hash-table)))
             (setf (gethash 'x tbl) 100)
             (gethash 'x tbl))
           100)))
  (testing "remhash"
    (let ((hash (make-hash-table)))
      (setf (gethash :a hash) 100)
      (setf (gethash :b hash) 200)
      (ok (= (gethash :a hash) 100))
      (ok (= (gethash :b hash) 200))
      (ok (remhash :a hash))
      (ok (not (gethash :a hash)))
      (ok (= (gethash :b hash) 200))
      (ok (not (remhash :a hash))))
    (testing "use variable as key"
      (let ((hash (make-hash-table)))
        (setf (gethash :a hash) 100)
        (ok (= (gethash :a hash) 100))
        (let ((key :a))
          (ok (remhash key hash)))
        (ok (not (gethash :a hash))))))
  (testing "maphash"
    (ok (= (let ((tbl (make-hash-table))
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
           600))))

(deftest.ps+ for-very-simple-format
  ;; The current simple format output completely different string from CL's format.
  ;; So only test in PS.
  (testing "string by format"
    (ok (string= (format nil "test") "test"))
    (ok (format nil "test~D~A" 1 "a")))
  (testing "print by format"
    ;; Because prints for PS has not been implemented, only make sure that it thorw no error.
    (ok (not (format t "test")))))

(deftest.ps+ for-error
  (ok (signals (error 'simple-error)
               'simple-error))
  (ok (signals (let ((x 1))
                 (error "test ~A ~A ~A ~A" 1 "a" x 'test))
               'simple-error))
  (ok (signals (error 'type-error :expected-type 'fixnum :datum "abc")
               'type-error)))

;; Note: console.warn is not implemented in cl-js. So only check expanded JavaScript code.
(deftest for-warn
  (ok (string= (ps:ps (warn 'simple-warn))
               "console.warn('simpleWarn');"))
  (ok (string= (ps:ps (warn "test: ~A" 100))
               "console.warn('test: 100');"))
  (ok (string= (ps:ps (warn 'x 100 200))
               "console.warn('\\'X: (100 200)');")))

(deftest.ps+ for-assert
  (ok (progn (assert (= 1 1))
             t))
  (ok (signals (assert (= 1 2))
               'simple-error)))

(deftest.ps+ for-ecase 
  (ok (= (let ((x 2))
           (ecase x (1 111) (2 222)))
         222))
  (ok (Signals (let ((x 3))
                 (ecase x (1 111) (2 222)))
               'error)))

(defstruct.ps+ test1 a)
(defstruct.ps+ (test2 (:include test1)) b)
(defstruct.ps+ test3 a)

(deftest.ps+ for-typep
  (ok (typep (make-test1) 'test1))
  (ok (typep (make-test2) 'test1))
  (ng (typep (make-test1) 'test3))
  (ok (let ((type 'test1))
        (typep (make-test1) type))))


(defun.ps+ test-typecase-with-t (x)
  (typecase x
    (test1 1)
    (test3 2)
    (t 99)))

(defun.ps+ test-typecase-without-t (x)
  (typecase x
    (test1 1)
    (test3 2)))

(defun.ps+ test-etypecase (x)
  (etypecase x
    (test1 1)
    (test3 2)))

(deftest.ps+ for-typecase_etypecase
  (testing "typecase"
    (labels ()
      (ok (= (test-typecase-with-t (make-test1)) 1))
      (ok (= (test-typecase-with-t (make-test3)) 2))
      (ok (= (test-typecase-with-t 111) 99)))
    (labels ()
      (ok (= (test-typecase-without-t (make-test1)) 1))
      (ok (= (test-typecase-without-t (make-test3)) 2))
      (ok (not (test-typecase-without-t 111)))))
  (testing "etypecase"
    (labels ()
      (ok (= (test-etypecase (make-test1)) 1))
      (ok (= (test-etypecase (make-test3)) 2))
      (ok (signals (test-etypecase 111) 'type-error)))))

(deftest.ps+ for-check-type
  (testing "some types"
    (ok (let ((obj (make-test1)))
          (check-type obj test1)
          t))
    (ok (let ((obj (make-test2)))
          (check-type obj test1)
          t))
    (ok (signals (let ((obj (make-test1)))
                   (check-type obj test3))
                 'type-error)))
  (testing "string type"
    (ok (let ((str "abc"))
          (check-type str string)
          t))
    (ok (signals (let ((num 12)) (check-type num string))
                 'type-error))))
