(in-package :cl-user)
(defpackage ps-experiment/t/base
  (:use :cl
        :ps-experiment
        :parenscript
        :rove)
  (:import-from :ps-experiment/base
                :replace-dot-in-tree))
(in-package :ps-experiment/t/base)

(enable-ps-experiment-syntax)

(deftest for-macro--
  (ok (equal (ps (-- a (b c) d (e)))
             "a.b(c).d.e();")))

(deftest for-reader-macro-j#
  (ok (eq '#j.TEST.AbCd# '-t-e-s-t.-ab-cd))
  (ok (signals (read-from-string "'#jWithoutDotAfterJ#")
               'simple-error)))

(deftest for-replace-dot-in-tree
  (ok (= (replace-dot-in-tree 2) 2))
  (ok (eq (replace-dot-in-tree 'test) 'test))
  (ok (equal (replace-dot-in-tree 'a.b) '(@ a b)))
  (ok (equal (replace-dot-in-tree '(a (1 :b c) "abc" test))
             '(a (1 :b c) "abc" test)))
  (ok (equal (replace-dot-in-tree '(a.b (test bc.d.efg 123) b.c !!d.e))
             '((@ a b) (test (@ bc d efg) 123) (@ b c) d.e)))
  (ok (replace-dot-in-tree '(a.b (#:b d.ef 12) c #:defg))))

(deftest for-macro-ps.
  (ok (equal (macroexpand-1 '(ps. (with-slots (a b) obj
                                    (setf a.x 100)
                                    (setf (@ a x) 200)
                                    (setf b 300))
                              (setf obj.a.x 100)))
             `(let ((ps-experiment/base:*original-package* ,*package*))
                (macroexpand '(ps (with-slots (a b)
                                      obj
                                    (setf (@ a x) 100)
                                    (setf (@ a x) 200)
                                    (setf b 300))
                               (setf (@ obj a x) 100)))))))

(deftest for-defmacro.ps-macro
  (ok (expands `(defmacro.ps test (a b)
                  (+ a.x b.y))
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (defpsmacro test (a b) (+ (@ a x) (@ b y))))))
  (ok (equal (macroexpand-1 '(ps. (defmacro test (a b)
                                    (+ a.x b.y))))
             `(let ((ps-experiment/base:*original-package* ,*package*))
                (macroexpand '(ps (defmacro test (a b) (+ (@ a x) (@ b y))))))))
  (ok (expands `(defmacro.ps+ test (a b)
                  (+ a.x b.y))
               `(progn (defmacro.ps test (a b) (+ a.x b.y))
                       (defmacro test (a b) (+ a.x b.y))))))
