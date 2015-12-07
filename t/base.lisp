(in-package :cl-user)
(defpackage ps-experiment-test.utils.common
  (:use :cl
        :ps-experiment
        :parenscript
        :prove)
  (:import-from :ps-experiment.base
                :replace-dot-in-tree))
(in-package :ps-experiment-test.utils.common)

(plan 4)

(enable-ps-experiment-syntax)

(subtest
    "Test #j. reader macro"
  (is '#j.TEST.AbCd# '-t-e-s-t.-ab-cd))

(subtest
    "Test replace-dot-in-tree"
  (is (replace-dot-in-tree 2) 2)
  (is (replace-dot-in-tree 'test) 'test)
  (is (replace-dot-in-tree 'a.b) '(@ a b)
      :test #'equal)
  (is (replace-dot-in-tree '(a (1 :b c) "abc" test))
      '(a (1 :b c) "abc" test)
      :test #'equal)
  (is (replace-dot-in-tree '(a.b (test bc.d.efg 123) b.c !!d.e))
      '((@ a b) (test (@ bc d efg) 123) (@ b c) d.e)
      :test #'equal)
  (ok (replace-dot-in-tree '(a.b (#:b d.ef 12) c #:defg))))

(subtest
    "Test ps. macro"
  (is-expand (ps. (with-slots (a b) obj
                    (setf a.x 100)
                    (setf (@ a x) 200)
                    (setf b 300))
                  (setf obj.a.x 100))
             (ps (with-slots (a b) obj
                   (setf (@ a x) 100)
                   (setf (@ a x) 200)
                   (setf b 300))
                 (setf (@ obj a x) 100))))

(subtest
    "Test defmacro.ps[+] macro"
  (is-expand (defmacro.ps test (a b)
               (+ a.x b.y))
             (ps. (defmacro test (a b)
                    (+ a.x b.y))))
  (is-expand (ps. (defmacro test (a b)
                    (+ a.x b.y)))
             (ps (defmacro test (a b)
                   (+ (@ a x) (@ b y)))))
  (is-expand (defmacro.ps+ test (a b)
               (+ a.x b.y))
             (progn (defmacro.ps test (a b) (+ a.x b.y))
                    (defmacro test (a b) (+ a.x b.y)))))

(finalize)
