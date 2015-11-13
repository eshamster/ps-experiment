(in-package :cl-user)
(defpackage ps-experiment-test.utils.common
  (:use :cl
        :ps-experiment.utils.common
        :parenscript
        :prove))
(in-package :ps-experiment-test.utils.common)

(plan 1)

(subtest
    "Test replace-dot-in-tree"
  (is (replace-dot-in-tree '(a (1 :b c) "abc" test))
      '(a (1 :b c) "abc" test)
      :test #'equal)
  (is (replace-dot-in-tree '(a.b (test bc.d.efg 123) b.c !!d.e))
      '((@ a b) (test (@ bc d efg) 123) (@ b c) d.e)
      :test #'equal)
  (ok (replace-dot-in-tree '(a.b (#:b d.ef 12) c #:defg))))

(finalize)
