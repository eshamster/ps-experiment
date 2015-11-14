(in-package :cl-user)
(defpackage ps-experiment-test.utils
  (:use :cl
        :ps-experiment.utils
        :parenscript
        :prove))
(in-package :ps-experiment-test.utils)

(plan 3)

(subtest
    "Test #j. reader macro"
  (is '#j.TEST.AbCd# '-t-e-s-t.-ab-cd))

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
    "Test defmacro.ps macro"
  (is-expand (defmacro.ps test (a b)
               (+ a.x b.y))
             (defmacro+ps test (a b)
               (+ (@ a x) (@ b y)))))

(finalize)
