(in-package :cl-user)
(defpackage ps-experiment-test.package
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment-test.test-utils
        :prove)
  (:import-from :ps-experiment.package
                :find-ps-symbol
                :def-ps-definer
                :unintern-all-ps-symbol)
  (:import-from :alexandria
                :symbolicate))
(in-package :ps-experiment-test.package)

(plan 4)

(defmacro def-test-package (name)
  `(defpackage ,name
     (:use :cl
           :ps-experiment.package
           :parenscript)))

;; --- affect global env --- ;;
(unintern-all-ps-symbol)

(def-ps-definer defhoge.ps (name value) ()
  `(defvar ,name ,value))

(defhoge.ps x 100)

(subtest
    "Test def-ps-definer"
  (ok (find-ps-symbol "X"))
  (is (with-use-ps-pack (:this))
      "var x = 100;
"
      :test #'equal))

(export 'defhoge.ps)

(def-test-package test.package.external)
(in-package :test.package.external)
(import 'ps-experiment-test.package:defhoge.ps)

(defhoge.ps a 10)

(in-package :ps-experiment-test.package)

(subtest
    "Test print un-exported symbol"
  (is (with-use-ps-pack (:this :test.package.external))
      "var x = 100;
var a = 10;
"
      :test #'equal))

;; --- affect global env --- ;;
(unintern-all-ps-symbol)

(def-top-level-form.ps test-top-level 
  (+ 1 2)
  (* 3 4))

(subtest
    "Test def-top-level-form.ps"
  (is (with-use-ps-pack (:this))
      "1 + 2;
3 * 4;
"))

;; --- affect global env --- ;;
(unintern-all-ps-symbol)

(def-test-package test.package.loop-a)
(def-test-package test.package.loop-b)

(use-package :test.package.loop-a :test.package.loop-b)
(use-package :test.package.loop-b :test.package.loop-a)

(in-package :test.package.loop-a)
(def-top-level-form.ps test-loop-a
  (+ 3 4))
(in-package :test.package.loop-b)
(def-top-level-form.ps test-loop-b
  (* 5 8))
(in-package :ps-experiment-test.package)

(subtest
    "Test circular reference between packages"
  (is-error (with-use-ps-pack (:test.package.loop-a))
            'simple-error))

(unintern-all-ps-symbol)

(finalize)
