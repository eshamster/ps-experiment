(defpackage ps-experiment/t/package
  (:use :cl
        :ps-experiment/base
        :ps-experiment/package
        :parenscript
        :rove
        ;; packages for test
        :ps-experiment/t/package/def-ps-definer
        :ps-experiment/t/package/import
        :ps-experiment/t/package/top-level-form
        :ps-experiment/t/package/refer-unimported-import
        :ps-experiment/t/package/refer-with-no-importing-export
        :ps-experiment/t/package/refer-with-no-importing-import
        :ps-experiment/t/package/type-specifier
        :ps-experiment/t/package/circular-a
        :ps-experiment/t/package/circular-b)
  (:import-from :ps-experiment/package
                :def-ps-definer
                :unintern-all-ps-symbol)
  (:import-from :ps-experiment/t/package/utils
                :defhoge.ps)
  (:import-from :alexandria
                :symbolicate))
(in-package :ps-experiment/t/package)

;; TODO: Assure the ps-experiment environment (*ps-func-store*, *ps-type-store*)
;; is restored after testing.

(defun run-package (package-keyword)
  (let ((pack (find-package (symbolicate :ps-experiment/t/package/
                                         package-keyword))))
    (ok (string= (funcall (intern "RUN-TEST" pack))
                 (symbol-value (intern "*EXPECTED-OUTPUT*" pack))))))

(deftest ps-experiment-package
  (testing "def-ps-definer"
    (run-package :def-ps-definer))
  (testing "importing symbol"
    (run-package :import))
  (testing "top level form"
    (run-package :top-level-form))
  (testing "referring un-imported symbol"
    (run-package :refer-unimported-import))
  (testing "referring with no importing"
    (run-package :refer-with-no-importing-import))
  (testing "type specifier"
    (ok (string= (ps. 'x) "'x';"))
    (ok (string= (ps. 'test-type) "testType;"))
    (ok (string= (ps. '(x test-type y z)) "['x', testType, 'y', 'z'];"))
    (ok (string= (ps. #(x test-type y z)) "['x', testType, 'y', 'z'];")))
  (testing "Error case: circular reference"
    (use-package :ps-experiment/t/package/circular-b :ps-experiment/t/package/circular-a)
    (use-package :ps-experiment/t/package/circular-a :ps-experiment/t/package/circular-b)
    (ok (signals (with-use-ps-pack (:ps-experiment/t/package/circular-a))
                 'simple-error))))

(deftest for-def-top-level-form.ps+
  (ok (expands '(def-top-level-form.ps+ :test-top-level-form
                 (+ 1 2))
               '(progn (def-top-level-form.ps :test-top-level-form (+ 1 2))
                 (progn (+ 1 2))))))

(deftest for-symbol-to-full-js-string
  (ok (string= (symbol-to-full-js-string 'run-package)
               "psExperiment_t_package.runPackage")))
