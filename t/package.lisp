(in-package :cl-user)
(defpackage ps-experiment-test.package
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment-test.test-utils
        :prove)
  (:import-from :ps-experiment.package
                :def-ps-definer
                :unintern-all-ps-symbol)
  (:import-from :alexandria
                :symbolicate))
(in-package :ps-experiment-test.package)

;; TODO: Assure the ps-experiment environment (*ps-func-store*, *ps-type-store*)
;; is restored after testing.

(import 'ps-experiment.package::find-ps-func)

(plan 6)

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
(export 'x)

(subtest
    "Test def-ps-definer"
  (ok (find-ps-func "X"))
  (is (with-use-ps-pack (:this))
      "var psExperimentTest_package = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  var x = 100;
  function __psMainFunc__() {
      return null;
  };
  /* --- extern symbols --- */
  return {
    'x': x,
    '_internal': {
      '__psMainFunc__': __psMainFunc__,
    }
  };
})();

psExperimentTest_package._internal.__psMainFunc__();"
      :test #'equal))

(export 'defhoge.ps)

(def-test-package test.package/external) ; Also try slash in package name
(in-package :test.package/external)
(import 'ps-experiment-test.package:defhoge.ps)
(import 'ps-experiment-test.package:x)

(defhoge.ps a 10)

(in-package :ps-experiment-test.package)

(subtest
    "Test print un-exported symbol"
  (is (with-use-ps-pack (:this :test.package/external))
      "var psExperimentTest_package = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  var x = 100;
  function __psMainFunc__() {
      return null;
  };
  /* --- extern symbols --- */
  return {
    'x': x,
    '_internal': {
      '__psMainFunc__': __psMainFunc__,
    }
  };
})();

var test_package_external = (function() {
  /* --- import symbols --- */
  var x = psExperimentTest_package.x;
  /* --- define objects --- */
  var a = 10;
  /* --- extern symbols --- */
  return {
    '_internal': {
      'a': a,
    }
  };
})();

psExperimentTest_package._internal.__psMainFunc__();"
      :test #'equal))

;; --- affect global env --- ;;
(unintern-all-ps-symbol)

(def-top-level-form.ps test-top-level 
  (+ 1 2)
  (* 3 4))

(subtest
    "Test def-top-level-form.ps"
  (is (with-use-ps-pack (:this))
      "var psExperimentTest_package = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  1 + 2;
  3 * 4;
  function __psMainFunc__() {
      return null;
  };
  /* --- extern symbols --- */
  return {
    '_internal': {
      '__psMainFunc__': __psMainFunc__,
    }
  };
})();

psExperimentTest_package._internal.__psMainFunc__();"))

;; --- affect global env --- ;;
(in-package :ps-experiment-test.package)
(unintern-all-ps-symbol)

(def-ps-definer defhoge.ps (name value) ()
  `(defvar ,name ,value))

(def-test-package test.package.un-imported-symbol-a)
(def-test-package test.package.un-imported-symbol-b)

(in-package :test.package.un-imported-symbol-a)
(ps-experiment-test.package::defhoge.ps external-sym 100)
(ps-experiment-test.package::defhoge.ps internal-sym 100)
(defpsmacro test-macro ()
  `(+ (external-sym) (internal-sym)))

(export 'external-sym)
(export 'test-macro)

(in-package :test.package.un-imported-symbol-b)
(import 'test.package.un-imported-symbol-a:test-macro)
(ps-experiment-test.package::defhoge.ps sym (test-macro))

(in-package :ps-experiment-test.package)
(subtest
    "Test referring un-imported symbol via imported macro"
  (is (with-use-ps-pack (:test.package.un-imported-symbol-b))
      "var test_package_unImportedSymbolA = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  var externalSym = 100;
  var internalSym = 100;
  /* --- extern symbols --- */
  return {
    'externalSym': externalSym,
    '_internal': {
      'internalSym': internalSym,
    }
  };
})();

var test_package_unImportedSymbolB = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  var sym = test_package_unImportedSymbolA.externalSym() + test_package_unImportedSymbolA._internal.internalSym();
  /* --- extern symbols --- */
  return {
    '_internal': {
      'sym': sym,
    }
  };
})();

psExperimentTest_package._internal.__psMainFunc__();"))

;; --- affect global env --- ;;
(in-package :ps-experiment-test.package)
(unintern-all-ps-symbol)

(def-test-package test.package.type-specifier)
(in-package :test.package.type-specifier)

(ps-experiment.package:register-ps-type 'test-type)
(export 'test-type)

(in-package :ps-experiment-test.package)
(import 'test.package.type-specifier:test-type)
(subtest
    "Test quote for type-specifier is removed"
  (is (ps. 'x) "'x';")
  (is (ps. 'test-type) "testType;")
  (is (ps. '(x test-type y z)) "['x', testType, 'y', 'z'];")
  (is (ps. #(x test-type y z)) "['x', testType, 'y', 'z'];"))

;; --- affect global env --- ;;
(in-package :ps-experiment-test.package)
(unintern-all-ps-symbol)

(def-ps-definer defhoge.ps (name value) ()
  `(defvar ,name ,value))

(def-test-package test.package.loop-a)
(def-test-package test.package.loop-b)

(use-package :test.package.loop-a :test.package.loop-b)
(use-package :test.package.loop-b :test.package.loop-a)

(in-package :test.package.loop-a)
(ps-experiment-test.package::defhoge.ps xa 100)
(export 'xa)
(in-package :test.package.loop-b)
(ps-experiment-test.package::defhoge.ps xb 200)
(export 'xb)
(in-package :ps-experiment-test.package)

(subtest
    "Test circular reference between packages"
  (is-error (with-use-ps-pack (:test.package.loop-a))
            'simple-error))

;; --- affect global env --- ;;
(unintern-all-ps-symbol)

(finalize)
