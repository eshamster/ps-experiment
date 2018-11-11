(defpackage ps-experiment/t/package/top-level-form
  (:use :cl
        :ps-experiment/package
        :parenscript
        :ps-experiment/t/package/utils))
(in-package :ps-experiment/t/package/top-level-form)

(def-top-level-form.ps test-top-level 
  (+ 1 2)
  (* 3 4))

;; --- for test --- ;;

(defun run-test ()
  (with-use-ps-pack (:this)))

(defparameter *expected-output*
  "var psExperiment_t_package_topLevelForm = (function() {
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

psExperiment_t_package_topLevelForm._internal.__psMainFunc__();")
