(defpackage ps-experiment/t/package/def-ps-definer
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment/t/package/utils)
  (:export :x))
(in-package :ps-experiment/t/package/def-ps-definer)

(defhoge.ps x 100)

;; --- for test --- ;;

(defun run-test ()
  (with-use-ps-pack (:this)))

(defparameter *expected-output*
  "var psExperiment_t_package_defPsDefiner = (function() {
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

psExperiment_t_package_defPsDefiner._internal.__psMainFunc__();")
