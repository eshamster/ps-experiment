(defpackage ps-experiment/t/package/refer-with-no-importing-import
  (:use :cl
        :ps-experiment/package
        :parenscript
        :ps-experiment/t/package/utils))
(in-package :ps-experiment/t/package/refer-with-no-importing-import)

;; --- for test --- ;;

(defun run-test ()
  (with-use-ps-pack (:this)
    (+ 200 ps-experiment/t/package/refer-with-no-importing-export::internal-sym)))

(defparameter *expected-output*
  "var psExperiment_t_package_referWithNoImportingExport = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  var internalSym = 100;
  /* --- extern symbols --- */
  return {
    '_internal': {
      'internalSym': internalSym,
    }
  };
})();

var psExperiment_t_package_referWithNoImportingImport = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  function __psMainFunc__() {
      return 200 + psExperiment_t_package_referWithNoImportingExport._internal.internalSym;
  };
  /* --- extern symbols --- */
  return {
    '_internal': {
      '__psMainFunc__': __psMainFunc__,
    }
  };
})();

psExperiment_t_package_referWithNoImportingImport._internal.__psMainFunc__();")
