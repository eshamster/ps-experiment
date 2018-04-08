(defpackage ps-experiment/t/package/refer-unimported-import
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment/t/package/utils)
  (:import-from :ps-experiment/t/package/refer-unimported-export
                :external-sym
                :test-macro))
(in-package :ps-experiment/t/package/refer-unimported-import)

(defhoge.ps sym (test-macro))

;; --- for test --- ;;

(defun run-test ()
  (with-use-ps-pack (:this)))

(defparameter *expected-output*
  "var psExperiment_t_package_referUnimportedExport = (function() {
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

var psExperiment_t_package_referUnimportedImport = (function() {
  /* --- import symbols --- */
  var externalSym = psExperiment_t_package_referUnimportedExport.externalSym;
  /* --- define objects --- */
  var sym = externalSym() + psExperiment_t_package_referUnimportedExport._internal.internalSym();
  function __psMainFunc__() {
      return null;
  };
  /* --- extern symbols --- */
  return {
    '_internal': {
      'sym': sym,
      '__psMainFunc__': __psMainFunc__,
    }
  };
})();

psExperiment_t_package_referUnimportedImport._internal.__psMainFunc__();")
