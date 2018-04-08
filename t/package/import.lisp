(defpackage ps-experiment/t/package/import
  (:use :cl
        :ps-experiment
        :parenscript
        :ps-experiment/t/package/utils)
  (:import-from :ps-experiment/t/package/export
                :export1))
(in-package :ps-experiment/t/package/import)

(defhoge.ps x 100)

;; --- for test --- ;;

(defun run-test ()
  (with-use-ps-pack (:this)))

(defparameter *expected-output*
  "var psExperiment_t_package_export = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  var export1 = 100;
  var export2 = 200;
  var notExport1 = 300;
  /* --- extern symbols --- */
  return {
    'export1': export1,
    'export2': export2,
    '_internal': {
      'notExport1': notExport1,
    }
  };
})();

var psExperiment_t_package_import = (function() {
  /* --- import symbols --- */
  var export1 = psExperiment_t_package_export.export1;
  /* --- define objects --- */
  var x = 100;
  function __psMainFunc__() {
      return null;
  };
  /* --- extern symbols --- */
  return {
    '_internal': {
      'x': x,
      '__psMainFunc__': __psMainFunc__,
    }
  };
})();

psExperiment_t_package_import._internal.__psMainFunc__();")
