(defsystem ps-experiment-tests-for-utils
  :version "0.1"
  :author "eshamster"
  :class :package-inferred-system
  :depends-on (:rove
               :parenscript
               :ps-experiment
               "ps-experiment-tests-for-utils/main"
               "ps-experiment-tests-for-utils/success"
               "ps-experiment-tests-for-utils/failure"))
