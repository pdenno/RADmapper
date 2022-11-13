(ns dev.dutil-macros
  "Tools for repl-based exploration of RADmapper code"
  (:require
   [clojure.test :refer [is testing]]
   [dev.dutil-util :refer [diag remove-meta clean-form nicer-sym run]]
   [rad-mapper.evaluate :as ev]))

(defmacro run-test
  "Print the test form using testing, run the test."
  [form-string expect & {:keys [rewrite? keep-meta? _debug? _debug-parse?]}]
  `(testing ~(str "\n(run \"" form-string "\")")
     (is (= ~expect (run ~form-string
                      :rewrite? ~rewrite?
                      :keep-meta? ~keep-meta?)))))
