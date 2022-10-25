(ns rad-mapper.test-macros
  "Macros for rewrite-test.cljc" ; ToDo: Why doesn't it work like it works in parse.cljc?
  (:require
   [clojure.test :refer [is testing]]
   [dev.dutil :as dev]
   [rad-mapper.rewrite  :as rew])) ; Not clear why shadow wants this!

(defmacro run-test
  "Use this to expand dev/run-test with :rewrite? true."
  [form-string expect & {:keys [keep-meta?]}]
    `(testing ~(str "\n(run \"" form-string "\")")
       (is (= ~expect (dev/run ~form-string
                        :rewrite? true
                        :keep-meta? ~keep-meta?)))))

(defmacro run
  "Used with REPL"
  [form-string]
  `(dev/run ~form-string :rewrite? true))
