(ns dev.dutil-macros
  "Tools for repl-based exploration of RADmapper code"
  (:require
   [clojure.test :refer [is testing]]
   [dev.dutil-util :refer [run]]))

(defn vec2set
  "Use this so that = testing on data works."
  [obj]
  (cond (map? obj)     (reduce-kv (fn [m k v] (assoc m k (vec2set v))) {} obj)
        (vector? obj)  (->> (map vec2set obj) set)
        :else          obj))

(defn unquote
  "Walk through the body replacing (quote <qvar>) with <qvar>.
   Rationale: In most situations we want qvars to be rewritten as quoted symbols.
   An exception is their use in the :where of a datalog query. There may be more usages."
  [form]
  (letfn [(unq [obj]
            (cond (map? obj)                   (reduce-kv (fn [m k v] (assoc m (unq k) (unq v))) {} obj),
                  (vector? obj)                (mapv unq obj),
                  (and (seq? obj)
                       (= 'quote (first obj))) (if (== 2 (count obj)) (second obj) (rest obj)),
                  :else obj))]
    (unq form)))

(defmacro run-test
  "Print the test form using testing, run the test."
  [form-string expect & {:keys [rewrite? keep-meta? sets?]}]
  `(testing ~(str "\n(run \"" form-string "\")")
     (is (= ~expect
            (cond-> (run ~form-string
                      :rewrite? ~rewrite?
                      :keep-meta? ~keep-meta?)
              ~sets? vec2set)))))
