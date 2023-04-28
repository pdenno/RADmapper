(ns dev.dutil-macros
  "Tools for repl-based exploration of RADmapper code"
  (:require
   [clojure.test :refer [is testing]]
   [dev.dutil-util :refer [run]]
   [promesa.core :as p]
   [rad-mapper.util :as util]))

(defn vec2set
  "Use this so that = testing on data works."
  [obj]
  (cond (map? obj)     (reduce-kv (fn [m k v] (assoc m k (vec2set v))) {} obj)
        (vector? obj)  (->> (map vec2set obj) set)
        :else          obj))

(defn unquote-body
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

(def test-results
  "A map indexed by the RM text run and valued with maps containing :expect and :actual.
      :form is a string which (hopefully!) identifies the test."
  (atom {}))

(def ^:dynamic *run-tests-as-cljs* nil)
(def run-tests-as-cljs? (atom nil))

;;; BTW you can't effectively wrap a macro in #?.
(defmacro run-test
  "Print the test form using testing, run the test."
  [form-string expect & {:keys [rewrite? keep-meta? sets?]}]
  (if true ; *run-tests-as-cljs* @run-tests-as-cljs? ... whatever they don't work!
    `(try
       (swap! test-results conj {~form-string {:expect ~expect}})
       (-> (run ~form-string :rewrite? ~rewrite? :keep-meta? ~keep-meta?)
           (p/then  (fn [res#] (swap! test-results (fn [tr#] (assoc-in tr# [~form-string :actual] res#)))))
           (p/catch (fn [res#] (swap! test-results (fn [tr#] (assoc-in tr# [~form-string :failed] res#))))))
       (catch :default e# (swap! test-results (fn [tr#] (assoc-in tr# [~form-string :error] e#)))))
    `(testing ~(str "\n(run \"" form-string "\")")
       (is (= ~expect
              (cond-> (run ~form-string
                        :rewrite?x ~rewrite?
                        :keep-meta? ~keep-meta?)
                ~sets? vec2set))))))
