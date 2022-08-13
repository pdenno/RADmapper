(ns rad-mapper.devl.devl-util
  "Tools for repl-based development"
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.test :refer [is testing]]))

;;; (require '[rad-mapper.devl.devl-util :refer [nicer]])

(defn clean-form [form]
  (let [ns-alia {"rad-mapper.builtins" "bi"
                 "bi"                  "bi"
                 "java.lang.Math"      "Math"}] ; ToDo: Make it more general. (Maybe "java.lang" since j.l.Exception too.)
    (letfn [(ni [form]
              (cond (vector? form) (->> form (map ni) doall vec)
                    (seq? form)    (->> form (map ni) doall)
                    (map? form)    (reduce-kv (fn [m k v] (assoc m k (ni v))) {} form)
                    (symbol? form) (let [nsa (-> form namespace ns-alia)]
                                     (if-let [[_ s] (re-matches #"([a-zA-Z0-9\-]+)__.*" (name form))]
                                       (symbol nsa s)
                                       (->> form name (symbol nsa))))
                    :else form))]
      (ni form))))

(defn nicer
  "Show macroexpand-1 pretty-printed form sans package names.
   Argument is a quoted form"
  [form & {:keys [pprint?] :or {pprint? true}}]
        (cond-> (-> form macroexpand-1 clean-form)
          pprint? pprint))

(defn nicer-
  "Show pretty-printed form sans package names.
   Argument is a quoted form"
  [form & {:keys [pprint?] :or {pprint? true}}]
        (cond-> (-> form clean-form)
          pprint? pprint))

(defn nicer-sym
  "Forms coming back from rew/rewrite* have symbols prefixed by clojure.core
   and other namespaces. On the quoted form in testing, I'd rather not see this.
   This takes away those namespace prefixes."
  [form]
  (clean-form form))

(defn remove-meta
  "Remove metadata from an object and its substructure.
   Changes records to maps too."
  [obj]
  (cond (map? obj) (reduce-kv (fn [m k v] (assoc m k (remove-meta v))) {} obj)
        (vector? obj) (mapv remove-meta obj)
        (seq? obj) (map remove-meta obj)
        :else obj))

(defn rew-rewrite*
  "Return rewrite/rewrite* function."
  []
  (-> (symbol "rad-mapper.rewrite" "rewrite*") resolve))

(defn run
  "Run the exp through whatever steps are specified; defaults to :execute and
   removes any metadata from value returned and its substructure."
  [exp & {:keys [simplify? rewrite? debug? debug-parse? keep-meta?]}]
  (let [execute? (not (or simplify? rewrite?))]
    (cond-> ((rew-rewrite*)
             :ptag/exp exp
             :simplify? simplify?
             :rewrite? rewrite?
             :execute? execute?
             :debug? debug?
             :debug-parse? debug-parse?)
      (not keep-meta?) remove-meta
      true nicer-sym)))

(defn run-rew
  "Run, but with :rewrite? true."
  [exp]
  (-> ((rew-rewrite*) :ptag/exp exp :rewrite? true) remove-meta nicer-sym))

(defn examine [exp]
  (-> ((rew-rewrite*) :ptag/exp exp :rewrite? true) nicer))

(defn examine- [exp]
  (-> ((rew-rewrite*) :ptag/exp exp :rewrite? true) nicer-))

(defmacro run-test
  "Print the test form using testing, run the test."
  [form-string expect & {:keys [simplify? rewrite? keep-meta? _debug? _debug-parse?]}]
  `(testing ~(str "\n(run \"" form-string "\")")
     (is (= ~expect (run ~form-string
                      :simplify? ~simplify?
                      :rewrite? ~rewrite?
                      :keep-meta? ~keep-meta?)))))
