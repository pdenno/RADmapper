(ns dev.dutil-util
  "Tools for repl-based exploration of RADmapper code"
  (:require
   [rad-mapper.evaluate :as ev]))

(defn remove-meta
  "Remove metadata from an object and its substructure.
   Changes records to maps too."
  [obj]
  (cond (map? obj) (reduce-kv (fn [m k v] (assoc m k (remove-meta v))) {} obj)
        (vector? obj) (mapv remove-meta obj)
        (seq? obj) (map remove-meta obj)
        :else obj))

(defn clean-form
  "Replace some namespaces with aliases"
  [form]
  (let [ns-alia {"rad-mapper.builtin"        "bi"
                 "bi"                        "bi"
                 "rad-mapper.builtin-macros" "bim"
                 "bim"                       "bim"
                 "promesa.core"              "p"
                 "java.lang.Math"     "Math"}] ; ToDo: Make it more general. (Maybe "java.lang" since j.l.Exception too.)
    (letfn [(ni [form]
              (let [m (meta form)]
                (cond (vector? form) (-> (->> form (map ni) doall vec) (with-meta m)),
                      (seq? form)    (-> (->> form (map ni) doall) (with-meta m)),
                      (map? form)    (-> (reduce-kv (fn [m k v] (assoc m k (ni v))) {} form) (with-meta m)),
                      (symbol? form) (-> (let [nsa (-> form namespace ns-alia)]
                                           (if-let [[_ s] (re-matches #"([a-zA-Z0-9\-]+)__.*" (name form))]
                                             (symbol nsa s)
                                             (->> form name (symbol nsa))))
                                         (with-meta m)),
                      :else form)))]
      (ni form))))

(defn nicer-sym
  "Forms coming back from ev/processRM have symbols prefixed by clojure.core
   and other namespaces. On the quoted form in testing, I'd rather not see this.
   This takes away those namespace prefixes."
  [form]
  (clean-form form))

(def diag (atom nil))

(defn run
  "Run the exp through whatever steps are specified; defaults to :execute and
   removes any metadata from value returned and its substructure."
  [exp & {:keys [rewrite? debug-parse? debug-eval? keep-meta? sci?]}]
  (let [execute? (not rewrite?)]
    (cond->> (ev/processRM
              :ptag/exp exp
              {:rewrite? rewrite?
               :execute? execute?
               :sci?     sci?
               :debug-parse? debug-parse?
               :debug-eval? debug-eval?})
      true (reset! diag)
      (not keep-meta?) remove-meta
      true nicer-sym)))
