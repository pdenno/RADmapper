(ns rad-mapper.devl.devl-util
  "Tools for repl-based development"
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.test :refer [is testing]]
   #?(:clj   [datahike.pull-api      :as dp]
      :cljs  [datascript.pull-api    :as dp])))

;;; (require '[rad-mapper.devl.devl-util :refer [nicer]])

(defn clean-form
  "Replace some namespaces with aliases"
  [form]
  (let [ns-alia {"rad-mapper.builtins" "bi"
                 "bi"                  "bi"
                 "java.lang.Math"      "Math"}] ; ToDo: Make it more general. (Maybe "java.lang" since j.l.Exception too.)
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
  "Forms coming back from rew/processRM have symbols prefixed by clojure.core
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

(defn rew-processRM
  "Return rewrite/processRM function."
  []
  (-> (symbol "rad-mapper.rewrite" "processRM") resolve))

(def diag (atom nil))

(defn run
  "Run the exp through whatever steps are specified; defaults to :execute and
   removes any metadata from value returned and its substructure."
  [exp & {:keys [rewrite? debug? debug-parse? keep-meta?]}]
  (let [execute? (not rewrite?)]
    (cond->> ((rew-processRM)
              :ptag/exp exp
              :rewrite? rewrite?
              :execute? execute?
              :debug? debug?
              :debug-parse? debug-parse?)
      true (reset! diag)
      (not keep-meta?) remove-meta
      true nicer-sym)))

(defn run-rew
  "Run, but with :rewrite? true."
  [exp]
  (-> ((rew-processRM) :ptag/exp exp :rewrite? true) remove-meta nicer-sym))

(defn examine [exp]
  (-> ((rew-processRM) :ptag/exp exp :rewrite? true) nicer))

(defn examine- [exp]
  (-> ((rew-processRM) :ptag/exp exp :rewrite? true) nicer-))

(defmacro run-test
  "Print the test form using testing, run the test."
  [form-string expect & {:keys [rewrite? keep-meta? _debug? _debug-parse?]}]
  `(testing ~(str "\n(run \"" form-string "\")")
     (is (= ~expect (run ~form-string
                      :rewrite? ~rewrite?
                      :keep-meta? ~keep-meta?)))))

;;; Adapted from owl-db-tools/resolve-obj, which uses :resource/iri as keys exclusively.
(defn resolve-tree
  "Resolve :db/id in the argument map."
  [m conn & {:keys [keep-db-ids?] :or {keep-db-ids? true}}]
  (let [expanded? (atom #{})]
    (letfn [(subobj [x]
              (cond
                ;; for object references...
                (and (map? x) (contains? x :db/id) (== (count x) 1))
                (if (@expanded? (:db/id x))
                  {:reference-to x}
                  (do (swap! expanded? conj (:db/id x))
                      (subobj (dp/pull conn '[*] (:db/id x))))),

                ;; for an ordinary entity...
                (map? x)
                (reduce-kv
                 (fn [m k v] (if (and (= k :db/id) (not keep-db-ids?)) m (assoc m k (subobj v))))
                 {} x),

                ;; for collections of data...
                (vector? x) (mapv subobj x),

                ;; for a data element
                :else x))]
      (cond-> (reduce-kv (fn [m k v] (assoc m k (subobj v))) {} m)
        (not keep-db-ids?) (dissoc :db/id)))))
