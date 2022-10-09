(ns rad-mapper.evaluate
  "Evaluate a rewritten form."
  (:require
   [clojure.pprint               :refer [cl-format pprint]]
   [clojure.spec.alpha           :as s :refer [check-asserts]]
   #?(:cljs [cljs.js :as cljs]) ; ToDo: Not needed?
   [failjure.core                :as fj]
   [rad-mapper.parse             :as par]
   [rad-mapper.rewrite           :as rew]
   [rad-mapper.util              :as util]
   [sci.core                     :as sci]
   [taoensso.timbre              :as log]))

(defn start
  "NOT USED."
  []
  #?(:cljs (js/console.log "evaluate.cljc: Loaded console message. Setting log min-level = :debug"))
  (util/config-log :debug)
  (log/info "Loaded!"))

(defn pretty-form
  "Replace some namespaces with aliases for diagnostic legability."
  [form]
  (let [ns-alia {"rad-mapper.builtins" "bi"
                 "bi"                  "bi"
                 "java.lang.Math"      "Math"}] ; ToDo: Make it more general. (Maybe "java.lang" since j.l.Exception too.)
    (letfn [(ni [form]
              (let [m (meta form)]
                (cond (vector? form) (-> (->> form (map ni) doall vec) (with-meta m)),
                      (seq? form)    (-> (->> form (map ni) doall) (with-meta m)),
                      (map? form)    (-> (reduce-kv (fn [m k v] (assoc m k (ni v))) {} form) (with-meta m)),
                      (symbol? form) (-> (if-let [ns (-> form namespace ns-alia)] (symbol ns (name form)) (symbol (name form)))
                                         (with-meta m)),
                      :else form)))]
      (ni form))))

(defn expand-sci-macros
  [form]
  (cond (vector? form) (->> form (map expand-sci-macros) doall vec),
        (seq? form)    (let [func (when (-> form first symbol?)
                                    #?(:cljs (try (-> form first eval) (catch :default _ nil))
                                       :clj  (when-let [v (-> form first resolve)]
                                               (try (var-get v) (catch Throwable _ nil)))))]
                         (if (and (fn? func) (-> func meta :sci/macro))
                           (-> (apply func (into [nil nil] (rest form))) expand-sci-macros)
                           (map expand-sci-macros form)))
        (map? form)    (reduce-kv (fn [m k v] (assoc m k (expand-sci-macros v))) {} form)
        :else          form))

(defn rad-form
  "Walk form replacing the namespace alias 'bi' with 'rad-mapper.builtins' and expanding macros for SCI.
   Wrap the form in code for multiple evaluation when it returns a primary fn."
  [form]
  (let [ns-alia {"bi" "rad-mapper.builtins"}]
    (letfn [(ni [form]
              (cond (vector? form) (->> form (map ni) doall vec),
                    (seq? form)    (->> form (map ni) doall),
                    (map? form)    (->> (reduce-kv (fn [m k v] (assoc m k (ni v))) {} form) doall)
                    (symbol? form) (if-let [nsa (-> form namespace ns-alia)]
                                     (->> form name (symbol nsa))
                                     form)
                    :else form))]
      `(do (rad-mapper.builtins/reset-env) (rad-mapper.builtins/again? ~(ni form))))))


(def ctx
  (let [publics     (ns-publics 'rad-mapper.builtins)
        bns         (sci/create-ns 'rad-mapper.builtins)
        pns         (sci/create-ns 'pprint-ns)
        tns         (sci/create-ns 'timbre-ns)
        builtins-ns (update-vals publics #(sci/copy-var* % bns))
        pprint-ns   {'cl-format (sci/copy-var* #'clojure.pprint/cl-format pns)}
        timbre-ns   {#_#_'debug     (sci/copy-var* #'taoensso.timbre/debug tns) ; a macro
                     #_#_'log!      (sci/copy-var* #'taoensso.timbre/log! tns)  ; a macro
                     '-log!     (sci/copy-var* #'taoensso.timbre/-log! tns)
                     '*config*  (sci/copy-var* #'taoensso.timbre/*config* tns)
                     }]
    (sci/init {:namespaces {'rad-mapper.builtins  builtins-ns,
                            'taoensso.timbre      timbre-ns,
                            'clojure-pprint       pprint-ns}})))

#?(:clj (def sw (java.io.StringWriter.)))


(defn user-eval
  "Evaluate the argument form."
  [form opts]
  (let [min-level (util/default-min-log-level)
        full-form (-> form rad-form expand-sci-macros)
        opts      (-> opts
                      #_(assoc :debug-eval? false)
                      (assoc :sci? (or (util/cljs?) (:sci? opts))))]
    (try
      (when (:debug-eval? opts)
        (util/config-log :debug)
        (log/debug (cl-format nil "*****  Running %s *****" (if (:sci? opts) "SCI" "eval")))
        (-> full-form pretty-form pprint))
      ;(s/check-asserts (:check-asserts? opts)) ; ToDo: Investigate why check-asserts? = true is a problem
      (sci/binding [sci/out sw]
        (sci/binding [(-> ctx :env deref :namespaces (get 'rad-mapper.builtins) (get '$)) nil]
          (if (:sci? opts)
            (sci/eval-string* ctx full-form)
            (binding [*ns* (find-ns 'rad-mapper.builtins)] (eval full-form)))))
      (finally (util/config-log min-level)))))


(defn processRM
  "A top-level function for all phases of translation.
   parse-string, rewrite and execute, but with controls for partial evaluation, debugging etc.
   With no opts it parses without debug output."
  ([tag str] (processRM tag str {}))
  ([tag str opts]
   (let [rewrite? (or (:rewrite? opts) (:execute? opts))
         ps-atm (atom nil)] ; An atom just to deal with :clj with-open vs :cljs.
     (binding [rew/*debugging?* (:debug? opts)
               par/*debugging?* (:debug-parse? opts)]
       (if (or rew/*debugging?* par/*debugging?*) (s/check-asserts true) (s/check-asserts false))
       #?(:clj (with-open [rdr (-> str char-array clojure.java.io/reader)]
                 (as-> (par/make-pstate rdr) ?ps
                   (par/parse tag ?ps)
                   (dissoc ?ps :line-seq) ; dissoc so you can print it.
                   (assoc ?ps :parse-status (if (-> ?ps :tokens empty?) :ok :premature-end))
                   (reset! ps-atm ?ps)))
          :cljs (as-> (par/make-pstate str) ?ps
                  (par/parse tag ?ps)
                  (assoc ?ps :parse-status (if (-> ?ps :tokens empty?) :ok :premature-end))
                  (reset! ps-atm ?ps)))
       (if (= :ok (:parse-status @ps-atm))
         (as-> (:result @ps-atm) ?r
           (if rewrite?
             (->> (if (:skip-top? opts) ?r {:typ :toplevel :top ?r}) rew/rewrite)
             ?r)
           (if (:execute? opts) (user-eval ?r opts) ?r)
           (if (:rewrite-error? ?r)
             (fj/fail "Error in rewriting: %s" (with-out-str (pprint ?r)))
             ?r))
         (case (:parse-status @ps-atm)
           :premature-end (log/error "Parse ended prematurely")))))))
