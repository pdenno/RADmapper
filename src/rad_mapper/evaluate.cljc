(ns rad-mapper.evaluate
  "Evaluate a rewritten form."
  (:require
    #?(:clj [clojure.java.io])
    [clojure.pprint               :refer [cl-format pprint]]
    [clojure.spec.alpha           :as s :refer [check-asserts]]
    [rad-mapper.builtins          :as bi]
    [rad-mapper.parse             :as par]
    [rad-mapper.rewrite           :as rew]
    [rad-mapper.util              :as util]
    [sci.core                     :as sci]
    [taoensso.timbre              :as log :refer-macros [info debug log]]))

(defn start
  "NOT USED (yet)."
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

(defn macro?
  "Return a ns-qualified -macro symbol substituting for the argument symbol, where
   such a macro is intended (See the map in the body of this function.)"
  [sym]
  (let [n (name sym)]
    (when (#{"rad-mapper.builtins" "bi"} (namespace sym))
      (get {"init-step"   'rad-mapper.builtins/init-step-m,
            "map-step"    'rad-mapper.builtins/map-step-m,
            "value-step"  'rad-mapper.builtins/value-step-m,
            "primary"     'rad-mapper.builtins/primary-m,
            "thread"      'rad-mapper.builtins/thread-m}
           n))))

(defn rad-form
  "Walk the form replacing the namespace alias 'bi' with 'rad-mapper.builtins' except where the var is an :sci/macro.
      - If it is an :sci/macro and running sci, drop the namespace altogether; sci doesn't like them.
      - If it is an :sci/macro and not running sci, replace it with the corresponding macro.
      - Wrap the form in code for multiple evaluation when it returns a primary fn."
  [form sci?]
  (let [ns-alia {"bi" "rad-mapper.builtins"}]
    (letfn [(ni [form]
              (cond (vector? form) (->> form (map ni) doall vec),
                    (seq? form)    (->> form (map ni) doall),
                    (map? form)    (->> (reduce-kv (fn [m k v] (assoc m k (ni v))) {} form) doall)
                    (symbol? form) (cond (and sci? (macro? form))        (-> form name symbol) ; SCI doesn't like ns-qualified.
                                         (macro? form)                   (macro? form)         ; Not sci; use a real macro named <x>-m.
                                         :else                           (if-let [nsa (-> form namespace ns-alia)]
                                                                           (->> form name (symbol nsa))
                                                                           form)),
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
                     #_#_'info      (sci/copy-var* #'taoensso.timbre/info tns)  ; a macro
                     #_#_'log!      (sci/copy-var* #'taoensso.timbre/log! tns)  ; a macro
                     '-log!     (sci/copy-var* #'taoensso.timbre/-log! tns)
                     '*config*  (sci/copy-var* #'taoensso.timbre/*config* tns)}]
    (sci/init
     {:namespaces {'rad-mapper.builtins  builtins-ns,
                   'taoensso.timbre      timbre-ns,
                   'clojure-pprint       pprint-ns}
      ; ToDo: SCI doesn't seem to want namespaced entries for macros.
      :bindings  {'init-step  rad-mapper.builtins/init-step
                  'map-step   rad-mapper.builtins/map-step
                  'value-step rad-mapper.builtins/value-step
                  'primary    rad-mapper.builtins/primary
                  'thread     rad-mapper.builtins/thread}})))

#?(:clj (def sw (java.io.StringWriter.)))

(defn user-eval
  "Evaluate the argument form."
  [form opts]
  (let [min-level (util/default-min-log-level)
        run-sci?   (or (util/cljs?) (:sci? opts))
        full-form (rad-form form run-sci?)]
    (when (or (:debug-eval? opts) (= min-level :debug))
      (util/config-log :info) ; ToDo: :debug level doesn't work with cljs (including SCI sandbox).
      (log/info (cl-format nil "*****  Running ~S *****" (if run-sci? "SCI" "eval")))
      (-> full-form pretty-form pprint))
    (try
      ;;(s/check-asserts (:check-asserts? opts)) ; ToDo: Investigate why check-asserts? = true is a problem
      (sci/binding [(-> ctx :env deref :namespaces (get 'rad-mapper.builtins) (get '$)) nil]
        (sci/binding [sci/out *out*]
          (if run-sci?
            (sci/eval-form ctx full-form)
            #?(:clj (binding [*ns* (find-ns 'rad-mapper.builtins)]
                      (try (-> full-form str util/read-str eval) ; Once again (see notes), just eval doesn't work!
                           (catch Throwable e
                             (ex-info "Failure in clojure.eval:" {:error e}))))
               :cljs :never-happens))))
      (finally (util/config-log min-level)))))

(defn user-eval-devl
  "Evaluate the argument form. For use in REPL. Form is anything executable in ctx."
  [form]
  (let [min-level (util/default-min-log-level)]
    (util/config-log :info) ; ToDo: :debug level doesn't work with cljs (including SCI sandbox). Use println for now.
    (log/info (cl-format nil "*****  Running SCI *****"))
    (try
      (sci/binding [sci/out *out*]
        (sci/eval-form ctx form))
      (finally (util/config-log min-level)))))

(defn processRM
  "A top-level function for all phases of translation.
   parse-string, rewrite and execute, but with controls for partial evaluation, debugging etc.
   With no opts it returns the parse structure without debug output."
  ([tag str] (processRM tag str {}))
  ([tag str opts]
   (let [rewrite? (or (:rewrite? opts) (:execute? opts))
         ps-atm (atom nil)] ; An atom just to deal with :clj with-open vs :cljs.
     (binding [rew/*debugging?* (:debug? opts)
               par/*debugging?* (:debug-parse? opts)]
       ;; Note that s/check-asserts = true can produce non-JSONata like behavior by means of
       ;; throwing an error where JSONata might just return ** No Match **
       (if (or rew/*debugging?* par/*debugging?*) (s/check-asserts true) (s/check-asserts false))
       #?(:clj  (with-open [rdr (-> str char-array clojure.java.io/reader)]
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
             (throw (ex-info "Error in rewriting:" {:string  (with-out-str (pprint ?r))}))
             ?r))
         (case (:parse-status @ps-atm)
           :premature-end (log/error "Parse ended prematurely")))))))
