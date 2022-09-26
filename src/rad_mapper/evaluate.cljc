(ns rad-mapper.evaluate
  "Evaluate a rewritten form."
  (:require
   [clojure.pprint               :refer [cl-format pprint]]
   [clojure.spec.alpha           :as s :refer [check-asserts]]
   #?(:cljs [cljs.js :as cljs])
   [rad-mapper.devl.devl-util    :as devl :refer [run examine]] ; ToDo: Temporary
   [rad-mapper.builtins          :as bi]
   [rad-mapper.util              :as util]
   [sci.core                     :as sci]
   [taoensso.timbre              :as log]))

(defn rad-string
  "Walk form replacing namespace alias 'bi' with 'rad-mapper.builtins'.
   Wrap the form in code for multiple evaluation when it returns a primary fn.
   Returns that string."
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
      (cl-format nil "(do (rad-mapper.builtins/reset-env) (rad-mapper.builtins/again? ~S))" (ni form)))))

(def ctx
  (let [publics     (ns-publics 'rad-mapper.builtins)
        bns         (sci/create-ns 'rad-mapper.builtins)
        pns         (sci/create-ns 'pprint-ns)
        tns         (sci/create-ns 'timbre-ns)
        builtins-ns (update-vals publics #(sci/copy-var* % bns))
        pprint-ns   {'cl-format (sci/copy-var* #'clojure.pprint/cl-format pns)}
        timbre-ns   {'debug     (sci/copy-var* #'taoensso.timbre/debug tns)
                     'log!      (sci/copy-var* #'taoensso.timbre/log! tns)
                     '-log!     (sci/copy-var* #'taoensso.timbre/-log! tns)
                     '*config*  (sci/copy-var* #'taoensso.timbre/*config* tns)
                     }]
    (sci/init {:namespaces {'rad-mapper.builtins  builtins-ns,
                            'taoensso.timbre      timbre-ns,
                            'clojure-pprint       pprint-ns}})))

;;;(def ctx nil)

;;; (sci/eval-string* ctx "(rad-mapper.builtins/+ 1 1)")
(def sw (java.io.StringWriter.))

(defn user-eval
  "Evaluate the argument form."
  [form & {:keys [debug? check-asserts? debug-eval? sci?]
           :or   {check-asserts? false}}] ; ToDo: debug? temporarily true.
  (let [sci? true                      ; ToDo: temporary
        debug-eval? true                ; ToDo: temporary
        min-level (util/default-min-log-level)
        full-form (rad-string form)]
    (try
      (when debug-eval?
        (util/config-log :debug)
        (log/debug (format "*****  Running %s *****" (if sci? "SCI" "eval"))))
      (when debug-eval? (-> full-form util/read-str devl/clean-form pprint))
      (s/check-asserts check-asserts?) ; ToDo: Investigate why check-asserts? = true is a problem
      (binding [*ns* (find-ns 'user)]  ; ToDo: This doesn't matter for SCI, right?
        (sci/binding [sci/out sw]
          (if sci?
            (sci/eval-string* ctx full-form)
            (-> full-form util/read-str eval))))
      (finally (util/config-log min-level)))))
