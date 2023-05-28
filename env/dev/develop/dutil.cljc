(ns develop.dutil
  "Tools for repl-based exploration of RADmapper code"
  (:require
   [clojure.pprint :refer [pprint]]
   #?(:clj   [datahike.pull-api      :as dp]
      :cljs  [datascript.pull-api    :as dp])
   [rad-mapper.evaluate              :as ev]
   [taoensso.timbre                  :as log]
   [develop.dutil-util                   :as duu :refer [diag remove-meta clean-form nicer-sym run]]
   #?(:clj [develop.dutil-macros         :refer [run-test]]))
#?(:cljs (:require-macros [dev.dutil-macros])))

;;; From util.cljc
(defn custom-output-fn
  " - I don't want :hostname_ and :timestamp_ in the log output preface text..
    - I don't want any preface text in rad-mapper.parse output."
  ([data] (custom-output-fn nil data))
  ([opts data]
   (if (=  (:?ns-str data) "rad-mapper.parse")
     (apply str (:vargs data)) ; So it can do simple indented call tracing.
     (taoensso.timbre/default-output-fn opts (dissoc data :hostname_ :timestamp_)))))

;;; From util.cljc
(defn config-log
  "Configure Timbre: set reporting levels and specify a custom :output-fn."
  [min-level]
  (if (#{:trace :debug :info :warn :error :fatal :report} min-level)
    (log/set-config!
     (-> log/*config*
         (assoc :output-fn #'custom-output-fn)
         (assoc :min-level [[#{"datahike.*"} :error]
                            [#{"datascript.*"} :error]
                            [#{"*"} min-level]])))
    (log/error "Invalid timbre reporting level:" min-level)))

(defn start
  "Without the start here, (and named shadow-cljs.edn build :modules {:app {:init-fn dev.dutil/start}}
   this file won't be watched!"
  []
  #?(:cljs (js/console.log "dutil.cljc: Loaded console message. Setting log min-level = :debug"))
  (config-log :debug)
  (log/debug "Loaded!"))

(defn ^:dev/after-load reload
  "To demonstrate debugging with browser"
  []
  #?(:cljs (js/console.log "dutil: reloading"))
  (config-log :debug)
  (log/debug "Reloaded!"))

(defn nicer
  "Show macroexpand-1 pretty-printed form sans package names.
   Argument is a quoted form"
  [form & {:keys [pprint?] :or {pprint? true}}]
        (cond-> (-> form duu/clean-form) #_(-> form macroexpand-1 clean-form) ; ToDo: problem with macroexpand-1 in cljs?
          pprint? pprint))

(defn nicer-
  "Show pretty-printed form sans package names.
   Argument is a quoted form"
  [form & {:keys [pprint?] :or {pprint? true}}]
        (cond-> (-> form duu/clean-form)
          pprint? pprint))

(defn run-rew
  "Run, but with :rewrite? true."
  [exp]
  (-> (ev/processRM :ptag/exp exp {:rewrite? true}) duu/remove-meta duu/nicer-sym))

(defn examine [exp]
  (-> (ev/processRM :ptag/exp exp {:rewrite? true})
      (ev/rad-form nil)
      nicer))

(defn examine- [exp]
  (-> (ev/processRM :ptag/exp exp {:rewrite? true}) nicer-))

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
