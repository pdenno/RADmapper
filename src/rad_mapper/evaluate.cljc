(ns rad-mapper.evaluate
  "Evaluate a rewritten form."
  (:require
   [clojure.pprint               :refer [cl-format]]
   [clojure.spec.alpha :as s :refer [check-asserts]]
   #?(:cljs [cljs.js :as cljs])
   [rad-mapper.builtins :as bi]
   #?(:clj [rad-mapper.util      :as util])
   [sci.core                     :as sci]
   [taoensso.timbre              :as log]))

(def bi-macs "'macs' is a misnomer, since they are rewritten to functions for SCI. They are treated like bi-vars below."
  [#'bi/with-context #'bi/thread  #'bi/value-step #'bi/primary #'bi/init-step #'bi/map-step])

(def bi-vars
  [#'bi/!= #'bi/$ #'bi/$$ #'bi/$abs #'bi/$addSchema #'bi/$append #'bi/$assert #'bi/$average #_#'bi/$base64decode
   #_#'bi/$base64encode #'bi/$boolean #'bi/$ceil #'bi/$contains #'bi/$count #'bi/$decodeUrl #'bi/$decodeUrlComponent #'bi/deref$
   #'bi/$distinct #'bi/$each #'bi/$encodeUrl #'bi/$encodeUrlComponent #'bi/$error #_#'bi/$eval #'bi/$exists #'bi/$filter
   #'bi/$floor #'bi/$formatBase #_#'bi/$formatInteger #_#'bi/$formatNumber #_#'bi/$fromMillis #'bi/$join #'bi/$keys #'bi/$length
   #'bi/$lookup #'bi/$lowercase #'bi/$map #'bi/$match #'bi/$max #'bi/$merge #_#'bi/$millis #'bi/$min #'bi/$not #_#'bi/$now
   #'bi/$number #'bi/$pad #'bi/$parseInteger #'bi/$power #'bi/$random #_#'bi/$read #_#'bi/$readSpreadsheet #'bi/$reduce
   #'bi/$replace #'bi/$reverse #_#'bi/$round #'bi/$schemaFor #'bi/$shuffle #'bi/$sift #'bi/$single #'bi/$sort #'bi/$split
   #'bi/$spread #'bi/$sqrt #'bi/$string #'bi/$substring #'bi/$substringAfter #'bi/$substringBefore #'bi/$sum #_#'bi/$toMillis
   #'bi/$trim #'bi/$type #'bi/$uppercase #'bi/$zip #'bi/* #'bi/+ #'bi/- #'bi// #'bi/< #'bi/<= #'bi/= #'bi/> #'bi/>=
   #'bi/aref #'bi/body&bset-ai-maps #'bi/cmap #'bi/container? #'bi/containerize #'bi/containerize? #'bi/date-fmt-xpath2java
   #'bi/entity-qvars #'bi/express #'bi/express-sub #'bi/filter-step #'bi/flatten-except-json
   #_#'bi/format-time #'bi/found? #'bi/get-scoped #'bi/get-step #'bi/handle-builtin #'bi/higher-order-query-fn
   #'bi/immediate-query-fn #'bi/jflatten #'bi/match-regex #'bi/path #'bi/query
   #'bi/query-fn-aux #'bi/qvar? #'bi/reduce-express #'bi/reduce-typical #'bi/reset-env #'bi/rewrite-qform
   #'bi/rewrite-sheet-for-mapper #'bi/run-steps #'bi/sbind-body #'bi/sbind-eq #'bi/sbind-path-to! #'bi/sbind? #'bi/set-context!
   #'bi/singlize #'bi/substitute-in-form #'bi/sym-bi-access #'bi/translate-part #'bi/transpose-sheet #'bi/update-db])

(def fns (sci/create-ns 'builtins-ns nil))
(def builtins-ns (reduce (fn [m v] (assoc m (-> v symbol name symbol) (sci/copy-var* v fns)))
                         {}
                         (into bi-vars bi-macs)))

(def ctx (sci/init {:namespaces {'bi                  builtins-ns, ; ToDo: I suppose this could be cleaner!
                                 'rad-mapper.builtins builtins-ns}}))

(defn rad-string
  "Walk form replacing namespace 'rad-mapper.builtins' with 'bi'."
  [form]
  (let [ns-alia {"rad-mapper.builtins" "bi"
                 "bi"                  "bi"}]
    (letfn [(ni [form]
              (cond (vector? form) (->> form (map ni) doall vec),
                    (seq? form)    (->> form (map ni) doall),
                    (map? form)    (->> (reduce-kv (fn [m k v] (assoc m k (ni v))) {} form) doall)
                    (symbol? form) (let [nsa (-> form namespace ns-alia)]
                                     (if-let [[_ s] (re-matches #"([a-zA-Z0-9\-]+)__.*" (name form))]
                                       (symbol nsa s)
                                       (->> form name (symbol nsa)))),
                    :else form))]
      (cl-format nil "~S" (ni form))))) ; ToDo: doall is not enough. str is not enough!

;;; (sci/eval-string* ctx "(rad-mapper.builtins/+ 1 1)")
;;; (sci/eval-string* ctx "(rad-mapper.builtins/run-steps (rad-mapper.builtins/init-step [[1 2 3] 4]) (rad-mapper.builtins/map-step (rad-mapper.builtins/deref$)) (rad-mapper.builtins/filter-step (fn [_x1] (rad-mapper.builtins/with-context _x1 1))))")

(def diag (atom nil))
(def diag-form (atom nil))

(defn user-eval
  "Do clojure eval in namespace app.model.mm-user.
   If the sexp has unresolvable symbols, catch them and return :unresolved-symbol."
  [form & {:keys [verbose? check-asserts? use-sci?] :or {check-asserts? false use-sci? true}}]
  (when verbose? (println "eval form: " form))
  (s/check-asserts check-asserts?) ; ToDo: Investigate why check-asserts? = true is a problem
  (binding [*ns* (find-ns 'user)]
    (try
      (bi/reset-env) ; ToDo: Why no longer (eval form) in CLJ?
      (let [res #?(:clj  (if use-sci?
                           (sci/eval-string* ctx (reset! diag-form (rad-string form)))
                           (-> form str util/read-str eval))
                   :cljs (sci/eval-string* ctx (-> form rad-nspaces str)))]
         (if (and (fn? res) (= :bi/primary (-> res meta :bi/step-type)))
           (bi/jflatten (res))
           (bi/jflatten res)))
      (catch #?(:clj Exception :cljs :default) e
        (reset! diag {:e e :form form})
        (log/error "\nError evaluating form: (See @diag)"
                   #?(:clj (-> e .getMessage str) :cljs e)
                   "\nform:"
                   form)))))
