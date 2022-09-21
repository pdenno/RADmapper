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
  [#'bi/!= #'bi/$ #'bi/$$ #'bi/$abs #'bi/$addSchema #'bi/$append #'bi/$assert #'bi/$average #'bi/$base64decode
   #'bi/$base64encode #'bi/$boolean #'bi/$ceil #'bi/$contains #'bi/$count #'bi/$decodeUrl #'bi/$decodeUrlComponent #'bi/deref$
   #'bi/$distinct #'bi/$each #'bi/$encodeUrl #'bi/$encodeUrlComponent #'bi/$error #'bi/$eval #'bi/$exists #'bi/$filter
   #'bi/$floor #'bi/$formatBase #'bi/$formatInteger #'bi/$formatNumber #'bi/$fromMillis #'bi/$join #'bi/$keys #'bi/$length
   #'bi/$lookup #'bi/$lowercase #'bi/$map #'bi/$match #'bi/$max #'bi/$merge #'bi/$millis #'bi/$min #'bi/$not #'bi/$now
   #'bi/$number #'bi/$pad #'bi/$parseInteger #'bi/$power #'bi/$random #'bi/$read #'bi/$readSpreadsheet #'bi/$reduce
   #'bi/$replace #'bi/$reverse #'bi/$round #'bi/$schemaFor #'bi/$shuffle #'bi/$sift #'bi/$single #'bi/$sort #'bi/$split
   #'bi/$spread #'bi/$sqrt #'bi/$string #'bi/$substring #'bi/$substringAfter #'bi/$substringBefore #'bi/$sum #'bi/$toMillis
   #'bi/$trim #'bi/$type #'bi/$uppercase #'bi/$zip #'bi/* #'bi/+ #'bi/- #'bi// #'bi/< #'bi/<= #'bi/= #'bi/> #'bi/>=
   #'bi/aref #'bi/body&bset-ai-maps #'bi/cmap #'bi/container? #'bi/containerize #'bi/containerize? #'bi/date-fmt-xpath2java
   #'bi/entity-qvars #'bi/express #'bi/express-sub #'bi/filter-step #'bi/flatten-except-json
   #'bi/format-time #'bi/found? #'bi/get-scoped #'bi/get-step #'bi/handle-builtin #'bi/higher-order-query-fn
   #'bi/immediate-query-fn #'bi/jflatten #'bi/match-regex #'bi/path #'bi/query
   #'bi/query-fn-aux #'bi/qvar? #'bi/reduce-express #'bi/reduce-typical #'bi/reset-env #'bi/rewrite-qform
   #'bi/rewrite-sheet-for-mapper #'bi/run-steps #'bi/sbind-body #'bi/sbind-eq #'bi/sbind-path-to! #'bi/sbind? #'bi/set-context!
   #'bi/singlize #'bi/substitute-in-form #'bi/sym-bi-access #'bi/translate-part #'bi/transpose-sheet #'bi/update-db])

(def bi-vars-clj
  "These are just used in the clj implementation."
  #?(:clj  [#'bi/$read]
     :cljs []))

(def fns (sci/create-ns 'builtins-ns nil))
(def builtins-ns (reduce (fn [m v] (assoc m (-> v symbol name symbol) (sci/copy-var* v fns)))
                         {}
                         (into (into bi-vars bi-macs) bi-vars-clj)))

(def ctx (sci/init {:namespaces {'bi                  builtins-ns, ; ToDo: I suppose this could be cleaner!
                                 'rad-mapper.builtins builtins-ns}}))

;;; (sci/eval-string* ctx "(rad-mapper.builtins/+ 1 1)")
(def diag (atom nil))
(def diag-form (atom nil))

(defn user-eval
  "Do clojure eval in namespace app.model.mm-user.
   If the sexp has unresolvable symbols, catch them and return :unresolved-symbol."
  [form & {:keys [debug? check-asserts? use-sci?] :or {check-asserts? false use-sci? true debug? false}}] ; ToDo: debug? temporarily true.
  (when debug? (println "eval form: " form))
  (s/check-asserts check-asserts?) ; ToDo: Investigate why check-asserts? = true is a problem
  (binding [*ns* (find-ns 'user)]
    (sci/binding [sci/out (if debug? *out* sci/out)]
      (try
        (bi/reset-env) ; ToDo: Why no longer (eval form) in CLJ?
        (let [res #?(:clj  (if use-sci?
                             (sci/eval-string* ctx (str form))
                             (-> form str util/read-str eval))
                     :cljs (sci/eval-string* ctx (-> form rad-nspaces str)))]
          (if (and (fn? res) (= :bi/primary (-> res meta :bi/step-type)))
            (bi/jflatten (res))
            (bi/jflatten res)))
        (catch #?(:clj Exception :cljs :default) e
          (reset! diag {:e e :form form})
          (log/error "\nError evaluating form: (See @rad-mapper.evaluate/diag)"
                     #?(:clj (-> e .getMessage str) :cljs e)
                     "\nform:"
                     form))))))
