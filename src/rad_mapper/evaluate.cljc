(ns rad-mapper.evaluate
  "Evaluate a rewritten form."
  (:require
    #?(:clj [clojure.java.io])
    [clojure.pprint               :refer [cl-format pprint]]
    [clojure.spec.alpha           :as s :refer [check-asserts]]
    [clojure.string               :as str]
    [rad-mapper.builtin           :as bi]
    [rad-mapper.builtin-macros    :as bm]
    [rad-mapper.parse-macros      :as pm]
    [rad-mapper.parse             :as par]
    [rad-mapper.rewrite           :as rew]
    [rad-mapper.rewrite-macros    :as rewm]
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
  (let [ns-alia {"rad-mapper.builtin"  "bi"
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

(def macro-subs
  "When rewriting produces any of the symbols corresponding to the keys of the map,
   AND evaluation with Clojure eval is intended, use the corresponding value of the map."
  {"init-step"   'rad-mapper.builtin-macros/init-step-m,
   "map-step"    'rad-mapper.builtin-macros/map-step-m,
   "value-step"  'rad-mapper.builtin-macros/value-step-m,
   "primary"     'rad-mapper.builtin-macros/primary-m,
   "thread"      'rad-mapper.builtin-macros/thread-m,
   "conditional" 'rad-mapper.builtin-macros/conditional-m})

(defn macro?
  "Return a ns-qualified -macro symbol substituting for the argument symbol created from rewriting.
   This is used where a macro is intended (i.e. when using clojure eval, not SCI, for the evaluation)."
  [sym]
  (let [n (name sym)]
    (when (#{"rad-mapper.builtin" "bi"} (namespace sym))
      (get macro-subs n))))

(defn rad-form
  "Walk the form replacing the namespace alias 'bi' with 'rad-mapper.builtin' except where the var is an :sci/macro.
      - If it is an :sci/macro and running sci, drop the namespace altogether; sci doesn't like them.
      - If it is an :sci/macro and not running sci, replace it with the corresponding macro.
      - Wrap the form in code for multiple evaluation when it returns a primary fn."
  [form sci?]
  (let [ns-alia {"bi" "rad-mapper.builtin"}]
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
      `(do (rad-mapper.builtin/reset-env) (rad-mapper.builtin/again? ~(ni form))))))

(def ctx
  (let [publics        (ns-publics 'rad-mapper.builtin)
        publics-m      (ns-publics 'rad-mapper.builtin-macros)
        bns            (sci/create-ns 'rad-mapper.builtin)
        bns-m          (sci/create-ns 'rad-mapper.builtin-macros)
        pns            (sci/create-ns 'pprint-ns)
        tns            (sci/create-ns 'timbre-ns)
        builtin-ns     (update-vals publics   #(sci/copy-var* % bns))
        builtin-m-ns   (update-vals publics-m #(sci/copy-var* % bns-m))
        pprint-ns      {'cl-format (sci/copy-var* #'clojure.pprint/cl-format pns)}
        timbre-ns      {#_#_'debug     (sci/copy-var* #'taoensso.timbre/debug tns) ; a macro
                        #_#_'info      (sci/copy-var* #'taoensso.timbre/info tns)  ; a macro
                        #_#_'log!      (sci/copy-var* #'taoensso.timbre/log! tns)  ; a macro
                        '-log!     (sci/copy-var* #'taoensso.timbre/-log! tns)
                        '*config*  (sci/copy-var* #'taoensso.timbre/*config* tns)}]
    (sci/init
     {:namespaces {'rad-mapper.builtin               builtin-ns,
                   'rad-mapper.builtin-macros        builtin-m-ns,
                   'taoensso.timbre                  timbre-ns,
                   'clojure-pprint                   pprint-ns}
      ; ToDo: SCI doesn't seem to want namespaced entries for macros.
      :bindings  {'init-step  rad-mapper.builtin/init-step
                  'map-step   rad-mapper.builtin/map-step
                  'value-step rad-mapper.builtin/value-step
                  'primary    rad-mapper.builtin/primary
                  'thread     rad-mapper.builtin/thread}})))

#?(:clj (def sw (java.io.StringWriter.)))

(defn user-eval
  "Evaluate the argument form."
  [full-form opts]
  (let [min-level (util/default-min-log-level)
        run-sci?   (or (util/cljs?) (:sci? opts))]
    (when (or (:debug-eval? opts)  ; I'm suppressing this in run-sci?/cljs. Need to investigate taoensso.timbre/*config* #{"*"} :debug.
              (and (not run-sci?) (= min-level :debug)))
      (util/config-log :info) ; ToDo: :debug level doesn't work with cljs (including SCI sandbox).
      (log/info (cl-format nil "*****  Running ~S *****" (if run-sci? "SCI" "eval")))
      (-> full-form pretty-form pprint))
    (try
      ;;(s/check-asserts (:check-asserts? opts)) ; ToDo: Investigate why check-asserts? = true is a problem
      (sci/binding [(-> ctx :env deref :namespaces (get 'rad-mapper.builtin-macros) (get '$)) nil]
        (sci/binding [sci/out *out*]
          (if run-sci?
            (sci/eval-form ctx full-form)
            #?(:clj (binding [*ns* (find-ns 'rad-mapper.builtin)]
                      (try (-> full-form str util/read-str eval) ; Once again (see notes), just eval doesn't work!
                           (catch Throwable e   ; Is this perhaps because I didn't have the alias for bi in builtin.cljc? No.
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

(declare processRM)

(defn combine-code-and-data
  "Return a new problem for use with processRM consisting of the argument data and code.
   This is typically used with the exerciser, where the user has the opportunity to
   specify data in a separate editor window."
  [code data]
  (let [pdata (as-> data ?d ; Remove last \; to test parsing of data. "(<assign>; <assign>; <assign>)"
                (str/trim ?d)
                (let [last-ix (-> ?d count dec)]
                  (if (= \; (get ?d last-ix))
                    (subs ?d 0 last-ix)
                    ?d)))
        [_ code-body] (re-matches #"(?s)\s*\((.+)\)\s*" code) ; Remove surrounding \( ... \) if any (could just be one expression).
        pcode (or code-body code)]
    (processRM :ptag/jvar-decls (cl-format nil "(~A)" pdata)) ; Will throw if not okay.
    (cl-format nil "(~A; ~A)" pdata pcode)))

(defn processRM
  "A top-level function for all phases of translation.
   parse-string, rewrite, and execute, but with controls for partial evaluation, debugging etc.
   With no opts it returns the parse structure without debug output."
  ([tag str] (processRM tag str {}))
  ([tag str opts]
   (assert (every? #(#{:user-data :rewrite? :executable? :execute? :sci? :debug-eval? :debug-parse? :debug-rewrite?} %)
                     (keys opts)))
   (let [str         (if-let [udata (-> opts :user-data not-empty)]
                       (combine-code-and-data str udata)
                       str)
         rewrite?    (or (:execute? opts) (:executable? opts) (:rewrite? opts))
         executable? (or (:execute? opts) (:executable? opts))
         sci?        (or (:sci? opts) (util/cljs?))
         ps-atm (atom nil)] ; An atom just to deal with :clj with-open vs :cljs.
     (binding [rewm/*debugging?* (:debug-rewrite? opts)
               pm/*debugging?*   (:debug-parse? opts)]
       ;; Note that s/check-asserts = true can produce non-JSONata like behavior by means of
       ;; throwing an error where JSONata might just return ** No Match **
       (if (or rewm/*debugging?* pm/*debugging?*) (s/check-asserts true) (s/check-asserts false))
       #?(:clj  (with-open [rdr (-> str char-array clojure.java.io/reader)]
                  (as-> (par/make-pstate rdr) ?ps
                    (pm/parse tag ?ps)
                    (dissoc ?ps :line-seq) ; dissoc so you can print it.
                    (assoc ?ps :parse-status (if (-> ?ps :tokens empty?) :ok :premature-end))
                    (reset! ps-atm ?ps)))
          :cljs (as-> (par/make-pstate str) ?ps
                  (pm/parse tag ?ps)
                  (assoc ?ps :parse-status (if (-> ?ps :tokens empty?) :ok :premature-end))
                  (reset! ps-atm ?ps)))
       (case (:parse-status @ps-atm)
         :premature-end (log/error "Parse ended prematurely")
         :ok (cond-> {:typ :toplevel :top (:result @ps-atm)}
               (not rewrite?)      (:top)
               rewrite?            (rew/rewrite)
               executable?         (rad-form sci?)
               (:execute? opts)    (user-eval opts)))))))

(def obj {"alice@alice.org"
          {"name" "Alice",
           "aData" "Alice-A-data",
           "bData" "Alice-B-data"},
          "bob@example.com" {"name" "Bob",
                             "aData" "Bob-A-data",
                             "bData" "Bob-B-data"}})

(declare pprint-obj)

;;; Pretty printing. This is unrelated to the above uses of the term pretty-print.
(defn pprint-map
  "Print a map: if it fits within width, print it with keys and values on the same line,
   If it does not fit, print the value on a second line, with additional indentation relative to its key."
  [obj used width]
  (let [strg (atom "")
        wspace (util/nspaces used)
        kv-pairs (reduce-kv (fn [m k v]
                              (conj m {:k (str wspace (pprint-obj k)) ; Assumes key is atomic (i.e. fits on one line)!
                                       :v (pprint-obj v)}))
                            []
                            obj)
        max-key (apply max (->> kv-pairs (map :k) count))   ; We will line them all up with the widest.
        max-val (apply max (->> kv-pairs (map :v) (map (fn [val] (apply max (map count str/split-lines val))))))]
    (if (> (+ max-key max-val) width) ; Too wide, break it to two lines each k/v pair.
      (let [more-indented (str wspace "    ")]
        (swap! strg #(str % "{"))
        (doseq [{:keys [k v]} kv-pairs]
          (swap! strg #(str % " " k ":\n"))
          (doseq [line (str/split-lines v)]
            (swap! strg #(str % more-indented line ",")))))
      ;; Otherwise, it fits. The values are made to line up with the entry with the longest key.
      (doseq [{:keys [k v]} kv-pairs]
        (swap! strg #(str " " k ": "))
        (doseq [line  (str/split-lines v)]
          (let [add (+ (- max-key (count k)) 2)]
            (swap! strg #(str % (util/nspaces add) line ","))))))
    ;; No comma after last one. ToDo: Would be better with str/join (which does the optional separator (e.g. comma) thing?
    (swap! strg #(subs % 0 (-> % count dec)))
    (swap! strg #(str % "}"))))

;;; This tries to print the content within the argument width, but because we assume
;;; that there is a horizontal scrollbar, it doesn't work too hard at it!
(defn pprint-obj
  "Pretty print the argument object."
  [obj & {:keys [indent width] :or {indent 2 width 80}}]
  (let [strg (atom "")
        depth (atom 0)]
    (letfn [(pp [obj]
              (cond (map? obj)     (swap! strg #(str % (pprint-map obj (* @depth indent) width)))
                    ;(vector? obj)  (swap! strg #(str % (pprint-vec obj (* @depth indent) width)))
                    (string? obj)  (swap! strg #(str %  "'" obj "'"))
                    :else          (swap! strg #(str % obj))))]
      (pp obj))))
