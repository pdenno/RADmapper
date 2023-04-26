(ns rad-mapper.evaluate
  "Evaluate a rewritten form."
  (:require
    #?(:clj [clojure.java.io])
    [clojure.pprint               :refer [cl-format pprint]]
    [clojure.spec.alpha           :as s :refer [check-asserts]]
    [clojure.string               :as str]
    [mount.core                   :refer [defstate]]
    [promesa.core                 :as p]
    [rad-mapper.builtin           :as bi]
    [rad-mapper.builtin-macros    :as bm]
    [rad-mapper.parse-macros      :as pm]
    [rad-mapper.parse             :as par]
    [rad-mapper.rewrite           :as rew]
    [rad-mapper.rewrite-macros    :as rewm]
    [rad-mapper.util              :as util :refer [nspaces]]
    [sci.core                     :as sci]
    #?(:cljs [sci.configs.funcool.promesa  :as scip])
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
                        '*config*  (sci/copy-var* #'taoensso.timbre/*config* tns)}
        nspaces        {'rad-mapper.builtin               builtin-ns,
                        'rad-mapper.builtin-macros        builtin-m-ns,
                        'taoensso.timbre                  timbre-ns,
                        #_#_'clojure-pprint               pprint-ns}]
    (sci/init
     {:namespaces #?(:clj nspaces :cljs (merge nspaces scip/namespaces))  ; for promesa.core and promesa.protocols.
      ; ToDo: SCI doesn't seem to want namespaced entries for macros. <=== See https://github.com/babashka/sci.configs/blob/main/src/sci/configs/funcool/promesa.cljs
      :bindings  {'init-step  rad-mapper.builtin/init-step
                  'map-step   rad-mapper.builtin/map-step
                  'value-step rad-mapper.builtin/value-step
                  'primary    rad-mapper.builtin/primary
                  'thread     rad-mapper.builtin/thread}})))

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

(declare pprint-obj)

(defn hello [] :hello)

(defn processRM
  "A top-level function for all phases of translation.
   parse-string, rewrite, and execute, but with controls to quit before doing all of these, debugging etc.
   With no opts it returns the parse structure without debug output."
  ([tag str] (processRM tag str {}))
  ([tag str opts]
   (assert (every? #(#{:user-data :rewrite? :executable? :execute? :sci? :debug-eval?
                       :debug-parse? :debug-rewrite? :pprint?} %)
                   (keys opts)))
   (let [str         (if-let [udata (-> opts :user-data not-empty)]
                       (combine-code-and-data str udata)
                       str)
         execute?    (or (:pprint? opts) (:execute? opts))
         rewrite?    (or (:pprint? opts) (:execute? opts) (:executable? opts) (:rewrite? opts))
         executable? (or (:pprint? opts) (:execute? opts) (:executable? opts))
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
               execute?            (user-eval opts)
               (:pprint? opts)     (pprint-obj)))))))

(defn indent-additional-lines
  "Indent every line but the first by the amount indicated by indent."
  [s indent]
  (let [spaces (nspaces indent)
        [one & others] (str/split-lines s)]
    (cl-format nil "~A~{~%~A~}"
               one
               (map #(str spaces %) others))))

(def name-order
  "The values here are listed in the order in which they should appear."
  {"schema"      ["name" "shortname" "type" "sdo" "spec" "version" "subversion" "topic" "pathname" "content"]
   "model"       ["name" "elementDef" "elementRef" "complexType" "sequence" "union" "extension"]
   "element"     ["name" "ref" "id" "sequence" "complexType" "simpleType"]
   "complexType" ["name" "id"]
   "codeList"    ["name" "id" "terms" "restriction" "union"]})

(def ns-order
  "This is the order that things should be presented when they have different namespaces.
   after these nspaces, it is alphabetical (so that 'xsd' is towards the end."
  ["schema" "element" "model" "codeList" "complexType" "component"  "cct" "attribute" "has"])

;;;==========  Pretty printing. This is unrelated to the above uses of the term pretty-print. ====================
;;; Maybe forget this and try json.stringify? / (json/pprint obj)
;;; The functions for pprint each return a string that may have line breaks.
;;; The string starts with no spaces, the spaces to place after a line-break are passed in as "ident"
;;; The map and vector functions add indentation to the values, which may themselves be multi-line.
(defn sort-map
  "Sort maps so that
    - '*/name' comes first, xsd/* comes last, */documentation comes next to last.
    - schema/* stuff is ordered by schema/name, schema/type ... with schema/content last."
  [m]
  (letfn [(compare-names [ns x y]
            (if-let [v (get name-order ns)]
              (let [xi (as-> (.indexOf v x) ?i (if (neg? ?i) nil ?i))
                    yi (as-> (.indexOf v y) ?i (if (neg? ?i) nil ?i))]
                (cond (and xi yi)  (if (< xi yi) -1 +1)
                      xi           -1
                      yi           +1
                      :else        (compare x y)))
              (compare x y)))
          (compare-namespaces [x y]
              (let [xi (as-> (.indexOf ns-order x) ?i (if (neg? ?i) nil ?i))
                    yi (as-> (.indexOf ns-order y) ?i (if (neg? ?i) nil ?i))]
                (cond (and xi yi)  (if (< xi yi) -1 +1)
                      xi           -1
                      yi           +1
                      :else        (compare x y))))
          (compar [x y]
            (let [nsp-x (namespace x)
                  nsp-y (namespace y)
                  name-x (name x)
                  name-y (name y)]
              (cond (and nsp-x nsp-y)           (if (= nsp-x nsp-y)
                                                  (compare-names nsp-x name-x name-y)
                                                  (compare-namespaces nsp-x nsp-y))
                    nsp-x                       -1
                    nsp-y                       +1
                    :else                       (compare name-x name-y))))]
    (into (sorted-map-by compar) m)))

(defn sort-obj
  "Recursively sort the maps in the given object.
   Everything else, of course, is left alone."
  [obj]
  (cond (map? obj)      (->> obj (reduce-kv (fn [m k v] (assoc m k (sort-obj v))) {}) sort-map)
        (vector? obj)   (mapv sort-obj obj)
        :else           obj))

(defn pprint-map
  "Print a map:
     (1) If it all fits within width minus indent, print it that way.
     (2) If the longest key/value it fits within width minus indent, print it with keys and values on the same line,
     (3) If it does not fit, print the value on a second line, with additional indentation relative to its key.
  - ident is the column in which this object can start printing.
  - width is column beyond which print should not appear."
  [obj indent width]
  (if (empty? obj)
    "{}"
    (let [kv-pairs (reduce-kv (fn [m k v] ; Here we get the 'dense' size; later calculate a new rest-start
                                (let [kk (pprint-obj k :width width)
                                      vv (pprint-obj v :width width)]
                                  (conj m {:k kk :v vv
                                           :k-len (count kk)
                                           :v-len (->> vv str/split-lines (map count) (apply max))})))
                              []
                              obj)
          max-key (apply max (map :k-len kv-pairs)) ; We will line them all up with the widest.
          max-val (apply max (map :v-len kv-pairs))]
      (if (>= (- width indent) (+ (apply + (map :k-len kv-pairs)) (apply + (map :v-len kv-pairs)) (* (count kv-pairs) 3)))
        ;; (1) The map fits on one line.
        (cl-format nil "{~{~A: ~A~^, ~}}" (interleave (map :k kv-pairs) (map :v kv-pairs)))
        ;; It doesn't fit on one line.
        (let [indent-spaces (nspaces indent)
              key-strs (into (-> kv-pairs first :k str vector)
                             (map #(str indent-spaces (:k %) (nspaces (- max-key (:k-len %)))) (rest kv-pairs)))
              ;; Values may have line breaks, each line but the first needs the indent plus the max-key
              val-strs (mapv #(indent-additional-lines (:v %) (+ indent 2)) kv-pairs)]
          (if (<= (+ max-key max-val) (- width indent))
            ;; (2) It fits. The values are made to line up with the entry with the longest key.
            (cl-format nil "{~{~A: ~A~^,~% ~}}" ; one space after ~% because starts with a '{'
                       (interleave key-strs val-strs))
            ;; (3) Too wide; break into two lines for each k/v pair.
            ;;     Now all the values need to be indented
            ;;     The cl-format indents the values a few spaces relative to their key.
            (cl-format nil "{~{~A:~%  ~A~^,~%  ~}}"
                       (interleave
                        key-strs
                        (map #(str indent-spaces %) val-strs)))))))))

(defn pprint-vec
  "Return a string representing vector:
    If it all fits on one line within width minus indent, the returned string contains now newlines.
    Else each element is on a new-line. (In fact, the elements themselves can run multiple lines.
    The first element no indentation, other are preceded by indent spaces."
  [obj indent width]
  (let [query-form? (-> obj meta :query-form?)
        elem-objs (reduce (fn [res elem]
                            (let [s (pprint-obj elem :width width :indent indent)]
                              (conj res {:val s :len (->> s str/split-lines (map count) (apply max))})))
                          [] obj)]
    (if (>= (- width indent) (+ (apply + (map :len elem-objs)) (* (count elem-objs) 2))) ; All on one line?
      (if query-form?
        (cl-format nil "[~{~A~^ ~}]"  (map :val elem-objs))
        (cl-format nil "[~{~A~^, ~}]" (map :val elem-objs)))
      ;; Not printable on one line, so every line but the first need the indent.
      ;; And the values might be multi-line; every line but the first needs the indent.
      (let [spaces (nspaces indent)
            elems (->> elem-objs (map :val) (map #(indent-additional-lines % indent)))
            val-strs (into (-> elems first vector)
                           (->> elems rest (map #(str spaces %))))]
        (if query-form?
          (cl-format nil "[~{~A~^~% ~}]" val-strs)
          (cl-format nil "[~{~A~^,~% ~}]" val-strs))))))

(def print-width "Number of characters that can be comfortably fit on a line.
                  This is an atom so that it can be set by other libraries."
  (atom 80))

(defn pprint-obj
  "Pretty print the argument object.
   (This tries to print the content within the argument width, but because we assume
   that there is a horizontal scrollbar, it doesn't work too hard at it!)
     - width: the character length of the total area we have to work with.
     - indent: a number of space characters per nesting level,
     - depth: the number of nesting levels.
     - start: a number of characters to indent owing to where this object starts because of key for which it is a value."
  [obj & {:keys [indent width] :or {indent 0 width @print-width}}]
  (let [strg (atom "")]
    (letfn [(pp [obj]
              (cond (map? obj)     (swap! strg #(str % (pprint-map obj indent width)))
                    (vector? obj)  (swap! strg #(str % (pprint-vec obj indent width)))
                    (string? obj)  (swap! strg #(str %  "'" obj "'"))
                    (keyword? obj) (if-let [ns (namespace obj)]
                                     (swap! strg #(str %  "'" ns "/" (name obj) "'"))
                                     (swap! strg #(str %  "'" (name obj) "'")))
                    (fn? obj)      (swap! strg #(str %  "<<function>>"))
                    :else          (swap! strg #(str % obj))))]
      (cond-> obj
        (map? obj) sort-obj
        (vector? obj) sort-obj
        true pp))))

(defstate evaluate
  :start
  (do
    (util/config-log :info)))
