(ns rad-mapper.rewrite
  "Rewrite the parse tree as Clojure, a simple task except for precedence in binary operators.
   rewrite* is a top-level function for this."
  (:require
   [clojure.java.io     :as io]
   [clojure.pprint      :refer [cl-format pprint]]
   [clojure.set         :as set]
   [clojure.spec.alpha  :as s]
   [rad-mapper.builtins :as bi]
   [rad-mapper.evaluate :as ev]
   [rad-mapper.util     :as util :refer [dgensym! reset-dgensym!]]
   [rad-mapper.parse    :as par]
   [taoensso.timbre     :as log]))

(def ^:dynamic *debugging?* false)
(def tags (atom []))
(def locals (atom [{}]))
(declare map-simplify rewrite)
(def diag (atom nil))

(defn rewrite*
  "A top-level function for all phases of translation.
   parse-string, simplify, rewrite and execute, but with controls for partial evaluation, debugging etc.
   With no opts it rewrites without debug output."
  [tag str & {:keys [simplify? rewrite? execute? file? debug? debug-parse? skip-top? verbose?] :as opts}]
  simplify? rewrite? ; ToDo Avoid Kondo warning
  (let [kopts (-> opts keys set)
        simplify? (not-empty (set/intersection #{:rewrite? :simplify? :execute?} kopts))
        rewrite?  (not-empty (set/intersection #{:rewrite? :execute?} kopts))]
    (binding [*debugging?* debug?
              par/*debugging?* debug-parse?]
      (if (or *debugging?* par/*debugging?*) (s/check-asserts true) (s/check-asserts false))
      (let [ps (with-open [rdr (io/reader (if file? str (char-array str)))]
                 (as-> (par/make-pstate rdr) ?ps
                   (par/parse tag ?ps)
                   (dissoc ?ps :line-seq) ; dissoc so you can print it.
                   (assoc ?ps :parse-status (if (-> ?ps :tokens empty?) :ok :premature-end))))]
        (if (= :ok (:parse-status ps))
          (as-> (:result ps) ?r
            (if simplify? (map-simplify ?r) ?r)
            (if rewrite?
              (->> (if skip-top? ?r {:_type :toplevel :top ?r}) rewrite)
              ?r)
            (if execute? (ev/user-eval ?r :verbose? verbose?) ?r)
            (if (:rewrite-error? ?r)
              (throw (ex-info "Error in rewriting" {:result (with-out-str (pprint ?r))}))
              ?r))
          (case (:parse-status ps)
            :premature-end (log/error "Parse ended prematurely")))))))

;;; Similar to par/defparse except that it serves no role except to make debugging nicer.
;;; You could eliminate this by global replace of "defrewrite" --> "defmethod rewrite" and removing defn rewrite.
;;; Note: The need for the doall below eluded me for a long time.
;;;       It is necessary when using dynamic binding or want state in an atom to be seen.
(defmacro defrewrite [tag [obj & keys-form] & body]
  `(defmethod rewrite-meth ~tag [~'tag ~obj ~@(or keys-form '(& _))]
     (when *debugging?* (cl-format *out* "~A==> ~A~%" (util/nspaces (count @tags)) ~tag))
     (swap! tags #(conj % ~tag))
     (swap! locals #(into [{}] %))
     (let [result# (try (let [res# (do ~@body)] (if (seq? res#) (doall res#) res#)) ; See note above.
                        ;; I don't think that I would typically want to catch these!
                        #_(catch Exception e#
                          (log/error "Error rewriting:" e#)
                          (ex-message (.getMessage e#))
                          {:obj ~obj :rewrite-error? true}))]
     (swap! tags #(-> % rest vec))
     (swap! locals #(-> % rest vec))
     (do (when *debugging?* (cl-format *out* "~A<-- ~A returns ~S~%" (util/nspaces (count @tags)) ~tag result#))
         result#))))

(defn type? [obj type]
  (cond (keyword? type) (= (:_type obj) type)
        (set? type) (-> obj :_type type)
        (map? type) (-> obj :_type type)))

(defn remove-nils
  "Remove map values that are nil."
  [m]
  (reduce-kv (fn [m k v] (if (= nil v) m (assoc m k v))) {} m))

(defn map-simplify
  "Recursively traverse the map structures changing records to maps,
   removing nil map values, and adding :_type value named after the record."
  [m]
  (cond (record? m) (-> {:_type (-> m .getClass .getSimpleName keyword)}
                        (into (zipmap (keys m) (map map-simplify (vals m))))
                        remove-nils),
        (map? m) m,
        (coll? m) (mapv map-simplify m),
        :else m))

(defn rewrite-dispatch [tag _ & _] tag)

(defmulti rewrite-meth #'rewrite-dispatch)

;;; ToDo: Investigate how this happens. It started with including the rewrite in gather-steps.
(defmethod rewrite-meth :default [& _args]
  ;(println "****Called :default rewrite-meth")
  {})

(defn rewrite [obj & keys]
  (cond (map? obj)                  (if-let [typ (:_type obj)] (rewrite-meth typ obj keys) obj)
        (seq? obj)                  obj #_(map  rewrite obj) ; ToDo: review this. I think I might be able to just return it.
        (vector? obj)               (mapv rewrite obj)
        (par/binary-op? obj)        (par/binary-op?  obj)  ; Certain keywords and chars correspond to operators.
        (string? obj)               obj
        (number? obj)               obj
        (symbol? obj)               obj
        (= obj :true)               true
        (= obj :false)              false
        (= obj true)                true
        (= obj false)               false
        (nil? obj)                  obj                    ; for optional things like (-> m :where rewrite)
        #_#_(= java.util.regex.Pattern (type obj)) obj
        :else
        (throw (ex-info (str "Don't know how to rewrite obj: " obj) {:obj obj}))))

(defrewrite :toplevel [m] (->> m :top rewrite))

(def ^:dynamic *assume-json-data?* false)
(def ^:dynamic *inside-let?*  "let is implemented in JaPrimary" false)

;;; These are (<var> <init>) that are mapcat into a let.
(defrewrite :JaJvarDecl [m]
  (reset-dgensym!)
  (letfn [(name-exp-pair [x]
            (binding [*assume-json-data?* true] ; This is for bi/jflatten, Rule 3.
              (cond (and (-> x :var :special?) (= "$" (-> x :var :jvar-name)))
                    `(~(dgensym!) (bi/set-context! ~(-> x :init-val rewrite))),
                    ;; ToDo: Is setting $$ a legit user activity?
                    (and (-> x :var :special?) (= "$$" (-> x :var :jvar-name)))
                    `(~(dgensym!) (reset! bi/$$ ~(-> x :init-val rewrite))),
                    :else
                    `(~(->> x :var rewrite)
                      ~(-> x :init-val rewrite)))))]
    (if *inside-let?*
      (name-exp-pair m)
      `(let [~@(name-exp-pair m)] ; In case the entire exp is like $var := <whatever>; no primary.
         ~(-> m :var rewrite)))))

{:_type :JaJvarDecl, :var {:_type :JaJvar, :jvar-name "$var"}, :init-val 3}

(defrewrite :JaJvar  [m]
  (cond (and (:special? m) (= "$" (:jvar-name m)))
        '(bi/deref$)
        (and (:special? m) (= "$$" (:jvar-name m)))
        `(deref bi/$$)
        :else (-> m :jvar-name symbol)))

(def ^:dynamic inside-delim?
  "When true, modify rewriting behavior inside 'delimited expressions'."
  false)

(def ^:dynamic inside-step?
  "When true, modify rewriting behavior inside 'a step'." ; ToDo: different context than inside-delim?
  false)

(defrewrite :JaField [m]
  (cond inside-delim? (:field-name m),
        inside-step?  `(~'bi/get-scoped ~(:field-name m)), ; ToDo: different context than inside-delim?
        :else `(~'bi/get-step  ~(:field-name m))))

(defrewrite :JaQvar [m]
  (-> m :qvar-name symbol (with-meta {:qvar? true})))

;;; Java's regex doesn't recognize switches /g and /i; those are controlled by constants in java.util.regex.Pattern.
;;; https://www.codeguage.com/courses/regexp/flags
;;; Flags - i = (ignore case) Makes the expression search case-insensitively.
;;;         m = (multiline) Makes the boundary characters ^ and $ match the beginning and ending of every single line.
;;;         u = (unicode) enable unicode matching
;;;         g = (global)  match all occurrences
;;;         s = (dot all) makes the wild character . match newlines as well.
;;;         y = (sticky) Makes the expression start its searching from the index indicated in its lastIndex property.
;;;
;;; https://www.w3schools.com/js/js_regexp.asp
(defn make-regex
  "Return a equivalent JS-like regular expression for the argument string and flags.
   These search the string for the pattern; they ignore characters outside of the
   pattern. For example, /e/.test('The best things in life are free!') ==> returns true.
   Thus it is like #'.*(pattern).*'."
  [base flags]
  (when (or (:sticky? flags) (:global? flags))
    (throw (ex-info "Regex currently does not support sticky or global flags."
                    {:base base :flags flags})))
  (let [body (subs base 1 (-> base count dec)) ; /pattern/ -> pattern
        flag-str (cond-> ""
                   (:ignore-case? flags)   (str "i")
                   (:multi-line? flags)    (str "m")
                   (:unicode? flags)       (str "u")
                   (:dot-all? flags)       (str "s"))]
    (if (empty? flag-str)
      (re-pattern (cl-format nil "~A" body))
      (re-pattern (cl-format nil "(?~A)~A" flag-str body)))))

(defrewrite :JaEnforceDef [m]
  (let [p (-> m :params rewrite)]
    `(~'bi/enforce {:params   '~(remove map? p)
                    :options  ' ~(some #(when (map? %) %) p)
                    :body '~(-> m :body rewrite)})))

;;; JaEnforceBody is like an JaObjExp (map) but not rewritten as one.
;;; Below they are interleaved.
(defrewrite :JaObjExp [m]
  `(-> {}
       ~@(map rewrite (:exp m))))

(defrewrite :JaEnforceMap [m]
  (reduce (fn [res kv-pair]
            (assoc res (-> kv-pair :key rewrite) (-> kv-pair :val rewrite)))
          {}
          (:kv-pairs m)))

(defrewrite :JaKVPair [m]
  `(assoc ~(rewrite (:key m)) ~(rewrite (:val m))))

(defrewrite :JaEnforceKVPair [m]
  (list (rewrite (:key m)) (rewrite (:val m))))

(def ^:dynamic in-regex-fn?
  "While rewriting built-in functions that take regular expressions rewrite
   the regex as a clojure regex, not a function, as used in 'str' ~> /pattern/."
  false)

(defrewrite :JaFnCall [m]
  (let [fname (-> m :fn-name)]
    (if (par/builtin-fns fname) ; ToDo: Be careful about what argument, and nesting.
      (binding [in-regex-fn? (#{"$match" "$split" "$contains" "$replace"} fname)]
        `(~(symbol "bi" fname) ~@(-> m :args rewrite)))
      `(~(symbol fname) ~@(-> m :args rewrite)))))

(defrewrite :JaRegExp [m]
    (if in-regex-fn?
      (make-regex (:base m) (:flags m))
      (let [s (dgensym!)]
        `(fn [~s] (bi/match-regex ~s ~(make-regex (:base m) (:flags m)))))))

(defrewrite :JaUniOpExp [m]
  `(~(-> m :uni-op str symbol)
    ~(-> m :exp rewrite)))

(defrewrite :JaRangeExp [m]
  `(-> (range ~(rewrite (:start m)) (inc ~(rewrite (:stop m)))) vec))

(defrewrite :JaArray [m]
  (if *assume-json-data?*
    `(with-meta
       ~(mapv rewrite (:exprs m))
       {:bi/json-array? true})
    (mapv rewrite (:exprs m))))

(defrewrite :JaQueryDef [m]
  `(~'bi/query '~(mapv rewrite (:params m)) '~(mapv rewrite (:triples m))))

(defrewrite :JaQueryTriple [m]
  `[~(rewrite (:ent m))
    ~(rewrite (:rel m))
    ~(rewrite (:val-exp m))])

(defrewrite :JaQueryPred [m]
  `[~(rewrite (:exp m))])

;;; This puts metadata on the function form for use by $map, $filter, $reduce, etc.
;;; User functions also translate using this, but don't use the metadata.
(defrewrite :JaFnDef [m]
  (let [vars (mapv #(-> % :jvar-name symbol) (:vars m))
        body (-> m :body rewrite)]
    `(with-meta (fn ~(mapv rewrite (:vars m)) ~body)
      {:bi/params '~vars :bi/type :bi/user-fn})))

(defrewrite :JaTripleRole [m] (:role-name m))

(defrewrite :JaImmediateUse [m]
  `(~(-> m :def rewrite) ~@(->> m :args (map rewrite))))

;;; See rewrite-thread-immediate for how this came to be.
(defrewrite :JaThreadRHSfn [m]
  `(~@(-> m :def rewrite) ~@(->> m :args (map rewrite))))

;;; See rewrite-thread-immediate for how this came to be.
;;; This one transform a function call into a function.
(defrewrite :JaThreadRHSfn-call [m]
  (reset-dgensym!) ; For debugging, start at _x1
  (let [xtra-arg (dgensym!)
        [fname & args] (-> m (assoc :_type :JaFnCall) rewrite)]
    `(fn [~xtra-arg] (~fname ~xtra-arg ~@args))))

(defrewrite :JaConditionalExp [m]
  `(if ~(-> m :predicate rewrite)
    ~(-> m :exp1 rewrite)
    ~(-> m :exp2 rewrite)))

(defrewrite :JaOptionsMap [m]
  (reduce (fn [res pair]
            (assoc res (-> pair :key keyword) (-> pair :val rewrite)))
          {}
          (:kv-pairs m)))

;;;----------------------------- Rewriting binary operations (the remainder of this file) -------------

(declare rewrite-bvec-as-sexp precedence op-precedence-tbl)

(defn rewrite-value-step
  "Wherever the sequence (:JaArray |  :JaObjExp), :bi/get-step, :JaApplyFilter appears,
   it indicates a quirk in parsing of things like [1,2,3].[0] which in execution
   should just return a [0] for each of [1,2,3]. Since that isn't a :JaApplyFilter
   at all, we need to rewrite it (as a :JaValueMap)."
  [s]
  (loop [res []
         svals s]
    (let [triple (take 3 svals)
          [p1 p2 p3] triple
          len (count triple)]
      (cond (< len 3) (into res triple)
            (and (map? p1) (#{:JaArray :JaObjExp} (:_type p1))
                 (= 'bi/get-step p2)
                 (map? p3) (= :JaApplyFilter (:_type p3)))
            (recur (into res (vector p1
                                     'bi/value-step
                                     {:_type :JaValueStep :body (:body p3)}))
                   (->> svals (drop 3) vec))
            :else (recur (conj res p1)
                         (-> svals rest vec))))))

(defn rewrite-thread-immediate
  "Wherever the sequence <whatever> bi/thread JaImmediateUse appears,
   it indicates a quirk in parsing things like 4 ~> function($x){$x+1}()
   where rewriting as a JaImmediateUse is incorrect; the RHS operator
   needs to be preserved as a function def, not executed. This does
   that by changing by replacing the JaImmediate use with a JaThreadRHS."
  [s]
  (loop [res []
         svals s]
    (let [triple (take 3 svals)
          [p1 p2 p3] triple
          len (count triple)]
      (cond (< len 3) (into res triple)
            (and (= 'bi/thread p2)
                 (map? p3) (#{:JaImmediateUse :JaFnCall} (:_type p3)))
            (recur (into res (vector p1
                                     'bi/thread
                                     (case (:_type p3)
                                       :JaImmediateUse (assoc p3 :_type :JaThreadRHSfn)
                                       :JaFnCall       (assoc p3 :_type :JaThreadRHSfn-call))))
                   (->> svals (drop 3) vec))
            :else (recur (conj res p1)
                         (-> svals rest vec))))))

;;; Filter path elements (at least(?) -- maybe map and reduce too) consist of a field path element
;;; followed by the body enclosed in the delimiters; the two are not separate path elements.
(defrewrite :JaBinOpSeq [m]
  (->> m
       :seq
       rewrite-value-step
       rewrite-thread-immediate
       rewrite-bvec-as-sexp)) ; This orders element and rewrites them to s-expressions.

(def path-fn? #{:get-step :filter-step :reduce-step :value-step :primary})

(defn wrap-non-path
  "The steps of bi/run-steps that aren't expressly path functions (for example,
   they aren't in the set path-fn but rather define data) are wrapped in a function
   of no arguments. This function takes a form, analyzes it and does that work."
  [forms]
  (letfn [(wrap-form? [form sub]
            (if (or (and (symbol? form) (-> form name keyword path-fn?))
                    (and (seq? form) (-> form first name keyword path-fn?)))
              form
              `(~sub ~form)))]
    (into (-> (wrap-form? (first forms) 'bi/init-step) vector)
          (map #(wrap-form? % 'bi/map-step) (rest forms)))))

;;; JaPath are created in gather-steps.
(defrewrite :JaPath [m]
  `(bi/run-steps
    ~@(binding [inside-step? false
                inside-delim? false]
        (->> m
             :path
             (remove #(or (symbol? %) (keyword? %)))
             (map rewrite)
             wrap-non-path))))

;;; Where any of the :exps are JaJvarDecl, they need to wrap the things that follow in a let.
;;; Essentially, this turns a sequence into a tree.
(defrewrite :JaPrimary [m]
  (binding [inside-step? true]
    (let [segs (util/split-by (complement #(= :JaJvarDecl (:_type %))) (:exps m)) ; split a let
          map-vec (loop [segs segs
                         res []]
                    (if (empty? segs) res
                        (let [seg (first segs)
                              new-forms (if (= :JaJvarDecl (-> seg first :_type))
                                          (reduce (fn [r form]
                                                    (binding [*inside-let?* true]
                                                      (if (= :JaJvarDecl (:_type form))
                                                        (update r :r/bindings conj (rewrite form))
                                                        (update r :r/body conj (rewrite form)))))
                                                  {:r/bindings [] :r/body []}
                                                  seg)
                                          {:r/body (mapv rewrite seg)})]
                          (recur (rest segs) (conj res new-forms)))))
          res (reduce (fn [r m] (update r :r/body conj m)) (first map-vec) (rest map-vec))] ; nest body
      (letfn [(rew [form] ; Rewrite nested map as a s-exp.
                (cond (:r/bindings form)  (if (-> form :r/body empty?)
                                              `(let [~@(mapcat #(list (first %) (second %)) (:r/bindings form))]
                                                 ~(-> form :r/bindings last first)) ; Then return value of the := assignment.
                                              `(let [~@(mapcat #(list (first %) (second %)) (:r/bindings form))]
                                                 ~@(->> form :r/body (map rew))))
                      (:r/body form)      (->> form :r/body (map rew)),
                      (vector? form)      (mapv rew form),
                      (seq? form)         (map rew form),
                      (map? form)         (reduce-kv (fn [m k v] (assoc m k (rew v))) {} form),
                      :else                form))]
        `(bi/primary
          ~(cond (:r/bindings res)              (rew res),
                 (== 1 (-> res :r/body count))  (-> res :r/body first rew)
                 :else                         `(do ~@(rew (:r/body res)))))))))

(defrewrite :JaApplyFilter [m]
  (reset-dgensym!)
  (let [sym (dgensym!)
        body (binding [inside-step? true] (-> m :body rewrite))]
    `(bi/filter-step
      (fn [~sym] (bi/with-context ~sym ~body)))))

(defrewrite :JaValueStep [m]
  (let [body (binding [inside-step? true] (-> m :body rewrite))]
    `(bi/value-step ~body)))

(defrewrite :JaApplyReduce [m]
  (let [sym (dgensym!)
        body (-> m :body rewrite)]
    `(bi/reduce-step
      ~(-> m :operand rewrite)
      (fn [~sym] (bi/with-context ~sym ~body)))))

(def spec-ops (-> par/binary-op? vals set)) ; ToDo: Not necessary?

(s/check-asserts true) ; See also *compile-asserts*
(s/def ::op spec-ops)
(s/def ::pos  (s/and integer? pos?)) ; I *think* pos?
(s/def ::prec (s/and integer? pos?))
(s/def ::info-op  (s/keys :req-un [::pos ::op ::prec]))
(s/def ::operators (s/coll-of ::info-op :kind vector?))
(s/def ::info (s/keys :req-un [::args ::operators]))

(defn value-step?
  "Returns true if matches value-step."
  [bvec]
  (let [[_p1 p2 p3] bvec]
    (and p3 (= p2 'bi/value-step))))

;;; Note: If support of filter-step non-compositional semantics is still "a thing" this is the place to fix it.
(defn gather-steps ; ToDo: Should :reduce-step really be :path?=true ?
  "Step through the bvec and collect segments that include :path?=true operators/operands into JaPath objects.
   The :path?=true things are bi/get-step, bi/filter-step, bi/reduce-step, bi/primary."
  [bvec]
  (loop [bv bvec
         res []]
    (let [consumed (atom 0)]
      (cond (empty? bv) res
            (or (-> bv first op-precedence-tbl :path?)
                (value-step? bv))
            (let [steal (last res)                                                       ; Last operand belongs with path...
                  actual (or (and (not-empty res) (subvec res 0 (-> res count dec))) []) ; ...so this is what res should be before gather path.
                  collected (loop [bv2 bv
                                   path (if steal [steal] [])]
                              (cond (empty? bv2) path   ; end of path

                                    (and (contains? op-precedence-tbl (first bv2)) ; end of path
                                         (-> bv2 first op-precedence-tbl :path? not)) path

                                    (value-step? bv2) ; add a value-step, e.g. [1 2 3].['hello'] to path.
                                    (do (swap! consumed #(+ % 3))
                                        (recur (drop 3 bv2)
                                               (conj (conj path (first bv2))
                                                     {:_type :JaValueStep
                                                      :body (:body (nth bv2 2))})))
                                    :else
                                    (do (swap! consumed inc)
                                        (recur (drop 1 bv2)
                                               (conj path (first bv2))))))]
              (recur (drop @consumed bv)
                     (conj actual (rewrite {:_type :JaPath :path collected}))))
            :else
            (recur (drop 1 bv)
                   (conj res (first bv)))))))

(defn basic-info
  "Using the bin-op-vec (at any level of processing), create a map containing information about it."
  [bvec]
  (reduce
   (fn [res [k v]]
     (if (odd? k)
       (let [op (rewrite v)]
         (-> res
             (update :operators #(conj % {:pos k
                                          :op op
                                          :prec (precedence op)}))
           (update :args #(conj % :$op$))))
        (update res :args #(conj % (rewrite v)))))
   {:operators [] :args []}
   (map #(vector %1 %2) (range (count bvec)) bvec)))

(defn bvec2info
  "Gather paths so that they their internal navigation isn't visible, then create the info object."
  [bvec]
  (-> bvec gather-steps basic-info))

(defn update-op-pos
  "Update the :pos values in operators according to new shortened :operands."
  [info]
  (let [args (:args info)
        replace-pos (reduce (fn [positions [idx v]]
                              (if (= v :$op$)
                                (conj positions idx)
                                positions))
                            []
                            (map #(vector %1 %2) (-> args count range) args))]
    (assoc info
           :operators
           (loop [operators (:operators info)
                  result []
                  positions replace-pos]
             (let [operator (first operators)
                   is-op? (contains? operator :op)]
               (if (empty? operators)
                 result
                 (recur
                  (rest operators)
                  (if is-op?
                    (conj result (assoc operator :pos (first positions)))
                    (conj result nil)) ; placeholder needed by calling function.
                  (if is-op? (rest positions) positions))))))))

(defn update-args
  "Remove used operands and replace with sexps."
  [info mod-pos]
  (update info
            :args
            (fn [args]
              (reduce (fn [o pos]
                        (cond (or (= mod-pos (dec pos)) (= mod-pos (inc pos)))  ;; eliminated
                              o,
                              (= mod-pos pos)
                              (conj o (some #(when (= (:pos %) pos) (:form %))  ;; composed sexp
                                            (:operators info))),
                              :else
                              (conj o (nth args pos))))                         ;; no change
                      []
                      (range (count args))))))

(defn rewrite-bvec-as-sexp
  "Process the :seq of a JaBinOpSeq a sexp conforming to
    (1) precedence rules (which are only a concern in languages that have C-language-like syntax).
    (2) lisp-like operator before operand ordering (which is a concern for all).
  The result can have embedded un-rewritten stuff in it; that will be rewritten later."
  [bvec]
  (let [info (bvec2info bvec)]
    (s/assert ::info info)
    (as-> info ?info
      (reduce (fn [info pval]
                (loop [index (-> info :operators count range)
                       info info]
                  (let [ops (:operators info)]
                    (if (empty? index)
                      info
                      (let [ix (first index) ; Picks out an operator (might not be modified; see pval).
                            omap (nth ops ix)
                            pos  (:pos omap)
                            prec (:prec omap)
                            omap (as-> omap ?omap
                                   (if (= pval prec)
                                     {:pos pos
                                      :form (list (-> ?omap :op symbol)
                                                  (nth (:args info) (dec pos))
                                                  (nth (:args info) (inc pos)))}
                                     ?omap))]
                        (recur (rest index)
                               (-> info
                                   (assoc :operators (into (conj (subvec ops 0 ix) omap)
                                                           (subvec ops (inc ix))))
                                   (cond-> (= pval prec) (update-args pos)
                                           (= pval prec) update-op-pos))))))))
              ?info
              (-> (map :prec (:operators ?info)) distinct sort))
      (-> ?info :args first))))

;;; A lower :val means tighter binding. ToDo: That's a leftover from MiniZinc!
;;; For example, 1+2*3 means 1+(2*3) because * (300) binds tighter than + (400).
(def op-precedence-tbl ; lower :val means binds tighter.
  {:or               {:path? false :assoc :left :val 1000}
   :and              {:path? false :assoc :left :val 900}
   'bi/<             {:path? false :assoc :none :val 800}
   'bi/>             {:path? false :assoc :none :val 800}
   'bi/<=            {:path? false :assoc :none :val 800}
   'bi/>=            {:path? false :assoc :none :val 800}
   'bi/=             {:path? false :assoc :none :val 800}
   :!=               {:path? false :assoc :none :val 800}
   :in               {:path? false :assoc :none :val 700}
   'bi/thread        {:path? false :assoc :left :val 700} ; ToDo guessing
   'bi/&             {:path? false :assoc :left :val 400} ; ToDo guessing
   'str              {:path? false :assoc :left :val 400}
   'bi/+             {:path? false :assoc :left :val 400}
   'bi/-             {:path? false :assoc :left :val 400}
   :range            {:path? false :assoc :left :val 400} ; ToDo guessing
   'bi/*             {:path? false :assoc :left :val 300}
   'bi/%             {:path? false :assoc :left :val 300}
   'bi//             {:path? false :assoc :left :val 300}
   'bi/get-step      {:path? true  :assoc :left :val 100}
   'bi/filter-step   {:path? true  :assoc :left :val 100}
   'bi/reduce-step   {:path? true  :assoc :left :val 100}})

(defn precedence [op]
  (if (contains? op-precedence-tbl op)
    (-> op op-precedence-tbl :val)
    (do (println "****** No precedence:" op)
        (reset! diag op)
        100)))
