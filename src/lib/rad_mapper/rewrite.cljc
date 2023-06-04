(ns rad-mapper.rewrite
  "Rewrite the parse tree as Clojure, a simple task except for precedence in binary operators.
   processRM is a top-level function for this."
  (:require
   #?(:clj [clojure.java.io])
   [clojure.pprint                       :refer [cl-format]]
   [clojure.spec.alpha          :as s]
   [promesa.core                :as p]
   [rad-mapper.builtin          :as bi]
   [rad-mapper.builtin-macros   :as bim]
   [rad-mapper.query            :as qu]
   [rad-mapper.util             :as util :refer [dgensym! reset-dgensym! rewrite-meth qvar? unquote-qvars quote-qvars]]
   [rad-mapper.parse            :as par  :refer [builtin-fns]]
;   #?(:cljs [sci.configs.funcool.promesa :as scip])
   #?(:clj  [rad-mapper.rewrite-macros   :refer [defrewrite *debugging?*]]))
  #?(:cljs (:require-macros [rad-mapper.rewrite-macros :refer [defrewrite *debugging?*]])))

;;; ToDo: A real problem with *inside-step?* is that it is true inside any primary!
;;;       The solution here might be to distinguish 'block' from primary.
;;;       Whatever the solution, it should fix the abomination which is bi/conditional currently.

(declare rewrite)
(def diag (atom nil))

(def tkn2sym
  "Rewrite parser tokens as the function symbol, boolean value, or operator that they represent.
   Not every :op/thing need be in this list.
   Sometimes, for example {:typ :RangeExp, :start 1, :stop 5}, the operator is by-passed.
   Likewise, the so-called value-step is computed."
  {:tk/true        true
   :tk/false       false
   :op/or          'or
   :op/and         'and
   :op/lt          'bi/lt
   :op/gt          'bi/gt
   :op/lteq        'bi/lteq
   :op/gteq        'bi/gteq
   :op/eq          'bi/eq
   :op/neq         'bi/neq
   :op/in          'bi/in
   :op/thread      'bi/thread
   :op/&           'bi/&
   :op/concat-op   'bi/concat-op
   :op/add         'bi/add
   :op/subtract    'bi/subtract
   :op/multiply    'bi/multiply
   :op/%           'bi/%
   :op/div         'bi/div
   :op/get-step    'bi/get-step
   :op/filter-step 'bi/filter-step
   :op/reduce-step 'bi/reduce-step})

(defn rewrite [obj & keys]
  (cond (map? obj)                            (if-let [typ (:typ obj)] (rewrite-meth typ obj keys) obj)
        (seq? obj)                            obj
        (vector? obj)                         (mapv rewrite obj)
        (string? obj)                         obj
        (number? obj)                         obj
        (and (keyword? obj)
             (#{"tk","op"} (namespace obj)))  (get tkn2sym obj {:rm/tk-not-found obj})
        (symbol? obj)                         obj ; Overambitious rewriting, probably.
        (nil? obj)                            obj ; for optional things like (-> m :where rewrite)
        :else                                 (throw (ex-info "Don't know how to rewrite obj:" {:obj obj}))))

(defrewrite :toplevel [m]
  (rad-mapper.rewrite-macros/clear-rewrite!) ; ToDo: Nothing less will suffice! (Though restarting shadow helped!)
  (->> m :top rewrite))

(def ^:dynamic *assume-json-data?* false)
(def ^:dynamic *inside-let?*  "let is implemented in Primary" false)

(defrewrite :StringLit [m] (:value m))

;;; These are (<var> <init>) that are mapcat into a let. Evaluates to <init>.
(defrewrite :JvarDecl [m]
  (reset-dgensym!)
  (letfn [(name-exp-pair [x]
            (binding [*assume-json-data?* true] ; This is for bi/jflatten, Rule 3.
              (cond (and (-> x :var :special?) (= "$" (-> x :var :jvar-name)))
                    `(~(dgensym!) (bim/set-context! ~(-> x :init-val rewrite))),
                    ;; ToDo: Is setting $$ a legit user activity?
                    (and (-> x :var :special?) (= "$$" (-> x :var :jvar-name)))
                    `(~(dgensym!) (reset! bim/$$ ~(-> x :init-val rewrite))),
                    :else
                    `(~(->> x :var rewrite)
                      ~(-> x :init-val rewrite)))))]
    (if *inside-let?* ; True except when no CodeBlock.
      (name-exp-pair m)
      #?(:clj `(deref (p/let [~@(name-exp-pair m)] ; In case the entire exp is like $var := <whatever>; no CodeBlock.
                        ~(-> m :var rewrite)))
         :cljs `(p/let [~@(name-exp-pair m)] ; In case the entire exp is like $var := <whatever>; no CodeBlock.
                  ~(-> m :var rewrite))))))

(defrewrite :Jvar  [m]
  (cond (and (:special? m) (= "$" (:jvar-name m)))
        '(bi/deref$)
        (and (:special? m) (= "$$" (:jvar-name m)))
        `(deref bim/$$)
        :else (-> m :jvar-name symbol)))

(def ^:dynamic *inside-delim?*
  "When true, modify rewriting behavior inside 'delimited expressions'."
  false)

(def ^:dynamic *inside-step?*
  "When true, modify rewriting behavior inside 'a step'."    ; ToDo: different context than *inside-delim?*
  false)

(defrewrite :Field [m]
  (cond *inside-delim?* (:field-name m),
        *inside-step?*  `(~'bi/get-scoped ~(:field-name m)), ; ToDo: different context than *inside-delim?*
        :else `(~'bi/get-step  ~(:field-name m))))

(defrewrite :Qvar [m]
  (if *inside-step?*
    `(~'bi/get-scoped '~(-> m :qvar-name symbol)),
    `'~(-> m :qvar-name symbol)))

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
    (throw (ex-info "Regex currently does not support sticky or global flags:"
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

(def key-order "ExpressDef key string in the order they appear; use for pretty printing results" (atom []))

(defrewrite :ExpressDef [m]
  (reset! key-order [])
  (letfn [(cleanup [obj] ; :base-body is data; it should not contain get-scoped. ToDo: What else might it contains?
            (cond (map? obj)    (reduce-kv (fn [m k v] (assoc m (cleanup k) (cleanup v))) {} obj)
                  (vector? obj) (mapv cleanup obj)
                  (seq? obj)    (if (= (first obj) 'bi/get-scoped) (second obj) obj)
                  :else obj))]
    (let [params    (-> m :params rewrite)
          base-body (-> m :body rewrite)
          order @key-order]
      `(~'bi/express {:params      '~(remove map? params)
                      :options     '~(some #(when (map? %) %) params)
                      :base-body   ~(cleanup base-body)
                      :key-order   ~order}))))

(defrewrite :ObjExp [m]
  (reduce (fn [res [k v]] (assoc res k v)) {} (map rewrite (:kv-pairs m))))

;;; We use this rather than rewriting to a 'literal object' so as to control quoting.
;;; Qvars are perhaps the only thing that needs to be quoted, but that's enough!
(defrewrite :ExpressMap [m]
  (reduce (fn [res kv-pair]
            (when (-> kv-pair :key string?) (swap! key-order #(conj % (:key kv-pair))))
            (assoc res (-> kv-pair :key rewrite) (-> kv-pair :val rewrite)))
          {}
          (:kv-pairs m)))

(defrewrite :KeyExp [m]
  [:redex/express-key (rewrite (:qvar m))])

(defrewrite :KVPair [m]
  [(-> m :key rewrite) (-> m :val rewrite)])

(defrewrite :ExpressKVPair [m]
  (list (rewrite (:key m)) (rewrite (:val m))))

(def ^:dynamic *in-regex-fn?*
  "While rewriting built-in functions that take regular expressions rewrite
   the regex as a clojure regex, not a function, as used in 'str' ~> /pattern/."
  false)

(defrewrite :FnCall [m]
  (let [fname (-> m :fn-name)]
    (if (builtin-fns fname) ; ToDo: Be careful about what argument, and nesting.
      (binding [*in-regex-fn?* (#{"$match" "$split" "$contains" "$replace"} fname)]
        `(~(symbol "bi" fname) ~@(binding [*inside-let?* false] (-> m :args rewrite))))
      `(bi/fncall   {:func ~(symbol fname)
                     :args [~@(binding [*inside-let?* false] (-> m :args rewrite))]}))))

(defrewrite :RegExp [m]
    (if *in-regex-fn?*
      (make-regex (:base m) (:flags m))
      (let [s (dgensym!)]
        `(fn [~s] (bi/match-regex ~s ~(make-regex (:base m) (:flags m)))))))

(defrewrite :UniOpExp [m]
  `(~(-> m :uni-op str symbol)
    ~(-> m :exp rewrite)))

(defrewrite :RangeExp [m]
  `(-> (range ~(rewrite (:start m)) (inc ~(rewrite (:stop m)))) vec))

(defrewrite :Array [m]
  (if *assume-json-data?*
    `(with-meta
       ~(mapv rewrite (:exprs m))
       {:bi/json-array? true})
    (mapv rewrite (:exprs m))))

(defn dbs-from-qform
  "Return the DB variables from the query form in the correct order
   (the order in which they appear)."
  [qform]
  (if-let [db-forms (not-empty (filter #(== 4 (count %)) qform))]
    (->> db-forms (map first) distinct)
    '[$]))

(defn rewrite-qform
  "Define the :where of the query the query, identify db and other-args.
   Because Datascript requires predicate vars to be substituted using :in, this rewrites the :where
   and adds to the :in using generated names. DS doesn't like dollar-named predicates in the query either,
   thus I use, for example, pred-match for $match.
   - The :where is untouched except where predicates are used.
     In that case a symbol is generated/substituted for the predicate and added to :in.
   - The :in is computed as the DB arguments plus the symbols generated for predicates as above."
  [where-raw]
  (if (= "$qIdent" (:fn-name where-raw)) ; Then it is a qcall and we rewrite the function.
    {:qcall (rewrite where-raw) :in '[$]}
    (let [where (binding [*inside-step?* false] (->> where-raw (mapv rewrite) unquote-qvars))
          pred-subs (atom {})
          where-atm (atom [])
          dbs (dbs-from-qform where)]   ; Else we have to study the where, it might not be so simple.
      (doseq [pat where]
        (if (-> pat first seq?)
          (let [fn-name (-> pat first first)
                [_ _ base-name] (re-matches #"(\$)?(.*)" (name fn-name))
                pred-name (->> base-name (str "pred-") symbol)]
            (when-not (contains? @pred-subs pred-name)
              (swap! pred-subs #(assoc % pred-name fn-name)))
            (swap! where-atm #(conj % [`(~pred-name ~@(-> pat first rest))])))
          (swap! where-atm #(conj % pat))))
      {:where @where-atm
       :in (into dbs (-> @pred-subs keys))
       :pred-args (-> @pred-subs vals vec)})))

;;; ToDo: I think use of quote is problematic in all these keyword parameters.
;;;       I have pushed quoting down WRT qvar, but still using it here in :body and :pred-args.
(defrewrite :QueryDef [m]
  (let [pats (->> m :patterns (filter #(= (:typ %) :QueryPattern)))]
    (if (or (not-any? #(:db %) pats) (every?  #(:db %) pats))
      (let [{:keys [where in pred-args qcall]} (rewrite-qform (:patterns|qcall m))]
      `(~'bi/query
        {:body ~(or qcall `(quote ~where))
         :in '~in
         :pred-args '~pred-args
         :params '~(mapv rewrite (:params m))
         :options ~(-> m :options rewrite)}))
      (throw (ex-info "Either (none of / all of) the non-predicate query patterns must specify a DB." {})))))

(defrewrite :QueryPattern [m]
  `[~@(if (:db m) (list (rewrite (:db m))) '())
    ~(rewrite (:ent m))
    ~(rewrite (:rel m))
    ~(rewrite (:val m))])

(defrewrite :QueryPred [m]
  `[~(rewrite (:exp m))])

;;; This puts metadata on the function form for use by $map, $filter, $reduce, etc.
;;; User functions also translate using this, but don't use the metadata.
(defrewrite :FnDef [m]
  (let [vars (mapv #(-> % :jvar-name symbol) (:vars m))
        body (-> m :body rewrite)]
    (if *inside-let?* ; prepare for letfn.
      {:typ :letfn :vars (mapv rewrite (:vars m)) :fn-body body}
      `(with-meta (fn ~(mapv rewrite (:vars m)) ~body)
         {:bi/params '~vars :bi/type :bi/user-fn}))))

(defrewrite :PatternRole [m] (:role-name m))

(defrewrite :ImmediateUse [m]
  `(~(-> m :def rewrite) ~@(->> m :args (map rewrite))))

(defrewrite :ConditionalExp [m]
  `(bi/conditional ~(-> m :predicate rewrite)  ~(-> m :exp1 rewrite)  ~(-> m :exp2 rewrite)))

(defrewrite :OptionsMap [m]
  (-> (reduce (fn [res pair]
                (assoc res (-> pair :key keyword) (-> pair :val rewrite)))
              {}
              (:kv-pairs m))
      (with-meta {:bi/options? true})))

;;;----------------------------- Rewriting binary operations (the remainder of this file) -------------

(declare rewrite-bvec-as-sexp precedence op-precedence-tbl)

(defn rewrite-value-step
  "Wherever the sequence (:Array |  :ObjExp), :op/get-step, :op/filter-step appears,
   it indicates a quirk in parsing of things like [1,2,3].[0] which in execution
   should just return a [0] for each of [1,2,3]. Since that isn't a :ApplyFilter
   at all, we need to rewrite it (as a :ValueMap).

   N.B. This is about the sequence [:op/get-step :ApplyFilter];
   not to be confused with sequence [:op/filter-step :op/get-step],
   which is the 'non-compositional' addressed in run-steps."
  [s]
  (loop [res []
         svals s]
    (let [triple (take 3 svals)
          [p1 p2 p3] triple
          len (count triple)]
      (cond (< len 3) (into res triple)
            (and (map? p1) (#{:Array :ObjExp} (:typ p1))
                 (= :op/get-step p2)
                 (map? p3) (= :ApplyFilter (:typ p3)))
            (recur (into res (vector p1
                                     'bi/value-step
                                     {:typ :ValueStep :body (:body p3)}))
                   (->> svals (drop 3) vec))
            :else (recur (conj res p1)
                         (-> svals rest vec))))))

;;; Filter path elements (at least(?) -- maybe map and reduce too) consist of a field path element
;;; followed by the body enclosed in the delimiters; the two are not separate path elements.
(defrewrite :BinOpSeq [m]
  (->> m
       :seq
       rewrite-value-step
       rewrite-bvec-as-sexp)) ; This orders element and rewrites them to s-expressions.

(def path-fn? #{:get-step :filter-step :reduce-step :value-step :primary})

(defn wrap-non-path
  "The steps of bi/run-steps that aren't expressly path functions (e.g aren't in
   the set path-fn but rather define data) are wrapped in a function of no arguments.
   This function takes a form, analyzes it and does that work."
  [forms]
  (letfn [(wrap-form? [form sub]
            (if (or (and (symbol? form) (-> form name keyword path-fn?))
                    (and (seq? form) (-> form first name keyword path-fn?)))
              form
              `(~sub ~form)))]
    (into (-> (wrap-form? (first forms) 'bi/init-step) vector)
          (map #(if (and (= (first %) 'quote) (-> % second qvar?)) ; ToDo: Why this special condition for qvar, and not for Field?
                  (wrap-form? % 'bi/get-step)                      ; Answer: Field uses *inside-step?* binding, which I'm trying to avoid.
                  (wrap-form? % 'bi/map-step))                     ; (See ToDo at top of file.) This might be the way to get that done!
               (rest forms)))))

;;; Path are created in gather-steps.
(defrewrite :Path [m]
  `(bi/run-steps
    ~@(binding [*inside-step?* false
                *inside-delim?* false]
        (->> m
             :path
             (remove #(or (symbol? %) (keyword? %)))
             (map rewrite)
             wrap-non-path))))

;;;   If the argument vector flat ends on a :JvarDecl, add a body that is the last variable bound.
;;;   This is because, for example, '($x := 5)' (or '$x :=5' for that matter) should evaluate to 5,
;;;   and to do that these should translate to (let [$x 5] $x)."

(defn binding-maps2sexp
  "Transform the {:r/bindings ... :r/body ...} objects to an s-expression.
   For example rewriting ( $a := 1; $b := 2; $f := function($x){$x+1}; $c := 3 ) produces
   the input to this function:
    #:r{:bindings [($a 1) ($b 2)],
        :body #:r{:bindings [($f {:typ :letfn, :vars [$x], :fn-body (bi/add $x 1)})],
                  :body #:r{:bindings [($c 3)]}}}
   which is translated to.
  '(let [$a 1 $b 2]
     (letfn [($f [$x] (bi/add $x 1))]
       (let [$f (with-meta $f {:bi/params '[$x] :bi/type :bi/user-fn})]
         (let [$c 3] $c))))
    (Note the extra let to accommodate function metadata.)"
  [nested-body]
  (letfn [(nbs [form]
            (cond (= :letfn (-> form :r/bindings first second :typ))
                  `(letfn [~@(map #(list (first %) (-> % second :vars) (-> % second :fn-body))
                               (:r/bindings form))]
                     (let [~@(mapcat #(list (first %)
                                            `(with-meta ~(first %) {:bi/params '~(-> % second :vars) :bi/type :bi/user-fn}))
                                  (:r/bindings form))]
                       ~(->> form :r/body nbs)))
                  (:r/bindings form)
                  #?(:clj  `(deref (p/let [~@(mapcat #(list (first %) (second %)) (:r/bindings form))]
                                     ~(->> form :r/body nbs)))
                     :cljs `(p/let [~@(mapcat #(list (first %) (second %)) (:r/bindings form))]
                              ~(->> form :r/body nbs)))
                  (vector? form)      (mapv nbs form),
                  (seq? form)         (map nbs form),
                  (map? form)         (reduce-kv (fn [m k v] (assoc m k (nbs v))) {} form),
                  :else               form))]
    (if (:r/bindings nested-body) ; ToDo: Does the 'else' ever happen anymore? (owing to separate :Primary)
      (nbs nested-body)
      `(do ~@(nbs (:r/body nested-body))))))

(defn nest-binding-maps
  "Put each subsequent :r/binding or :r/body map in the :r/body of the previous one."
  [flat]
  (let [flat-stack (atom (rest flat))]
    (letfn [(nest [obj]
              (cond (empty? @flat-stack)     obj
                    (-> obj :r/bindings not) (:r/body obj)
                    :else                    (let [nxt (first @flat-stack)]
                                               (swap! flat-stack rest)
                                               (assoc obj :r/body (nest nxt)))))]
      (-> flat first nest))))

(defn add-last-body
  "The final :r/bindings map may end without an :r/body. In that case, the expression
   evaluates to the value of the last binding. To make this happen, we add an :r/body
   if necessary."
  [flat]
  (conj (-> flat butlast vec)
        (if (-> flat last (contains? :r/body))
          (last flat)
          (assoc (last flat)
                 :r/body
                 (-> flat last :r/bindings last first)))))

(defn rewrite2binding-maps
  "Argument is a vector of vectors of expressions (:ptag/exp) where each expressions of each sub-vector
   are of the same type
    (1) :JvarDecls binding non-function values,
    (2) :JvarDecls binding functions,
    (3) A final expression that is neither of these.
   The result is a rewrite of these to maps with :r/bindings (and the last one with a :r/body).
   Note that $x := val evaluates to value, so if there is no explicit final :r/body, the last binding is used.
   For example, output for ( $a := 1; $b := 2; $f := function($x){$x+1}; $c := 3 ) is
      [#:r{:bindings [($a 1) ($b 2)]}
       #:r{:bindings [($f {:typ :letfn, :vars [$x], :fn-body (bi/add $x 1)})]}
       #:r{:bindings [($c 3)], :body $c}]"
  [segs]
  (loop [segs segs
         res []]
    (if (empty? segs) res
        (let [seg (first segs)
              new-forms (if (= :JvarDecl (-> seg first :typ))
                          (reduce (fn [r form] ; Doesn't make sense to have :body except last. (No side-effects!)
                                    (if (= :JvarDecl (:typ form))
                                      (update r :r/bindings conj (binding [*inside-let?* true] (rewrite form))),
                                      (assoc  r :r/body (rewrite form)))) ; body is an expression.
                                  {:r/bindings []}
                                  seg)
                          {:r/body (mapv rewrite seg)})]
          (recur (rest segs) (conj res new-forms))))))

;;; Argument is {:typ :CodeBlock :exps [<:ptag/exps>]}, but since those exps can be JvarDecl assignments,
;;; which are translated to either let or letfn, they have to nest.
;;; Note that this was split off from :Primary in the parser. :Primary is just one expression, whereas
;;; these should be all JvarDecls, except optionally the last one, which is any :ptag/exp.
(defrewrite :CodeBlock [m]
  (letfn [(typ [x] (cond (and (= :JvarDecl (:typ x)) (= :FnDef (-> x :init-val :typ))) :letfn,
                         (= :JvarDecl (:typ x))                                        :let,
                         :else                                                         :ptag/exp))]
    (let [segs (loop [exps (-> m :exps rest)
                      ty (-> m :exps first typ)
                      res [(-> m :exps first vector)]]
                 (let [next-ty (-> exps first typ)]
                   (if (empty? exps)
                     res
                     (recur
                      (rest exps)
                      next-ty
                      (if (or (= ty next-ty) (= :ptag/exp next-ty))
                        (update-in res [(-> res count dec)] conj (first exps))
                        (conj res (-> exps first vector)))))))]
      ;; This is a good place to comment out calls and see how rewriting is going.
      (-> segs rewrite2binding-maps add-last-body nest-binding-maps binding-maps2sexp))))

(defrewrite :Primary [m]
  (when (> (-> m :exps count) 1)
    (throw (ex-info "Primary with multiple expressions." {:exp m})))
  (binding [*inside-step?* true]
    `(bi/primary  ~(-> m :exps first rewrite))))

(defrewrite :ApplyFilter [m]
  (reset-dgensym!)
  (let [sym (dgensym!)
        body (binding [*inside-step?* true] (-> m :body rewrite))]
    `(bi/filter-step
      (fn [~sym] (binding [bim/$ (atom ~sym)] ~body)))))

(defrewrite :ValueStep [m]
  (let [body (binding [*inside-step?* true] (-> m :body rewrite))]
    `(bi/value-step [~body])))

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
  "Step through the bvec and collect segments that include :path?=true operators and their operands into Path objects.
   The :path?=true things are :op/get-step, :op/filter-step, :op/reduce-step, and the computed value-step."
  [bvec]
  (loop [bv bvec
         res []]
    (let [consumed (atom 0)]
      (cond (empty? bv) res
            (or (-> bv first #{:op/get-step :op/filter-step :op/reduce-step})
                (value-step? bv))
            (let [steal (last res)                                                       ; Last operand belongs with path...
                  actual (or (and (not-empty res) (subvec res 0 (-> res count dec))) []) ; ...so this is what res should be before gather path.
                  collected (loop [bv2 bv
                                   path (if steal [steal] [])]
                              (cond (empty? bv2) path   ; end of path

                                    (and (contains? op-precedence-tbl (-> bv2 first rewrite)) ; end of path
                                         (-> bv2 first rewrite op-precedence-tbl :path? not)) path

                                    (value-step? bv2) ; add a value-step, e.g. [1 2 3].['hello'] to path.
                                    (do (swap! consumed #(+ % 3))
                                        (recur (drop 3 bv2)
                                               (conj (conj path (first bv2))
                                                     {:typ :ValueStep
                                                      :body (:body (nth bv2 2))})))
                                    :else
                                    (do (swap! consumed inc)
                                        (recur (drop 1 bv2)
                                               (conj path (first bv2))))))]
              (recur (drop @consumed bv)
                     (conj actual (rewrite {:typ :Path :path collected}))))
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
  "Process the :seq of a BinOpSeq a sexp conforming to
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
                                      :form (let [op (-> ?omap :op symbol)]
                                              (if (= op 'bi/thread) ; This allows us to not need a macro for threading
                                                (let [fn-arg (nth (:args info) (inc pos)) ; 'immediate' means function(..)
                                                      immediate? (and (seq? (first fn-arg)) (= "with-meta" (-> fn-arg first first name)))]
                                                  (list op
                                                        (nth (:args info) (dec pos))
                                                        (if immediate?
                                                          `(binding [bim/*threading?* true] ~@fn-arg)
                                                          `(binding [bim/*threading?* true] ~fn-arg))))
                                                (list op
                                                      (nth (:args info) (dec pos))
                                                      (nth (:args info) (inc pos)))))}

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


;;; ToDo: This is a leftover from MiniZinc!
;;; A lower :val means tighter binding, means do it do it sooner.
;;; For example, 1+2*3 means 1+(2*3) because * (300) binds tighter than + (400).
(def op-precedence-tbl ; lower :val means binds tighter.
  {'or               {:path? false :assoc :left :val 1000}
   'and              {:path? false :assoc :left :val 900}
   'bi/lt            {:path? false :assoc :none :val 800}
   'bi/gt            {:path? false :assoc :none :val 800}
   'bi/lteq          {:path? false :assoc :none :val 800}
   'bi/gteq          {:path? false :assoc :none :val 800}
   'bi/eq            {:path? false :assoc :none :val 800}
   'bi/neq           {:path? false :assoc :none :val 800}
   'bi/in            {:path? false :assoc :none :val 700}
   'bi/thread        {:path? false :assoc :left :val 700} ; ToDo guessing
   'bi/&             {:path? false :assoc :left :val 400} ; ToDo guessing
   'bi/concat-op     {:path? false :assoc :left :val 400}
   'bi/add           {:path? false :assoc :left :val 400}
   'bi/subtract      {:path? false :assoc :left :val 400}
   'bi/range         {:path? false :assoc :left :val 400} ; ToDo guessing
   'bi/multiply      {:path? false :assoc :left :val 300}
   'bi/%             {:path? false :assoc :left :val 300}
   'bi/div           {:path? false :assoc :left :val 300}
   'bi/get-step      {:path? true  :assoc :left :val 100}
   'bi/filter-step   {:path? true  :assoc :left :val 100}
   'bi/reduce-step   {:path? true  :assoc :left :val 100}})

(defn precedence [op]
  (if (contains? op-precedence-tbl op)
    (-> op op-precedence-tbl :val)
    (throw (ex-info "****** No precedence:" {:op op})))) ; ToDo: This can go away someday
