(ns rad-mapper.rewrite
  "Rewrite the parse tree as Clojure, a simple task except for precedence in binary operators.
   rewrite* is a top-level function for this."
  (:require
   [rad-mapper.builtins :as bi]
   [rad-mapper.evaluate :as ev]
   [rad-mapper.util :as util]
   [rad-mapper.parse  :as par]
   [clojure.pprint :refer [cl-format pprint]]
   [clojure.set    :as set]
   [clojure.spec.alpha :as s]
   [taoensso.timbre   :as log]))

;;; ToDo:
;;;   - Look into why you can't do (defrewriter \. ...) That is, use something other than a keyword for the tag.
;;;   - All this stuff belongs in a library!

(def ^:dynamic *debugging?* false)
(def tags (atom []))
(def locals (atom [{}]))
(declare map-simplify remove-nils rewrite make-runnable)
(declare binops2bvecs walk-for-bvecs reorder-for-delimited-exps connect-bvec-fields rewrite-bvec-as-sexp precedence)

(defn rewrite*
  "A top-level function for all phases of translation.
   parse-string, simplify, rewrite and execute, but with controls for partial evaluation, debugging etc.
   With no opts it rewrites without debug output."
  [tag str & {:keys [simplify? rewrite? execute? file? debug? debug-parse? verbose?] :as opts}]
  simplify? rewrite? ; ToDo Avoid Kondo warning
  (let [kopts (-> opts keys set)
        simplify? (not-empty (set/intersection #{:execute? :rewrite? :simplify?} kopts))
        rewrite?  (not-empty (set/intersection #{:execute? :rewrite?} kopts))]
    (binding [*debugging?* debug?
              par/*debugging?* debug-parse?]
      (as-> (:result (par/parse-string tag (if file? (slurp str) str))) ?r
        (if simplify? (map-simplify ?r) ?r)
        (if rewrite?  (rewrite ?r) ?r)
        (if execute?  (ev/user-eval ?r :verbose? verbose?) ?r)
        (if (:rewrite-error? ?r)
          (log/error "Error in rewriting: result = " (with-out-str (pprint ?r)))
          ?r)))))

;;; Similar to par/defparse except that it serves no role except to make debugging nicer.
;;; You could eliminate this by global replace of "defrewrite" --> "defmethod rewrite" and removing defn rewrite.
(defmacro defrewrite [tag [obj & keys-form] & body]
  `(defmethod rewrite-meth ~tag [~'tag ~obj ~@(or keys-form '(& _))]
     (when *debugging?* (cl-format *out* "~A==> ~A~%" (util/nspaces (count @tags)) ~tag))
     (swap! tags #(conj % ~tag))
     (swap! locals #(into [{}] %))
     (let [result# (try ~@body
                        (catch Exception e#
                          (log/error "Error rewriting:" e#)
                          (ex-message (.getMessage e#)) {:obj ~obj :rewrite-error? true}))]
     (swap! tags #(-> % rest vec))
     (swap! locals #(-> % rest vec))
     (do (when *debugging?* (cl-format *out* "~A<-- ~A returns ~S~%" (util/nspaces (count @tags)) ~tag result#))
         result#))))

(defn type? [obj type]
  (cond (keyword? type) (= (:_type obj) type)
        (set? type) (-> obj :_type type)
        (map? type) (-> obj :_type type)))

(defn map-simplify
  "Recursively traverse the map structures changing records to maps,
   removing nil map values, and adding :_type value named after the record."
  [m]
  (cond (record? m) (-> {:_type (-> m .getClass .getSimpleName keyword)}
                        (into (zipmap (keys m) (map map-simplify (vals m))))
                        remove-nils),
        (coll? m) (mapv map-simplify m),
        :else m))

(defn remove-nils
  "Remove map values that are nil."
  [m]
  (reduce-kv (fn [m k v] (if (= nil v) m (assoc m k v))) {} m))

(defn rewrite-dispatch [tag _ & _] tag)

(defmulti rewrite-meth #'rewrite-dispatch)

(defn rewrite [obj & keys]
  (cond (map? obj)                  (rewrite-meth (:_type obj) obj keys)
        (seq? obj)                  (map  rewrite obj)
        (vector? obj)               (mapv rewrite obj)
        (string? obj)               obj
        (number? obj)               obj
        (symbol? obj)               obj
        (nil? obj)                  obj                    ; for optional things like (-> m :where rewrite)
        (par/binary-op?  obj)       (par/binary-op?  obj)  ; Certain keywords corespond to operators.
        (= java.util.regex.Pattern (type obj)) obj
        :else
        (throw (ex-info (str "Don't know how to rewrite obj: " obj) {:obj obj}))))

(defrewrite :JaCodeBlock [m]
  `(~'let [~@(reduce (fn [res vdecl] (into res (rewrite vdecl))) [] (:bound-vars m))]
    ~(-> m :body rewrite)))

(defrewrite :JaVarDecl [m]
  (let [val (-> m :init-val rewrite)]
    (vector (-> m :var :var-name symbol)
            (if (= :JaFnDef (:_type val)) (:form val) val))))

(defrewrite :JaField [m] (-> m :field-name))

(defrewrite :JaVar    [m] (-> m :var-name symbol)) ; ToDo Temporary?
#_(defrewrite :JaVar [m]
  (if (or (:bound? m) (= \$ (get (:var-name m) 0))) ; ToDo drop the :bound? part???
    (-> m :var-name symbol)
    (throw (ex-info "Expected an ID that started with a $." {:arg m}))))

(defn combined-map-translation [m]
  (let [sym (or bi/*test-sym* (gensym "x"))]
    `(~'mapv (~'fn [~sym] ~(-> m :exp rewrite (bi/sym-bi-access sym)))
      ~(-> m :operand rewrite))))

(defrewrite :JaParenDelimitedExp [m]
  (if (:operand m)
    (combined-map-translation m)
    (-> m :exp rewrite)))

(defrewrite :MAP [m]
  (combined-map-translation m))

;;; Whether you filter or aref depends on the argument.
;;; Thus this one has to be done at runtime. (It's a JSONata quirk).
(defn combined-filter-translation [m]
  `(~'bi/filter-aref
    ~(-> m :operand rewrite)
    ~(-> m :exp first rewrite)))

(defrewrite :JaSquareDelimitedExp [m]
  (if (:operand m)
    (combined-filter-translation m)
    (-> m :exp rewrite)))

(defrewrite :FILTER [m]
  (combined-map-translation m))

;;; ToDo: Maybe there should be a more direct way to create data!
;;; Some of these become :CONSTRUCT
(defrewrite :JaCurlyDelimitedExp [m]
  `(~'-> {}
       ~@(map rewrite (:exp m))))

(defrewrite :JaMapPair [m]
  `(~'assoc ~(:key m) ~(rewrite (:val m))))

(defrewrite :JaFnCall [m]
  (let [fname (-> m :fn-name)
        args (rewrite (:args m))]
    (if (par/builtin-fns fname)
      `(~(symbol "bi" fname) ~@args)
      `(~(symbol fname) ~@args))))

(defrewrite :JaArray [m]
  (mapv rewrite (:exprs m)))

(defrewrite :JaUniOpExp [m]
  `(~(-> m :uni-op str symbol)
    ~(-> m :exp rewrite)))

(defrewrite :or  [m] (par/binary-op? m))
(defrewrite :and [m] (par/binary-op? m))

(defrewrite :JaRangeExp [m]
  `(~'range ~(rewrite (:start m)) (~'inc ~(rewrite (:stop m)))))

(defrewrite :JaQueryDef [m]
  `(~'bi/query ~(mapv rewrite (:params m)) ~(mapv rewrite (:triples m))))

(defrewrite :JaTriple [m]
  `[~(rewrite (:ent m))
    ~(rewrite (:rel m))
    ~(rewrite (:val-exp m))])

(def ^:dynamic *in-enforce?* false)

(defrewrite :JaQueryVar [m]
  (if *in-enforce?*
    `(~'bi/get-from-bs
      ~'binding-set
      ~(-> (:qvar-name m) (subs 1) keyword))
    (-> (:qvar-name m) symbol)))

;;; enforce is a function called (typically from $reduce) with a binding set.
;;; ToDo: (:vars m) are not yet processed.
(defrewrite :JaEnforceDef [m]
  (binding [*in-enforce?* true]
    (let [result `(~'fn [~'binding-set]
                   ~(-> m :body rewrite))]
      (str "result =" result)
      result)))

(defn checking [arg]
  (println "checking: arg = " arg))

;;; This puts metadata on the function form for use by $map, $filter, $reduce, etc.
;;; User functions also translate using this, but don't use the metadata.
(defrewrite :JaFnDef [m]
  (let [vars (mapv #(-> % :var-name symbol) (:vars m))
        body (-> m :body rewrite)]
    `(~'-> (~'fn ~(mapv rewrite (:vars m)) ~body)
      (~'with-meta {:params '~vars :body '~body}))))


(defrewrite :JaTripleRole [m] (:role-name m))

;;;---------------------------- Binary ops, precedence ordering, and paths --------------------------------------------------
;;; This one produces a :BFLAT structure.
(defrewrite :JaBinOpExp [m]
  (let [preprocess (-> m  binops2bvecs walk-for-bvecs reorder-for-delimited-exps)]
    (when *debugging?*
      (println "======== :BFLAT Preprocess (START) =========")
      (pprint preprocess *out*)
      (println "------- :BFLAT Preprocess (step 1) ---------"))
    (rewrite preprocess)))

(defrewrite :BFLAT [m]
  (let [preprocess (-> m :bf connect-bvec-fields rewrite-bvec-as-sexp)]
    (when *debugging?*
      (pprint preprocess *out*)
      (println "========= :BFLAT Preprocess (END) ========"))
  (rewrite preprocess)))

(defn binops2bvecs
  "Walk structure rewriting JaBinOpExp as a map with :_type :BVEC and :bvec, a vector of the arguments and operators.
   This doesn't flatten things, just gets the bvecs in there."
  [exp]
  (cond (vector? exp) (mapv binops2bvecs exp)
        (seq? exp)    (map  binops2bvecs exp)
        (and (map? exp) (type? exp :JaBinOpExp))
        (as-> exp ?e
          (assoc ?e :bvec [(-> ?e :exp1 binops2bvecs) (:bin-op ?e) (-> ?e :exp2 binops2bvecs)])
          (assoc ?e :_type :BVEC)
          (dissoc ?e :exp1 :bin-op :exp2))
        (map? exp) (reduce-kv (fn [m k v] (assoc m k (binops2bvecs v))) {} exp)
        :else exp))

(defn collect-bvec
  "Given a BVEC, return the vector of expressions (operands) separated by binary operators it expresses."
  [bvec]
  (loop [bv bvec
         res []]
    (if (empty? bv)
      {:_type :BFLAT :bf res}
      (let [[operand1 op operand2] (:bvec bv)]
        (if (type? operand2 :BVEC)
          (recur operand2
                 (into res (vector (walk-for-bvecs operand1) op)))
          (recur []
                 (into res (vector (walk-for-bvecs operand1) op (walk-for-bvecs operand2)))))))))

(defn atomic?
  "This is mostly to speed up stepping through walk-for-bvecs!"
  [exp]
  (or (string? exp)
      (number? exp)
      (keyword? exp)
      (and (map? exp) (#{:JaField :JaVar} (:_type exp)))))

;;;**** Walk the structure. When you encounter a BVEC, call collect-bvec.
;;;     Note: collect-bvec calls this on its operands and eventually returns a BFLAT.
(defn walk-for-bvecs
  "Walk the structure; when you encounter a BVEC, replace the value with with what is
   provided by collect-bvec called on the value. collect-bvec calls this function too."
  [exp]
  (cond (seq? exp) (map walk-for-bvecs exp)
        (vector? exp) (mapv walk-for-bvecs exp)
        (and (map? exp) (type? exp :BVEC)) (collect-bvec exp)
        (map? exp) (reduce-kv (fn [m k v] (assoc m k (walk-for-bvecs v))) {} exp)
        (atomic? exp) exp
        :else exp))

(defrewrite :FILTER [m] ; ToDo: Investigate use of ~@ here.
  `(~'bi/filter-path ~(-> m :exp first rewrite second) ~(-> m :arg rewrite)))

;;; ToDo Needs work!
(defrewrite :CONSTRUCT [m]
  `(~'bi/construct-path ~(-> m :exp rewrite) ~(-> m :arg rewrite)))

(def JaDelimitedExp? {:JaParenDelimitedExp  :MAP,
                      :JaSquareDelimitedExp :FILTER,
                      :JaCurlyDelimitedExp  :CONSTRUCT})

(defn reorder-for-delimited-exps
  "The operand of things in JaDelimitedExp are things lexically prior to the delimiter exp.
   This function transforms a JaDelimitedExps (three kinds; I use the term as an abstraction over the three)
   by walking backwards over a BFLAT from the JaDelimitedExp while the expression leading to it are
   path navigations. These become the argument to the MAP/FILTER/CONSTRUCT. In the case of the FILTER,
   the builtin function determines whether it is intended as array indexing or actual filtering.
   One more quirk is addressed here: Owing to the way parsing is implemented (currently?) the
   JaDelimitedExp has an attribute that is the last operand encountered. This is put into the BFLAT
   separated by one more \\."
  [bflat]
  (let [[p1 p2] (split-with #(not (type? % JaDelimitedExp?)) (:bf bflat))]
    (if (empty? p2)
      bflat
      (let [[p1-1-r p1-2-r] (split-with #(not (and (par/binary-op? %) (not= \. %))) (reverse p1))
            p1-1 (reverse p1-2-r)
            p1-2 (reverse p1-1-r)
            [JaDE & others] p2
            primary (assoc JaDE :operand (-> {:_type :BFLAT
                                              :bf (-> p1-2 butlast vec (conj \.) ; put stuff from p1-1
                                                      (conj (:operand JaDE)))}   ; in front of what you had.
                                             reorder-for-delimited-exps))]       ; Then go deeper.
        ;(println "p1-2=" p1-2 "\n")
        ;(println "JaDE=" JaDE "\n")
        ;(println "p1-1=" p1-1 "\n")
        ;(println "others=" others "\n")
        ;(println "primary=" primary "\n")
        (-> bflat
            (assoc  :bf (conj (vec p1-1) primary))
            (update :bf into (-> {:_type :BFLAT :bf (vec others)} reorder-for-delimited-exps :bf)))))))

;;; Here it makes sense to wrap both of the fields: "a + b * f()"
;;; [{:_type :JaField, :field-name "a"} \+ {:_type :JaField, :field-name "b"} \* {:_type :JaFnCall, :fn-name "$f"}]
;;;
;;; $sum($v.field)
;;; [{:_type :JaVar :var-name "$v"} \. {:_type :JaField :field-name "field"}]
;;;
;;; [{:_type :JaField :field-name "a"} \+ {:_type :JaField :field-name "b"} \* {:_type :JaFnCall :fn-name "$f"}]
(defn connect-bvec-fields
  "Any JaField in a BFLAT.bf that isn't preceded by a bi/access (\\.) is a reference to that field in the context variable.
   Thus replace such JaFields with the one-argument call to bi/access.
   The rewrite for :JaField just returns the string.
   This leaves some fields, the ones preceded by a \\., to be processed by rewrite-bvec-as-sexp.
   These others (they start new accesses into context variable) are wrapped in 1-arg bi/access here."
  [bvec]
  (loop [bv bvec
         res []
         start? true]
    (cond (empty? bv) res,

          ;;[\. {:_type :JaField, :field-name "b"}]
          (and (== 2 (count bvec)) (= \. (first bv)) (type? (second bv) :JaField))
          (conj res `(~'bi/access ~(-> bv second :field-name))),

          (and start? (type? (first bv) :JaField))                  ; starts with a field; wrap in 1-arg bi/access.
          (recur (-> bv rest vec)
                 (vector `(~'bi/access ~(-> bv first :field-name)))
                 false),

          start?                                                    ; Starts with anything but a field, advance by one.
          (recur (-> bv rest vec)
                 (-> bv first vector)
                 false),

          ;; After start, it does [<operator>, <operand>].
          (and (= \. (first bv)) (type? (second bv) :JaField))      ; Ordinary field, advance two.
          (recur (if (<= (count bv) 2) [] (subvec bv 2))            ; rewrite-bvec-as-sexp will take care of it.
                 (into res (subvec bv 0 2))
                 false),

          (type? (second bv) :JaField)
          (recur (if (<= (count bv) 2) [] (subvec bv 2))            ; 'Newly started' field; wrap it.
                 (into res (vector (first bv) `(~'bi/access ~(-> bv second :field-name))))
                 false)
          :else                                                     ; Advance two. If it contains a field,
          (recur (if (<= (count bv) 2) [] (subvec bv 2))            ; rewrite-bvec-as-sexp will take care of it.
                 (into res (subvec bv 0 2))
                 ;(if (<= (count bv) 2) res (into res (subvec bv 0 2)))
                 false))))

;;; A lower :val means tighter binding. For example 1+2*3 means 1+(2*3) because * (300) binds tighter than + (400).
;;; Precedence ordering is done *within* rewriting, thus it is done with par symbols, not the bi ones.
(def op-precedence-tbl ; lower :val means binds tighter.
  {:or          {:assoc :left :val 1000}
   :and         {:assoc :left :val 900}
   \<           {:assoc :none :val 800}
   \>           {:assoc :none :val 800}
   :<=          {:assoc :none :val 800}
   :>=          {:assoc :none :val 800}
   \=           {:assoc :none :val 800}
   :!=          {:assoc :none :val 800}
   :in          {:assoc :none :val 700}
   \&           {:assoc :left :val 400} ; ToDo guessing
   \+           {:assoc :left :val 400}
   \-           {:assoc :left :val 400}
   :range       {:assoc :left :val 400} ; ToDo guessing
   \*           {:assoc :left :val 300}
   \%           {:assoc :left :val 300}
   \/           {:assoc :left :val 300}
   \.           {:assoc :left :val 150}})

(defn precedence [op]
  (if (contains? op-precedence-tbl op)
    (-> op op-precedence-tbl :val)
    100))

(def spec-ops (-> par/binary-op? vals set))

(s/check-asserts true)
(s/def ::op spec-ops)
(s/def ::pos  (s/and integer? pos?)) ; I *think* pos?
(s/def ::prec (s/and integer? pos?))
(s/def ::info-op  (s/keys :req-un [::pos ::op ::prec]))
(s/def ::operators (s/coll-of ::info-op :kind vector?))
(s/def ::info (s/keys :req-un [::args ::operators]))

(defn bvec2info
  "Using the bin-op-vec (at any level of processing), create a map containing information about it."
  [bvec]
  (reduce
   (fn [res [k v]]
     (if (odd? k)
       (-> res
           (update :operators #(conj % {:pos k
                                        :op (rewrite v)
                                        :prec (:val (op-precedence-tbl v))}))
           (update :args #(conj % :$op$)))
       (update res :args #(conj % v))))
   {:operators [] :args []}
   (map #(vector %1 %2) (range (count bvec)) bvec)))

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
  "Process the :bvec of BVEC to a sexps conforming to
    (1) precedence rules (which are only a concern in languages that have C-language-like syntax).
    (2) lisp-like operator before operand ordering (which is a concern for all).
  The result can have embedded un-rewritten stuff in it. Will hit that with be rewritten later."
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
