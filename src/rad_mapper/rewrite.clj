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
                   (reset! diag (dissoc ?ps :line-seq)) ; dissoc so you can print it.
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
        (coll? m) (mapv map-simplify m),
        :else m))

(defn rewrite-dispatch [tag _ & _] tag)

(defmulti rewrite-meth #'rewrite-dispatch)

;;; ToDo: Investigate how this happens. It started with including the rewrite in gather-paths.
(defmethod rewrite-meth :default [& args]
  ;(println "****Called :default rewrite-meth")
  {})

(defn rewrite [obj & keys]
  (cond (map? obj)                  (rewrite-meth (:_type obj) obj keys)
        (seq? obj)                  (map  rewrite obj) ; ToDo: review this. I think I might be able to just return it.
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
        (= java.util.regex.Pattern (type obj)) obj
        :else
        (throw (ex-info (str "Don't know how to rewrite obj: " obj) {:obj obj}))))

(defrewrite :toplevel [m]
  `(~'bi/finish
    (~@(->> m :top rewrite))))

;;; ToDo: Return to the let-style implementation; assignments are expressions
(defrewrite :JaCodeBlock [m]
  (util/reset-dgensym!)
  (let [returned-exp (-> m :body last)
        others (->> m :body butlast)]
    (if (empty? others)
      `(-> (bi/init-state-obj) ~(rewrite returned-exp))
      `(let [~@(mapcat rewrite others)] ~(rewrite returned-exp)))))

;;; These are (<var> <init>) that are mapcat into a let.
(defrewrite :JaJvarDecl [m]
  (cond (and (-> m :var :special?) (= "$" (-> m :var :jvar-name)))
        `(~(dgensym!) (bi/set-context! ~(-> m :init-val rewrite))),
        ;; ToDo: Is setting $$ a legit user activity?
        (and (-> m :var :special?) (= "$$" (-> m :var :jvar-name)))
        `(~(dgensym!) (reset! bi/$$ ~(-> m :init-val rewrite))),
        :else
        `(~(->> m :var rewrite)
          ~(-> m :init-val rewrite))))

(defrewrite :JaJvar  [m]
  (cond (and (:special? m) (= "$" (:jvar-name m)))
        `(bi/deref$)
        (and (:special? m) (= "$$" (:jvar-name m)))
        `(deref bi/$$)
        :else (-> m :jvar-name symbol)))

(def ^:dynamic inside-delim?
  "When true, modify rewriting behavior inside 'delimited expressions'."
  false)

(def ^:dynamic in-enforce?
  "While inside an enforce, qvar references are wrapped."
  false)

(defrewrite :JaField [m]
  (if inside-delim?
    (-> m :field-name)
    `(~'bi/dot-map  ~(-> m :field-name))))

(defrewrite :JaQvar [m]
  (if in-enforce? ; b-set is an argument passed into the enforce function.
    `(~'bi/get-from-b-set b-set ~(-> m :qvar-name keyword))
    (-> m :qvar-name symbol)))

(defrewrite :JaObjExp [m]
  `(-> {}
    ~@(map rewrite (:exp m))))

(defrewrite :JaKVPair [m]
  `(assoc ~(:key m) ~(rewrite (:val m))))

(defrewrite :JaFnCall [m]
  (let [fname (-> m :fn-name)
        args (rewrite (:args m))]
    (if (par/builtin-fns fname)
      `(~(symbol "bi" fname) ~@args)
      `(~(symbol fname) ~@args))))

(defrewrite :JaUniOpExp [m]
  `(~(-> m :uni-op str symbol)
    ~(-> m :exp rewrite)))

;;; ToDo: Taken care of by defn rewrite above, right?
(defrewrite :or  [_m] #_(par/binary-op? m) (throw (ex-info ":or ???" {})))
(defrewrite :and [_m] #_(par/binary-op? m) (throw (ex-info ":and ???" {})))

(defrewrite :JaRangeExp [m]
  `(range ~(rewrite (:start m)) (inc ~(rewrite (:stop m)))))

(defrewrite :JaArray [m]
  (mapv rewrite (:exprs m)))

(defrewrite :JaQueryDef [m]
  `(~'bi/query '~(mapv rewrite (:params m)) '~(mapv rewrite (:triples m))))

(defrewrite :JaTriple [m]
  `[~(rewrite (:ent m))
    ~(rewrite (:rel m))
    ~(rewrite (:val-exp m))])

(defrewrite :JaEnforceDef [m]
    `(~'bi/enforce {:params ~(-> m :params rewrite)
                    :body ~(binding [in-enforce? true] (-> m :body rewrite))}))

;;; This puts metadata on the function form for use by $map, $filter, $reduce, etc.
;;; User functions also translate using this, but don't use the metadata.
(defrewrite :JaFnDef [m]
  (let [vars (mapv #(-> % :jvar-name symbol) (:vars m))
        body (-> m :body rewrite)]
    `(-> (fn ~(mapv rewrite (:vars m)) ~body)
      (with-meta {:params '~vars :body '~body}))))

(defrewrite :JaTripleRole [m] (:role-name m))

(defrewrite :JaImmediateUse [m]
  `(~(-> m :def rewrite) ~@(->> m :args (map rewrite))))

(defrewrite :JaConditionalExp [m]
  `(if ~(-> m :predicate rewrite)
    ~(-> m :exp1 rewrite)
    ~(-> m :exp2 rewrite)))

(defn atomic?
  "This is mostly to speed up stepping through walk-for-bvecs!"
  [exp]
  (or (string? exp)
      (number? exp)
      (keyword? exp)
      (and (map? exp) (#{:JaField :JaJvar} (:_type exp)))))

;;;----------------------------- Rewriting binary operations (the remainder of this file) -------------

(declare rewrite-bvec-as-sexp precedence op-precedence-tbl)

;;; Filter path elements (at least(?) -- maybe map and reduce too) consist of a field path element
;;; followed by the body enclosed in the delimiters; the two are not separate path elements.
(defrewrite :JaBinOpSeq [m]
  (->> m :seq rewrite-bvec-as-sexp)) ; This orders element and rewrites them to s-expressions.

;;; JaPath are created in gather-paths.
(defrewrite :JaPath [m]
  `(bi/step->
    ~@(->> m
           :path
           (remove #(or (symbol? %) (keyword? %)))
           (map rewrite))))

(defrewrite :JaApplyMap [m]
  (reset-dgensym!)
  (let [sym (dgensym!)
        body (-> m :body rewrite)]
    `(bi/apply-map
      (fn [~sym]
        (bi/with-context ~sym ~@body)))))

(defrewrite :JaApplyFilter [m]
  (reset-dgensym!)
  (let [sym (dgensym!)
        body (-> m :body rewrite)]
    `(bi/apply-filter
      ~(-> m :operand rewrite)
      (fn [~sym] (bi/with-context ~sym ~body)))))

(defrewrite :JaApplyReduce [m]
  (let [sym (dgensym!)
        body (-> m :body rewrite)]
    `(bi/apply-reduce
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

(defn gather-paths
  "Step through the bvec and collect segments that include :path?=true operators/operands into
   JaPath objects."
  [bvec]
  (loop [bv bvec
         res []]
    (cond (empty? bv) res
          (-> bv first op-precedence-tbl :path?)
          (let [steal (last res)                           ; Last operand belongs with path...
                actual (subvec res 0 (-> res count dec))   ; ...so this is what res should be before gather path.
                collected (loop [bv2 bv
                                 path [steal]]
                            (cond (empty? bv2) path
                                  (and (contains? op-precedence-tbl (first bv2))
                                       (-> bv2 first op-precedence-tbl :path? not)) path
                                  :else
                                  (recur (drop 1 bv2)
                                         (-> path (conj (first bv2))))))]
            (recur (drop (-> collected count dec) bv)
                   (conj actual (rewrite {:_type :JaPath :path collected}))))
            :else
            (recur (drop 1 bv)
                   (conj res (first bv))))))

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
  (reset! diag (-> bvec gather-paths basic-info)))

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

;;; A lower :val means tighter binding. For example 1+2*3 means 1+(2*3) because * (300) binds tighter than + (400).
;;; Precedence ordering is done *within* rewriting, thus it is done with par symbols, not the bi ones.
(def op-precedence-tbl ; lower :val means binds tighter.
  {:or               {:path? false :assoc :left :val 1000}
   :and              {:path? true  :assoc :left :val 900}
   '<                {:path? false :assoc :none :val 800}
   '>                {:path? false :assoc :none :val 800}
   :<=               {:path? false :assoc :none :val 800}
   :>=               {:path? false :assoc :none :val 800}
   '=                {:path? false :assoc :none :val 800}
   :!=               {:path? false :assoc :none :val 800}
   :in               {:path? false :assoc :none :val 700}
   'bi/thread        {:path? true  :assoc :left :val 700} ; ToDo guessing
   'bi/&             {:path? false :assoc :left :val 400} ; ToDo guessing
   'bi/+             {:path? false :assoc :left :val 400}
   'bi/-             {:path? false :assoc :left :val 400}
   :range            {:path? false :assoc :left :val 400} ; ToDo guessing
   'bi/*             {:path? false :assoc :left :val 300}
   'bi/%             {:path? false :assoc :left :val 300}
   'bi//             {:path? false :assoc :left :val 300}
   #_#_'bi/step->    {:path? true  :assoc :left :val 150}
   'bi/dot-map       {:path? true  :assoc :left :val 150}
   #_#_'bi/apply-map     {:path? true  :assoc :left :val 150}
   #_#_'bi/apply-filter  {:path? true  :assoc :left :val 100}
   :apply-map        {:path? true  :assoc :left :val 100}
   :apply-filter     {:path? true  :assoc :left :val 100}
   :apply-reduce     {:path? true  :assoc :left :val 100}})

(defn precedence [op]
  (if (contains? op-precedence-tbl op)
    (-> op op-precedence-tbl :val)
    (do (println "****** No precedence:" op)
        (reset! diag op)
        100)))
