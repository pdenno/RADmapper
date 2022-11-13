(ns rad-mapper.builtins-macros
  (:require
   [clojure.string :as str]))

(def ^:dynamic $  "JSONata context variable" (atom :orig-val))
(def $$ "JSONata root context." (atom :bi/unset))

(defn set-context!
  "Set the JSONata context variable to the argument.
   If the root context has not yet been set, set that too.
   Returns argument."
  [val]
  (reset! $ val)
  (when (= @$$ :bi/unset) (reset! $$ val))
  val)

(defn containerize [obj] (-> obj (with-meta (merge (meta obj) {:bi/container? true}))))
(defn container?   [obj] (-> obj meta :bi/container?))

(defn containerize?
  "If obj is a vector, set  metadata :bi/container?."
  [obj]
  (if (vector? obj)
    (with-meta obj (merge (meta obj) {:bi/container? true}))
    obj))

(defn flatten-except-json
  "Adapted from core/flatten:
   Takes any nested combination of sequential things (lists, vectors, etc.)
   and returns their contents as a single, flat lazy sequence.
   (flatten nil) returns an empty sequence.
   EXCEPTION: If the thing is a vector with metadata :type :bi/json-array, it isn't flattened. "
  [x]
  (let [m (meta x)]
    (letfn [(seq-except? [o]
              (and (sequential? o)
                   (not (-> o meta :bi/json-array?))))]
      (-> (remove seq-except? (tree-seq seq-except? seq x))
          vec
          (with-meta m)))))

(defn jflatten
  "Accommodate JSONata's quirky equivalence in behavior of scalars and arrays containing one object.
   Note that this is only applied for results of mapping (called 'containers'), not ordinary access.
   - ordinary access            : {'nums'   : [[1], 2, 3]}.nums[0]
   - container (because using $): [[1, 2, 3] 4].$

   See http://docs.jsonata.org/processing section 'Sequences', which currenlty reads as follows:
       The sequence flattening rules are as follows:

    1) An empty sequence is a sequence with no values and is considered to be 'nothing' or 'no match'.
       It won't appear in the output of any expression.
       If it is associated with an object property (key/value) pair in a result object, then that object will not have that property.

    2) A singleton sequence is a sequence containing a single value.
       It is considered equivalent to that value itself, and the output from any expression, or sub-expression will be
       that value without any surrounding structure.

    3) A sequence containing more than one value is represented in the output as a JSON array.
       This is still internally flagged as a sequence and subject to the next rule.
       Note that if an expression matches an array from the input JSON, or a JSON array is explicitly constructed in
       the query using the array constructor, then this remains an array of values rather than a sequence of values and
       will not be subject to the sequence flattening rules.
       However, if this array becomes the context of a subsequent expression, then the result of that will be a sequence.

    4) If a sequence contains one or more (sub-)sequences, then the values from the sub-sequence are pulled up to
       the level of the outer sequence. A result sequence will never contain child sequences (they are flattened)."
  [obj]
  (letfn [(elim-empty [o] ; rule-1
            (let [m (meta o)]
              (cond (map? o) (-> (reduce-kv
                                  (fn [m k v]
                                    (if (or (nil? v) (and (coll? v) (empty? v))) m (assoc m k (elim-empty v))))
                                  {}
                                  o)
                                 (with-meta m)),
                    (vector? o) (-> (->> o (remove nil?) (mapv elim-empty)) (with-meta m))
                    :else o)))]
    (cond (container? obj)
          (let [len (count obj)]
            (cond (== 0 len) nil ; Or should I call it ::no-match ? Rule 1
                  (== 1 len) (-> obj first elim-empty) ; (-> Rule 2, Rule 1)
                  :else (-> (elim-empty obj) flatten-except-json containerize))) ; Rule 1, Rule 3 JSON array.

          (vector? obj) (let [m   (meta obj)
                              obj (-> (->> obj (remove nil?) vec) (with-meta m))
                              len (count obj)]
                          (cond (== 0 len) nil ; Or should I call it ::no-match ? Rule 1
                                ;;(== 1 len) (first obj)
                                :else obj))
          :else obj)))


(defmacro defn*
  "Convenience macro for numerical operators. They can be passed functions."
  [fn-name doc-string [& args] & body]
  `(def ~fn-name
     ~doc-string
     (fn [~@args]
       (let [~@(mapcat #(list % `(-> (if (fn? ~%) (~%) ~%) jflatten)) args)]
         ~@(map #(list 'clojure.spec.alpha/assert ::number %) args)
         ~@body))))

(defmacro defn$
  "Define two function arities using the body:
     (1) the ordinary one, that has the usual arguments for the built-in, and,
     (2) a function where the missing argument will be assumed to be the context variable, $.
   The parameter ending in a \\_ is the one elided in (2). (There must be such a parameter.)
   doc-string is required."
  [fn-name doc-string [& params] & body]
  (let [param-map (zipmap params (map #(symbol nil (name %)) params))
        abbrv-params (vec (remove #(str/ends-with? (str %) "_") (vals param-map)))
        abbrv-args (mapv #(if (str/ends-with? (str %) "_") '@$ %) (vals param-map))
        fn-name (symbol nil (name fn-name))]
    (letfn [(rewrite [x]
              (cond (seq? x)    (map  rewrite x)
                    (vector? x) (mapv rewrite x)
                    (map? x)    (reduce-kv (fn [m k v] (assoc m (rewrite k) (rewrite v))) {} x)
                    :else (get param-map x x)))]
    `(defn ~fn-name ~doc-string
       (~abbrv-params (~fn-name ~@abbrv-args))
       ([~@(vals param-map)]
        (let [res# (do ~@(rewrite body))] (if (seq? res#) (doall res#) res#)))))))

(defmacro thread-m [x y]
  `(let [xarg# ~x]
     (do (set-context! xarg#)
         (let [yarg# ~y]
           (if (fn? yarg#)
             (yarg# xarg#)
             (throw (ex-info "The RHS argument to the threading operator is not a function."
                             {:rhs-operator yarg#})))))))

(defmacro value-step-m
  [body]
  `(-> (fn [& ignore#] ~body)
       (with-meta {:bi/step-type :bi/value-step :body '~body})))

(defmacro primary-m [body]
  `(-> (fn [& ignore#] ~body)
       (with-meta {:bi/step-type :bi/primary})))

(defmacro init-step-m [body]
  `(-> (fn [_x#] ~body)
       (with-meta {:bi/step-type :bi/init-step :bi/body '~body})))

(defmacro map-step-m [body]
  `(-> (fn [_x#] ~body)
       (with-meta {:bi/step-type :bi/map-step :body '~body})))
