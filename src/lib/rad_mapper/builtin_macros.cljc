(ns rad-mapper.builtin-macros
  (:require
   [clojure.string  :as str]
   [promesa.core    :as p]
   [taoensso.timbre :as log :refer-macros[error debug info log!]]))

(def ^:dynamic $  "JSONata context variable" (atom :orig-val)) ; It really is both dynamic and an atom!
(def $$ "JSONata root context." (atom :bi/unset))

(defn set-context!
  "Set the JSONata context variable to the argument (using p/then if it is a promise).
   If the root context has not yet been set, set that too.
   Returns argument."
  [val]
  (let [res (if (p/promise? val)
              (p/then val #(reset! $ %))
              (reset! $ val))]
    (when (= @$$ :bi/unset)
      (if (p/promise? val)
        (p/then val #(reset! $$ %))
        (reset! $$ val)))
    res))

#_(defn set-context!
  "Set the JSONata context variable to the argument (using p/then if it is a promise).
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

;;; ToDo: Is this really necessary?
(defmacro defn1
  "Convenience macro for numerical operators. They can be passed functions." ; <==== or promises?
  [fn-name doc-string [& args] & body]
  `(def ~fn-name
         ~doc-string
     (fn [~@args]
       (if (some p/promise? [~@args])
         (-> (p/all [~@args]) (p/then (fn [[~@args]] ~@body)))
         (do ~@body)))))

#_(defmacro defn1
  "Convenience macro for numerical operators. They can be passed functions." ; <==== or promises?
  [fn-name doc-string [& args] & body]
  `(def ~fn-name
         ~doc-string
     (fn [~@args]
       (if (some p/promise? [~@args])
         (-> (p/all [~@args]) (p/then (fn [[~@args]] ~@body)))
         (do ~@body)))))

(def threading? "Set in bi/thread and used in defn* macro to decide whether to use the context variable $ or return a partial." (atom false))

;;; ToDo: Promise code here not shown to help.
(defn deref$
  "Dereference or set the $ atom.
   Expressions such as [[1,2,3], [1]].$ will translate to (bi/map-steps-m (deref$))
   making it advantageous to have a deref that sets the value and returns it.
   (That expression flattens the list to [1 2 3 1], BTW.)"
  ([]     (if (p/promise? @$)
            (p/then @$ #(containerize? %))
            (containerize? @$)))
  ([val]  (if (p/promise? @$)
            (p/then @$ (fn [_] (set-context! (containerize? val))))
            (set-context! (containerize? val)))))

#_(defmacro defn*
  "Define two function arities using the body:
     (1) the ordinary one, that has the usual arguments for the built-in, and,
     (2) a function where the missing argument will be assumed to be the context variable, $.
   The parameter ending in a \\_ is the one elided in (2). (There must be such a parameter.)
   doc-string is required."
  [fn-name doc-string [& params] & body]
  (let [param-map (zipmap params (map #(symbol nil (name %)) params))
        abbrv-params (vec (remove #(str/ends-with? (str %) "_") (vals param-map)))
        abbrv-args (mapv #(if (str/ends-with? (str %) "_") '(deref$) %) (vals param-map))
        fn-name  (->> fn-name name (symbol nil))
        fn-name- (gensym fn-name)]
    (letfn [(rewrite [x]
              (cond (seq? x)    (map  rewrite x)
                    (vector? x) (mapv rewrite x)
                    (map? x)    (reduce-kv (fn [m k v] (assoc m (rewrite k) (rewrite v))) {} x)
                    :else (get param-map x x)))]
      `(letfn [(~fn-name- [~@(vals param-map)]
                (let [res# (do ~@(rewrite body))]
                  (cond (p/promise? res#)          (-> res#
                                                       (p/then #(if (seq? %) (doall %) %))
                                                       (p/catch #(throw (ex-info ~(str fn-name) {:err %}))))
                        (seq? res#)                (doall res#)
                        :else                      res#)))]
         (defn ~fn-name ~doc-string
           (~abbrv-params
            (if @threading?
              (partial ~fn-name- ~@abbrv-params)
              (if (p/promise? @$)
                (-> @$ (p/then (fn [_#] (~fn-name- ~@abbrv-args)))) ; This one has a @$ in it somewhere (see above).
                (~fn-name- ~@abbrv-args)))) ; This one has a @$ in it somewhere (see above).
           ([~@(vals param-map)]
            (if (p/promise? @$) ; <========== THIS HAS NOT BEEN SHOWN TO HELP! Keep the implementation below ========================
              (-> @$ (p/then (fn [_#]
                               (let [p# (p/all (vector ~@(vals param-map)))]
                                 (-> p# (p/then (fn [_#] (~fn-name- ~@(vals param-map)))))))))
              (~fn-name- ~@(vals param-map)))))))))

(defmacro defn*
  "Define two function arities using the body:
     (1) the ordinary one, that has the usual arguments for the built-in, and,
     (2) a function where the missing argument will be assumed to be the context variable, $.
   The parameter ending in a \\_ is the one elided in (2). (There must be such a parameter.)
   doc-string is required."
  [fn-name doc-string [& params] & body]
  (let [param-map (zipmap params (map #(symbol nil (name %)) params))
        abbrv-params (vec (remove #(str/ends-with? (str %) "_") (vals param-map)))
        abbrv-args (mapv #(if (str/ends-with? (str %) "_") '(deref$) %) (vals param-map))
        fn-name  (->> fn-name name (symbol nil))
        fn-name- (gensym fn-name)]
    (letfn [(rewrite [x]
              (cond (seq? x)    (map  rewrite x)
                    (vector? x) (mapv rewrite x)
                    (map? x)    (reduce-kv (fn [m k v] (assoc m (rewrite k) (rewrite v))) {} x)
                    :else (get param-map x x)))]
      `(letfn [(~fn-name- [~@(vals param-map)]
                (let [res# (do ~@(rewrite body))]
                  (cond (p/promise? res#)          (-> res#
                                                       (p/then #(if (seq? %) (doall %) %))
                                                       (p/catch #(throw (ex-info ~(str fn-name) {:err %}))))
                        (seq? res#)                (doall res#)
                        :else                      res#)))]
         (defn ~fn-name ~doc-string
           (~abbrv-params
            (if @threading?
              (partial ~fn-name- ~@abbrv-params)
              (~fn-name- ~@abbrv-args))) ; This one has a @$ in it somewhere (see above).
           ([~@(vals param-map)]
            (~fn-name- ~@(vals param-map))))))))


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

;;; ToDo: Rethink use of primary-m, again? and run-steps.
;;;       Possibly more example of added complexity exist beyond this conditional.
;;;       Primary, owing to it produces a function, entails this extra complexity.
(defmacro conditional-m
  "Implement the JSONata-like <test> ? <then-exp> <else-exp>."
  [condition e1 e2]
  `(let [cond# ~condition
         true?# (if (fn? cond#) (cond#) cond#)]
     (cond (or (and (fn? true?#) (true?#))
               (and (not (fn? true?#)) true?#))   (let [res# ~e1]
                                                    (if (fn? res#) (res#) res#))
           :else                                (let [res# ~e2]
                                                  (if (fn? res#) (res#) res#)))))
