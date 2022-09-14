(ns rad-mapper.ast
  "Define syntax trees for interoperable exchange of mappings, among other purposes."
  (:require
   [failjure.core          :as fj]
   [rad-mapper.query       :as qu]
   [rad-mapper.rewrite     :as rew]))

(def diag (atom nil))
(declare rw-ast)

(def type2name
  {:JaConditionalExp "Conditional"
   :JaField          "FieldAccess"
   :JaFnCall         "FnCall"
   :JaBinOpExp       "BinaryExp"
   :JaFnDef          "FnDef"
   :JaJvar           "VarDef"
   :JaMapPair        "KVpair"

   :ArrayConstruction    "Array"
   :ObjectConstruction   "Object"
   :JaParenDelimitedExp  "ELIMINATED?"})

(def needs-analysis? #{:JaSquareDelimitedExp :JaCurlyDelimitedExp :JaParenDelimitedExp})

(defn actual-type
  "The objects for which needs-analysis? is true are rewritten to see what
  is actually being done."
  [m]
  (case (:_type m)
    :JaSquareDelimitedExp (if (:operand m)
                            (assoc m :_type :FilterExp)
                            (-> m
                                (assoc :_type :ArrayConstruction)
                                (assoc :elem (:exp m))
                                (dissoc :exp)))
    :JaParenDelimitedExp  (if (:operand m)
                            (assoc m :_type :MapExp)
                            (:exp m)) ; It is a "primary"
    :JaCurlyDelimitedExp  (-> m
                              (assoc :_type :ObjectConstruction)
                              (assoc :kv-pair (:exp m))
                              (dissoc :exp))))

(def char2op
  {\. :dot-map
   \& :str-concat
   \> :>
   \< :<
   \= :equality
   \+ :plus
   \- :minus
   \* :times
   \/ :divide})

(defn lookup-op [c]
  (if (contains? char2op c)
    (char2op c)
    (keyword (str "unknown-op" c))))

(defn rw-ast
  "Rewrite the AST (or more accurately concrete syntax tree) to
    (1) box primitives and regular expressions,
    (2) determine the actual type of xDelimtedExps, and
    (3) rename keys."
  [o]
  (cond (string? o)                           {:table/type :BoxedStr :BoxedStr/val o},
        (number? o)                           {:table/type :BoxedNum :BoxedNum/val o},
        (instance? java.util.regex.Pattern o) {:table/type :RegExp   :RegExp/val (str o)},
        (char?   o)                           (lookup-op o),
        (vector? o) (mapv rw-ast o),
        (map?    o) (as-> o ?o
                        (if (needs-analysis? (:_type ?o)) (actual-type ?o) ?o)
                        (if-let [obj-type (-> ?o :_type type2name)]
                          (reduce-kv (fn [m k v]
                                       (if (= k :_type)
                                         (assoc m :table/type (keyword obj-type))
                                         (assoc m (keyword obj-type (name k)) (rw-ast v))))
                                     {}
                                     ?o)
                          (rw-ast ?o))) ; If here actual-type probably turned it into a primitive.
        :else o))

;;; ToDo: :FnCall/fn-name needs this but doesn't work like the others!
(defn adjust-exp
  "Correct a few annoying things about rw-ast: vars and fields define strings;
   they don't have to be boxed."
  [o]
  (let [typ (:table/type o)]
    (cond (and (map? o) (#{:VarDef :FieldAccess :FnCall} typ))
          (->> (case typ
                 :FnCall (assoc o :FnCall/fn-name (-> o :FnCall/fn-name :BoxedStr/val))
                 :VarDef (-> o
                             (assoc  :VarDef/var-name (-> o :VarDef/jvar-name :BoxedStr/val))
                             (dissoc :VarDef/jvar-name))
                 :FieldAccess (assoc o :FieldAccess/field-name (-> o :FieldAccess/field-name :BoxedStr/val)))
               (reduce-kv (fn [m k v] (assoc m k (adjust-exp v))) {})),
          (map? o)      (reduce-kv (fn [m k v] (assoc m k (adjust-exp v))) {} o)
          (vector? o)   (mapv adjust-exp o)
          :else o)))

(defn set-indexes
  "A few object types, such as function calls and arrays, hold an ordered collection of elements.
   This returns the object with the elements of those sub-objects indexed."
  [o]
  (letfn [(updat [obj attr attr-ix]
            (update obj
                    attr
                    #(mapv (fn [e i] (assoc e attr-ix i))
                           %
                           (range 1 (-> % count inc)))))]
    (let [typ (:table/type o)]
      (cond (and (map? o) (#{:Array :FnCall :Object} typ))
            (->> (case typ
                   :Array  (updat o :Array/elem     :Array-elem/index)
                   :FnCall (updat o :FnCall/args    :FnCall-args/index)
                   :Object (updat o :Object/kv-pair :Object-kvpair/index))
                 (reduce-kv (fn [m k v] (assoc m k (set-indexes v))) {})),
            (map? o)      (reduce-kv (fn [m k v] (assoc m k (set-indexes v))) {} o),
            (vector? o)   (mapv set-indexes o),
            :else o))))

(defn mark-toplevel
  "Mark the toplevel expression as such."
  [obj]
  (cond (map? obj)            (assoc obj :table/toplevel-exp? true)
        (vector? obj)  (mapv #(assoc %   :table/toplevel-exp? true) obj)
        :else (fj/fail "Toplevel is a primitive type?: %s" obj)))

(def scott-result (rew/rewrite*
                   :ptag/exp "data/testing/map-examples/scott/shipped-item-instance-clean.json"
                   :file? true
                   :simplify? true))

(defn tryme-again
  []
  (-> scott-result
      rw-ast
      mark-toplevel
      adjust-exp
      set-indexes
      qu/db-for!))

(defn tryme-2
  []
  (-> scott-result
      rw-ast
      mark-toplevel
      adjust-exp
      set-indexes
      qu/learn-schema
      #_qu/db-for!))
