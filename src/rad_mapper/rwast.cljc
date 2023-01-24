(ns rad-mapper.rwast
  "Define 'interoperable' syntax trees from RM parse.cljc trees.
   rwast = Rewrite AST"
  (:require
   [clojure.data.json      :as json]
   [rad-mapper.evaluate    :as ev]
   [rad-mapper.util        :as util :refer [rwast-meth]]
   #?(:clj  [rad-mapper.rwast-macros :refer [defrwast *debugging?*]]))
   #?(:cljs (:require-macros [rad-mapper.rwast-macros :refer [defrwast *debugging?*]])))

(def diag (atom nil))
(declare rwast)

(def type2name
  {:ConditionalExp "Conditional"
   :Field          "FieldAccess"
   :FnCall         "FnCall"
   :BinOpExp       "BinaryExp"
   :FnDef          "FnDef"
   :Jvar           "VarDef"
   :MapPair        "KVpair"

   :ArrayConstruction  "Array"
   :ObjectConstruction "Object"
   :ParenDelimitedExp  "ELIMINATED?"})

(def needs-analysis? #{:SquareDelimitedExp :CurlyDelimitedExp :ParenDelimitedExp})

(defn actual-type
  "The objects for which needs-analysis? is true are rewritten to see what
  is actually being done."
  [m]
  (case (:typ m)
    :SquareDelimitedExp (if (:operand m)
                            (assoc m :typ :FilterExp)
                            (-> m
                                (assoc :typ :ArrayConstruction)
                                (assoc :elem (:exp m))
                                (dissoc :exp)))
    :ParenDelimitedExp  (if (:operand m)
                            (assoc m :typ :MapExp)
                            (:exp m)) ; It is a "primary"
    :CurlyDelimitedExp  (-> m
                              (assoc :typ :ObjectConstruction)
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

(defn rwast
  "Toplevel to rewrite the AST (or more accurately concrete syntax tree)."
  [o & keys]
  (cond (map?    o)               (if-let [typ (:typ o)] (rwast-meth typ o keys) o) ; Entry point for many situations.
        (string? o)               (str o)
        (number? o)               (str o)
        (util/regex? o)           (str o)
        (char?   o)               (lookup-op o),
        (vector? o)               (mapv rwast o),
        :else                      o))

(defrwast :Primary
  [m]
  {:Block (->> m :exps rwast)})

(defrwast :JvarDecl
  [m]
  {:VarDecl {:VarName (->> m :var rwast)
             :VarValue (->> m :init-val rwast)}})

(defrwast :Jvar [m] (:jvar-name m))

(defrwast :FnDef [m]
  {:FnDef {:Params (->> m :vars (mapv rwast))}
   :Body  (-> m :body rwast)})

(defrwast :ConditionalExp [m]
  {:IfExp {:Predicate (-> m :predicate rwast)
           :Then (-> m :exp1 rwast)
           :Else (-> m :exp2 rwast)}})

(defrwast :BinOpSeq [m]
  {:BinaryExpression (->> m :seq (mapv rwast))})

(defrwast :FnCall [m]
  {:FnCall {:Args (->> m :args (mapv rwast))
            :FnName (:fn-name m)}})

(defrwast :ObjExp [m]
  {:Obj (reduce (fn [m {:keys [key val]}]
                  (assoc m (rwast key) (rwast val)))
                {}
                (:kv-pairs m))})

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

(def example-ast
  {:typ :Primary,
   :exps
   [{:typ :JvarDecl,
     :var {:typ :Jvar, :jvar-name "$order"},
     :init-val
     {:typ :ObjExp,
      :kv-pairs
      [{:typ :KVPair, :key "name", :val "Example Customer"}
       {:typ :KVPair, :key "shippingAddress", :val "123 Mockingbird Lane..."}
       {:typ :KVPair, :key "item part no.", :val "p12345"}
       {:typ :KVPair, :key "qty", :val {:typ :ObjExp,
                                        :kv-pairs [{:typ :KVPair, :key "amt", :val 4}
                                                   {:typ :KVPair, :key "uom", :val "unit"}]}}]}}
    {:typ :JvarDecl,
     :var {:typ :Jvar, :jvar-name "$name2CustomerFn"},
     :init-val
     {:typ :FnDef,
      :vars [{:typ :Jvar, :jvar-name "$res"}
             {:typ :Jvar, :jvar-name "$k"}
             {:typ :Jvar, :jvar-name "$v"}],
      :body
      {:typ :ConditionalExp,
       :predicate {:typ :Primary, :exps [{:typ :BinOpSeq, :seq [{:typ :Jvar, :jvar-name "$k"} :op/eq "name"]}]},
       :exp1 {:typ :FnCall, :fn-name "$assoc", :args [{:typ :Jvar, :jvar-name "$res"}
                                                      "customer"
                                                      {:typ :Jvar, :jvar-name "$v"}]},
       :exp2 {:typ :FnCall, :fn-name "$assoc", :args [{:typ :Jvar, :jvar-name "$res"}
                                                      {:typ :Jvar, :jvar-name "$k"}
                                                      {:typ :Jvar, :jvar-name "$v"}]}}}}
    {:typ :FnCall, :fn-name "$reduceKV", :args [{:typ :Jvar, :jvar-name "$name2CustomerFn"}
                                                {:typ :ObjExp, :kv-pairs []}
                                                {:typ :Jvar, :jvar-name "$order"}]}]})

(defn tryme []
  (rwast example-ast))
