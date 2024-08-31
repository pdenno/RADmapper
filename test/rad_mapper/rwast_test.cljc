(ns rad-mapper.rwast-test
  (:require
   [clojure.test                 :refer [deftest testing is]]
   [rad-mapper.builtin   :as bi]
   [rad-mapper.rwast     :as rwa :refer [rwast]]))

;;; ===============================================================================================
;;; Currently, owing to ns-setup! defined below, this is the namespace you should set your REPL to.
;;; ===============================================================================================
#?(:clj (def alias? (atom (-> (ns-aliases *ns*) keys set))))

#?(:clj (defn safe-alias
          [al ns-sym]
          (when (and (not (@alias? al))
                     (find-ns ns-sym))
            (alias al ns-sym))))

#?(:clj (defn ^:diag ns-setup!
          "Use this to setup useful aliases for working in this NS."
          []
          (reset! alias? (-> (ns-aliases *ns*) keys set))
          (safe-alias 'io     'clojure.java.io)
          (safe-alias 's      'clojure.spec.alpha)
          (safe-alias 'uni    'clojure.core.unify)
          (safe-alias 'edn    'clojure.edn)
          (safe-alias 'io     'clojure.java.io)
          (safe-alias 'str    'clojure.string)
          (safe-alias 'd      'datahike.api)
          (safe-alias 'dp     'datahike.pull-api)
          (safe-alias 'mount  'mount.core)
          (safe-alias 'p      'promesa.core)
          (safe-alias 'px     'promesa.exec)
          (safe-alias 'bi     'rad-mapper.builtin)
          (safe-alias 'openai 'wkok.openai-clojure.api)))


(def example-reduceKV
  (bi/processRM :ptag/exp
   "( $order := {'name'            : 'Example Customer',
                 'shippingAddress' : '123 Mockingbird Lane...',
                 'item part no.'   : 'p12345',
                 'qty'             : {'amt' : 4, 'uom' : 'unit'}};

      $name2CustomerFn := function($res, $k, $v)
                            { ($k = 'name') ? $assoc($res, 'customer', $v) : $assoc($res, $k, $v) };

      $reduceKV($name2CustomerFn, {}, $order)
    )"))

(def ^:diag example-map-reduceKV
  (bi/processRM :ptag/exp
   "( $order := {'name'            : 'Example Customer',
                 'shippingAddress' : '123 Mockingbird Lane...',
                 'item part no.'   : 'p12345',
                 'qty'             : {'amt' : 4, 'uom' : 'unit'}};

      $name2CustomerFn := function($res, $k, $v)
                            { ($k = 'name') ? $assoc($res, 'customer', $v) : $assoc($res, $k, $v) };

      $reduceKV($name2CustomerFn, {}, $order)
    )"))

(deftest rwast-rewriting
  (testing "Testing rewriting."
    (testing " Testing example $reduceKV"
      (is (= {:Block
              [{:VarDecl {:VarName "$order",
                          :VarValue {:Obj {"name" "Example Customer",
                                           "shippingAddress" "123 Mockingbird Lane...",
                                           "item part no." "p12345",
                                           "qty" {:Obj {"amt" "4",
                                                        "uom" "unit"}}}}}}
               {:VarDecl {:VarName "$name2CustomerFn",
                          :VarValue
                          {:FnDef {:Params ["$res" "$k" "$v"]},
                           :Body {:IfExp {:Predicate {:Block [{:BinaryExpression ["$k" :op/eq "name"]}]},
                                          :Then {:FnCall {:Args ["$res" "customer" "$v"], :FnName "$assoc"}},
                                          :Else {:FnCall {:Args ["$res" "$k" "$v"], :FnName "$assoc"}}}}}}}
               {:FnCall {:Args ["$name2CustomerFn" {:Obj {}} "$order"], :FnName "$reduceKV"}}]}
             (rwast example-reduceKV))))))

(def answer
  "{\"rm.Block\":
    [{\"rm.VarDecl\":
      {\"rm.VarName\":\"$order\",
       \"rm.VarValue\":
       {\"rm.Object\":
        {\"name\":\"Example Customer\",
         \"shippingAddress\":\"123 Mockingbird Lane...\",
         \"item part no.\":\"p12345\",
         \"qty\":{\"rm.Object\":{\"amt\":\"4\", \"uom\":\"unit\"}}}}}},
     {\"rm.VarDecl\":
      {\"rm.VarName\":\"$name2CustomerFn\",
       \"rm.VarValue\":
       {\"rm.FnDef\":{\"rm.Params\":[\"$res\", \"$k\", \"$v\"]},
        \"rm.Body\":
        {\"rm.IfExp\":
         {\"rm.Predicate\":
          {\"rm.Block\":[{\"rm.BinaryExpression\":[\"$k\", \"op.eq\", \"name\"]}]},
          \"rm.Then\":{\"rm.$assoc\":{\"rm.args\":[\"$res\", \"customer\", \"$v\"]}},
          \"rm.Else\":{\"rm.$assoc\":{\"rm.args\":[\"$res\", \"$k\", \"$v\"]}}}}}}},
     {\"rm.$reduceKV\":
      {\"rm.args\":[\"$name2CustomerFn\", {\"rm.Object\":{}}, \"$order\"]}}]}")
