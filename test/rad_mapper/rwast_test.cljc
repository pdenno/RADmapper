(ns rad-mapper.rwast-test
  (:require
   [clojure.test                 :refer [deftest testing is]]
   [rad-mapper.evaluate  :as ev  :refer [processRM]]
   [rad-mapper.rwast     :as rwa :refer [rwast]]))

(def example-reduceKV
  (processRM :ptag/exp
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
