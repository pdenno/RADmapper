(ns rad-mapper.rewrite-test
  "Test the rewrite of parse trees. rew/rewrite* is a toplevel function"
  (:require
   [clojure.test       :refer  [deftest is testing]]
   [rad-mapper.builtins :as bi]
   [rad-mapper.rewrite  :as rew]))

;;; (rad-mapper.rewrite-test/for-testing-in-par)
(defn for-testing-in-par
  "Convenience for REPL-based exploration while in rad-mapper.parse"
  []
  (binding [*ns* (find-ns 'rad-mapper.parse)]
    (alias 'bi  'rad-mapper.builtins)
    (alias 'rew 'rad-mapper.rewrite)))

(deftest binary-reordering
  (testing "Testing that we reorder infix to prefix syntax correctly."
    (let [bflat {:_type :BFLAT, ; This is "$var.a + b.c.(P * Q)"  (assumes b is in $)
                 :bf [{:_type :JaVar, :var-name "$var"}
                      \.
                      {:_type :JaField, :field-name "a"}
                      \+
                      {:_type :JaField, :field-name "b"}
                      \.
                      {:_type :JaParenDelimitedExp,  ; This is mapping
                       :exp {:_type :BFLAT,
                             :bf [{:_type :JaField, :field-name "P"}
                                  \*
                                  {:_type :JaField, :field-name "Q"}]},
                       :operand {:_type :JaField, :field-name "c"}}]}]
      (is (= {:_type :BFLAT,
              :bf
              [{:_type :JaVar, :var-name "$var"}
               \.
               {:_type :JaField, :field-name "a"}
               \+
               {:_type :JaParenDelimitedExp,
                :exp {:_type :BFLAT, :bf [{:_type :JaField, :field-name "P"} \* {:_type :JaField, :field-name "Q"}]},
                :operand {:_type :BFLAT, :bf [{:_type :JaField, :field-name "b"} \. {:_type :JaField, :field-name "c"}]}}]}
             (rew/reorder-for-delimited-exps bflat))))))

;;; (not= #"abc" #"abc") so don't bother testing things containing regular expressions.
(deftest expression-rewrites
  (testing "Some :ptag/exp translations to clj"
    (binding [bi/*test-sym* 'foo]
      (is (= 1                                                    (rew/rewrite* :ptag/exp "1" :rewrite? true)))
      (is (= [1 2 3]                                              (rew/rewrite* :ptag/exp "[1, 2, 3]" :rewrite? true)))
      (is (= '(bi/+ 1 2)                                          (rew/rewrite* :ptag/exp "1 + 2"  :rewrite? true)))
      (is (= '(range 1 (inc 5))                                   (rew/rewrite* :ptag/exp "[1..5]" :rewrite? true)))
      (is (= '(bi/filter-aref $A 1)                               (rew/rewrite* :ptag/exp "$A[1]"  :rewrite? true)))
      (is (= '(bi/$sum (bi/access $v "field"))                    (rew/rewrite* :ptag/exp "$sum($v.field)" :rewrite? true)))
      (is (= '(bi/$sum (bi/access (bi/access "a") "b"))           (rew/rewrite* :ptag/exp "$sum(a.b)" :rewrite? true)))
      (is (= '(bi/* (bi/access "A") (bi/access "B"))              (rew/rewrite* :ptag/exp "(A * B)" :rewrite? true)))
      (is (= '(bi/thread 4 ($f))                                  (rew/rewrite* :ptag/exp "4 ~> $f()" :rewrite? true)))
      (is (= 'bi/$                                                (rew/rewrite* :ptag/exp "$"   :rewrite? true)))
      (is (= 'bi/$$                                               (rew/rewrite* :ptag/exp "$$"  :rewrite? true)))
      (is (= 'bi/$$$                                              (rew/rewrite* :ptag/exp "$$$" :rewrite? true)))
      (is (= '$foo                                                (rew/rewrite* :ptag/exp "$foo" :rewrite? true)))

      ;; This one actually tests the tokenizer! \? and \: are problematic owing to use in qvars and triple roles.
      (is (= '(if true "a" "b") (rew/rewrite* :ptag/exp "true?'a':'b'" :rewrite? true)))
      ;; ToDo: This one doesn't work yet, but it tokenizes. I think the problem is that triple-roles are literals yet.
      (is (= '(if true :foo/bar :foo/bat) (rew/rewrite* :ptag/exp "true?:foo/bar::foo/bat" :rewrite? true)))

      ;; Testing the synax reordering of map/filter on paths
      (is (= '(mapv (fn [foo] (bi/* (bi/access foo "P") (bi/access foo "Q"))) $var)
             (rew/rewrite* :ptag/exp "$var.(P * Q)" :rewrite? true)))

      (is (= '(mapv (fn [foo] (bi/* (bi/access foo "A") (bi/access foo "B"))) $data)
             (rew/rewrite* :ptag/exp "$data.(A * B)" :rewrite? true)))

      (is (= '(bi/+ (bi/access (bi/access (bi/access (bi/access "a") "b") "c") "d") (bi/access (bi/access "e") "f"))
             (rew/rewrite* :ptag/exp "a.b.c.d + e.f" :rewrite? true)))

      (is (= '(bi/+ (bi/access "a") (bi/* (bi/access "b") ($f (bi/+ (bi/access "c") (bi/access "d")))))
             (rew/rewrite* :ptag/exp "a + b * $f(c + d)" :rewrite? true)))

      (is (= '(bi/+ (bi/access $var "a")
                    (mapv (fn [foo] (bi/* (bi/access foo "P") (bi/access foo "Q")))
                          (bi/access (bi/access "b") "c")))
             (rew/rewrite* :ptag/exp "$var.a + b.c.(P * Q)" :rewrite? true)))

      (is (= '(bi/+ (bi/+ (bi/access $var "a")
                          (mapv (fn [foo] (bi/* (bi/access foo "P") (bi/access foo "Q")))
                                (bi/access (bi/access "b") "c")))
                    (bi/access "d"))
             (rew/rewrite* :ptag/exp "$var.a + b.c.(P * Q) + d" :rewrite? true)))

      (is (= '(bi/+ (bi/+ (bi/access $var "a")
                          (mapv (fn [foo] (bi/* (bi/access foo "P") (bi/access foo "Q")))
                                (bi/access (bi/access "b") "c")))
                    (mapv (fn [foo] (bi/* (bi/access foo "M") (bi/access foo "N")))
                          (bi/access "d")))
             (rew/rewrite* :ptag/exp "$var.a + b.c.(P * Q) + d.(M * N)" :rewrite? true)))

      (is (= '(mapv (fn [foo] (bi/* (bi/access foo "P") (bi/access foo "Q"))) (bi/access (bi/access (bi/access $var "a") "b") "c"))
             (rew/rewrite* :ptag/exp "$var.a.b.c.(P * Q)" :rewrite? true)))

      (is (= '(bi/$sum (mapv (fn [foo] (bi/* (bi/access foo "P") (bi/access foo "Q"))) (bi/access $v "a")))
             (rew/rewrite* :ptag/exp "$sum($v.a.(P * Q))" :rewrite? true)))

      ;; ToDo: is with-meta sill useful here?
      ;; Miscellaneous other tests.
      (is (= '(-> (fn [$v $i $a] (< (bi/access $v "cbc_InvoicedQuantity") 0))
                  (with-meta {:params '[$v $i $a], :body '(< (bi/access $v "cbc_InvoicedQuantity") 0)}))
             (rew/rewrite* :ptag/fn-def "function($v,$i,$a) { $v.cbc_InvoicedQuantity < 0 }" :rewrite? true)))

      (is (= '(let [$inc (-> (fn [$v] (bi/+ $v 1)) (with-meta {:params '[$v], :body '(bi/+ $v 1)}))]
                (bi/$map [1 2 3] $inc))
             (rew/rewrite* :ptag/code-block "($inc := function($v) { $v + 1}; $map([1, 2, 3], $inc))" :rewrite? true)))

      (is (= '(bi/$reduce (range 1 (inc 5)) $sum 100)
             (rew/rewrite* :ptag/exp "$reduce([1..5], $sum, 100)" :rewrite? true)))

      (is (= '($fn1 (bi/access (bi/access ($fn2 $v) "a") "b"))
             (rew/rewrite* :ptag/exp "$fn1($fn2($v).a.b)" :rewrite? true)))

      (is (= '(bi/$sum
               (bi/access
                (bi/access
                 (bi/$filter
                  (bi/access $v "InvoiceLine")
                  (-> (fn [$v $i $a] (< (bi/access $v "Quantity") 0))
                      (with-meta {:params '[$v $i $a], :body '(< (bi/access $v "Quantity") 0)})))
                 "Price")
                "PriceAmount"))
             (rew/rewrite* :ptag/exp "$sum($filter($v.InvoiceLine, function($v,$i,$a) { $v.Quantity < 0 }).Price.PriceAmount)"
                           :rewrite? true))))))

(deftest code-block
  (testing "Testing that code blocks handle binding and special jvars correctly"

    ;; Simple test
    (is (= '(do (let [$x 1] ($f $x) ($g $x)))
           (rew/rewrite* :ptag/code-block  "( $x := 1; $f($x) $g($x) )" :rewrite? true)))

    ;; Mix of specials and locals entails nesting
    (is (= '(do (let [$x "foo" $xx "xfoo"]
                  (bi/reset-special! bi/$$$ "bar")
                  (let [$y "bat" $yy "ybat"])
                  ($f $x $y)))
           (rew/rewrite* :ptag/code-block
                         "( $x   :=  'foo';
                            $xx  := 'xfoo';
                            $$$  := 'bar';
                            $y   := 'bat';
                           $yy  := 'ybat';
                           $f($x, $y) )"
                         :rewrite? true)))))
              
              

