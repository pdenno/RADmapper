(ns rad-mapper.rewrite-test
  "Test the rewrite of parse trees. rew/rewrite* is a toplevel function"
  (:require
   [clojure.test       :refer  [deftest is testing]]
   [rad-mapper.builtins :as bi]
   [rad-mapper.rewrite  :as rew]))

(defn rew [exp & {:keys [simplify? execute? debug? debug-parse? skip-top?]}]
  "Default rewriting function"
  (let [rewrite? (not (or simplify? execute?))]
    (rew/rewrite* :ptag/exp exp
                  :simplify? simplify?
                  :rewrite? rewrite?
                  :execute? execute?
                  :debug? debug?
                  :skip-top? skip-top?
                  :debug-parse? debug-parse?)))

;;; (not= #"abc" #"abc") so don't bother testing things containing regular expressions.
(deftest simple
  (testing "Simple (rewrite):"
    (testing "miscellaneous simple rewrites"
      (binding [bi/*test-sym* 'foo]
        (is (= 1                                                    (rew "1")))
        (is (= [1 2 3]                                              (rew "[1, 2, 3]")))
        (is (= '(bi/+ 1 2)                                          (rew "1 + 2" )))
        (is (= '(range 1 (inc 5))                                   (rew "[1..5]")))
        (is (= '(bi/filter-aref $A 1)                               (rew "$A[1]" )))
        (is (= '(bi/$sum (bi/dot-map $v "field"))                   (rew "$sum($v.field)")))
        (is (= '(bi/$sum (bi/dot-map (bi/dot-map "a") "b"))         (rew "$sum(a.b)")))
        (is (= '(bi/* (bi/dot-map "A") (bi/dot-map "B"))            (rew "(A * B)")))
        (is (= '(bi/thread 4 ($f))                                  (rew "4 ~> $f()")))
        (is (= '(deref bi/$)                                        (rew "$"  )))
        (is (= '(deref bi/$$)                                       (rew "$$" )))
        (is (= '$foo                                                (rew "$foo")))))

    (testing "tokenizer"
      (is (= '(if true "a" "b") (rew "true?'a':'b'"))))

    ;; ToDo: This one doesn't work yet, but it tokenizes. See parse_test.clj.
    ;; I think the problem is that triple-roles are literals yet. I'm not sure that they should be.
    #_(testing "triple-role in conditional"
        (is (= '(if true :foo/bar :foo/bat) (rew "true?:foo/bar::foo/bat"))))

    (testing "the synax reordering of map/filter on paths"
      (is (= '(mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q"))) $var)
             (rew "$var.(P * Q)")))

      (is (= '(mapv (fn [foo] (bi/* (bi/dot-map foo "A") (bi/dot-map foo "B"))) $data)
             (rew "$data.(A * B)")))

      (is (= '(bi/+ (bi/dot-map (bi/dot-map (bi/dot-map (bi/dot-map "a") "b") "c") "d") (bi/dot-map (bi/dot-map "e") "f"))
             (rew "a.b.c.d + e.f")))

      (is (= '(bi/+ (bi/dot-map "a") (bi/* (bi/dot-map "b") ($f (bi/+ (bi/dot-map "c") (bi/dot-map "d")))))
             (rew "a + b * $f(c + d)")))

      (is (= '(bi/+ (bi/dot-map $var "a")
                    (mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q")))
                          (bi/dot-map (bi/dot-map "b") "c")))
             (rew "$var.a + b.c.(P * Q)")))

      (is (= '(bi/+ (bi/+ (bi/dot-map $var "a")
                          (mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q")))
                                (bi/dot-map (bi/dot-map "b") "c")))
                    (bi/dot-map "d"))
             (rew "$var.a + b.c.(P * Q) + d")))

      (is (= '(bi/+ (bi/+ (bi/dot-map $var "a")
                          (mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q")))
                                (bi/dot-map (bi/dot-map "b") "c")))
                    (mapv (fn [foo] (bi/* (bi/dot-map foo "M") (bi/dot-map foo "N")))
                          (bi/dot-map "d")))
             (rew "$var.a + b.c.(P * Q) + d.(M * N)")))

      (is (= '(mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q"))) (bi/dot-map (bi/dot-map (bi/dot-map $var "a") "b") "c"))
             (rew "$var.a.b.c.(P * Q)")))

      (is (= '(bi/$sum (mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q"))) (bi/dot-map $v "a")))
             (rew "$sum($v.a.(P * Q))")))))

  (testing "miscellaneous"
    (is (= '(-> (fn [$v $i $a] (< (bi/dot-map $v "cbc_InvoicedQuantity") 0))
                (with-meta {:params '[$v $i $a], :body '(< (bi/dot-map $v "cbc_InvoicedQuantity") 0)}))
           (rew/rewrite* :ptag/fn-def "function($v,$i,$a) { $v.cbc_InvoicedQuantity < 0 }")))

    (is (= '(let [$inc (-> (fn [$v] (bi/+ $v 1)) (with-meta {:params '[$v], :body '(bi/+ $v 1)}))]
              (bi/$map [1 2 3] $inc))
           (rew "($inc := function($v) { $v + 1}; $map([1, 2, 3], $inc))")))

    (is (= '(bi/$reduce (range 1 (inc 5)) (-> (fn [$x $y] (bi/+ $x $y)) (with-meta {:params '[$x $y], :body '(bi/+ $x $y)})) 100)
           (rew "$reduce([1..5], function($x,$y){$x + $y}, 100)")))

    (is (= '($fn1 (bi/dot-map (bi/dot-map ($fn2 $v) "a") "b"))
           (rew "$fn1($fn2($v).a.b)")))

    (is (= '(bi/$sum
             (bi/dot-map
              (bi/dot-map
               (bi/$filter
                (bi/dot-map $v "InvoiceLine")
                (-> (fn [$v $i $a] (< (bi/dot-map $v "Quantity") 0))
                    (with-meta {:params '[$v $i $a], :body '(< (bi/dot-map $v "Quantity") 0)})))
               "Price")
              "PriceAmount"))
           (rew "$sum($filter($v.InvoiceLine, function($v,$i,$a) { $v.Quantity < 0 }).Price.PriceAmount)"
                )))))

(deftest code-block
  (testing "Testing that code blocks handle binding and special jvars correctly"

    ;; ToDo: I need to look at JSONata use of \; on this one.
    (is (= '(let [$x 1] ($f $x) ($g $x))
           (rew  "( $x := 1; $f($x) $g($x) )")))

    ;; A side-effected dummy binding is used for the special $.
    (is (= '(let [$x "foo" _x1 (bi/reset-special! bi/$ (-> {} (assoc "a" 1))) $y "bat" $yy "ybat"] ($f $x $y))
           (rew
                         "( $x   :=  'foo';
                            $    := {'a' : 1};
                            $y   := 'bat';
                            $yy  := 'ybat';
                            $f($x, $y) )"
                        )))))

(deftest nav-compression
  (testing "Testing compression of naviagation"
    (is (= '(bi/+ (bi/step-> ?tl (bi/dot-map "a") (bi/dot-map "b") (bi/dot-map "c"))
                  (bi/step-> ?tl (bi/dot-map "d") (bi/dot-map "e") (bi/dot-map "f")))
           (rew "a.b.c + d.e.f" :skip-top? true)))))


(deftest rewriting-apply
  (testing "rewrite apply a.b.(c + f)"
    (is (= '[{:_type :JaField, :field-name "a"}
             bi/step->
             {:_type :JaField, :field-name "b"}
             bi/apply-map
             {:_type :apply-map-fn,
              :body [{:_type :JaBinOpSeq,
                      :seq [{:_type :JaField, :field-name "c"} bi/+ {:_type :JaField, :field-name "f"}]}]}]
           ;; The arg here is (rew/rewrite* :ptag/exp "a.b.(c + f)" :simplify? true)
           (rew/rewrite-apply '[{:_type :JaField, :field-name "a"}
                                bi/step->
                                {:_type :JaField, :field-name "b"}
                                :apply-map
                                {:_type :JaCodeBlock,
                                 :body
                                 [{:_type :JaBinOpSeq,
                                   :seq
                                   [{:_type :JaField, :field-name "c"}
	                            bi/+
	                            {:_type :JaField, :field-name "f"}]}]}])))))

           


