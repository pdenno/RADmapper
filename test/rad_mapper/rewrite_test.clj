(ns rad-mapper.rewrite-test
  "Test the rewrite of parse trees. rew/rewrite* is a toplevel function"
  (:require
   [clojure.test       :refer  [deftest is testing]]
   [rad-mapper.builtins :as bi]
   [rad-mapper.rewrite  :as rew]))

;;; (not= #"abc" #"abc") so don't bother testing things containing regular expressions.
(deftest expression-rewrites
  (testing "Some :ptag/exp translations to clj"
    (binding [bi/*test-sym* 'foo]
      (is (= 1                                                    (rew/rewrite* :ptag/exp "1" :rewrite? true)))
      (is (= [1 2 3]                                              (rew/rewrite* :ptag/exp "[1, 2, 3]" :rewrite? true)))
      (is (= '(bi/+ 1 2)                                          (rew/rewrite* :ptag/exp "1 + 2"  :rewrite? true)))
      (is (= '(range 1 (inc 5))                                   (rew/rewrite* :ptag/exp "[1..5]" :rewrite? true)))
      (is (= '(bi/filter-aref $A 1)                               (rew/rewrite* :ptag/exp "$A[1]"  :rewrite? true)))
      (is (= '(bi/$sum (bi/dot-map $v "field"))                   (rew/rewrite* :ptag/exp "$sum($v.field)" :rewrite? true)))
      (is (= '(bi/$sum (bi/dot-map (bi/dot-map "a") "b"))         (rew/rewrite* :ptag/exp "$sum(a.b)" :rewrite? true)))
      (is (= '(bi/* (bi/dot-map "A") (bi/dot-map "B"))            (rew/rewrite* :ptag/exp "(A * B)" :rewrite? true)))
      (is (= '(bi/thread 4 ($f))                                  (rew/rewrite* :ptag/exp "4 ~> $f()" :rewrite? true)))
      (is (= '(deref bi/$)                                        (rew/rewrite* :ptag/exp "$"   :rewrite? true)))
      (is (= '(deref bi/$$)                                       (rew/rewrite* :ptag/exp "$$"  :rewrite? true)))
      (is (= '$foo                                                (rew/rewrite* :ptag/exp "$foo" :rewrite? true)))

      ;; This one actually tests the tokenizer! \? and \: are problematic owing to use in qvars and triple roles.
      (is (= '(if true "a" "b") (rew/rewrite* :ptag/exp "true?'a':'b'" :rewrite? true)))
      
      ;; ToDo: This one doesn't work yet, but it tokenizes. See parse_test.clj.
      ;; I think the problem is that triple-roles are literals yet. I'm not sure that they should be. 
      #_(is (= '(if true :foo/bar :foo/bat) (rew/rewrite* :ptag/exp "true?:foo/bar::foo/bat" :rewrite? true)))

      ;; Testing the synax reordering of map/filter on paths
      (is (= '(mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q"))) $var)
             (rew/rewrite* :ptag/exp "$var.(P * Q)" :rewrite? true)))

      (is (= '(mapv (fn [foo] (bi/* (bi/dot-map foo "A") (bi/dot-map foo "B"))) $data)
             (rew/rewrite* :ptag/exp "$data.(A * B)" :rewrite? true)))
      
      (is (= '(bi/+ (bi/dot-map (bi/dot-map (bi/dot-map (bi/dot-map "a") "b") "c") "d") (bi/dot-map (bi/dot-map "e") "f"))
             (rew/rewrite* :ptag/exp "a.b.c.d + e.f" :rewrite? true)))

      (is (= '(bi/+ (bi/dot-map "a") (bi/* (bi/dot-map "b") ($f (bi/+ (bi/dot-map "c") (bi/dot-map "d")))))
             (rew/rewrite* :ptag/exp "a + b * $f(c + d)" :rewrite? true)))

      (is (= '(bi/+ (bi/dot-map $var "a")
                    (mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q")))
                          (bi/dot-map (bi/dot-map "b") "c")))
             (rew/rewrite* :ptag/exp "$var.a + b.c.(P * Q)" :rewrite? true)))

;;; Wrong  '(bi/+ (bi/dot-map $var "a") (bi/dot-map (bi/dot-map "b") (mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q"))) (bi/dot-map "c"))))

      (is (= '(bi/+ (bi/+ (bi/dot-map $var "a")
                          (mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q")))
                                (bi/dot-map (bi/dot-map "b") "c")))
                    (bi/dot-map "d"))
             (rew/rewrite* :ptag/exp "$var.a + b.c.(P * Q) + d" :rewrite? true)))

      (is (= '(bi/+ (bi/+ (bi/dot-map $var "a")
                          (mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q")))
                                (bi/dot-map (bi/dot-map "b") "c")))
                    (mapv (fn [foo] (bi/* (bi/dot-map foo "M") (bi/dot-map foo "N")))
                          (bi/dot-map "d")))
             (rew/rewrite* :ptag/exp "$var.a + b.c.(P * Q) + d.(M * N)" :rewrite? true)))

      (is (= '(mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q"))) (bi/dot-map (bi/dot-map (bi/dot-map $var "a") "b") "c"))
             (rew/rewrite* :ptag/exp "$var.a.b.c.(P * Q)" :rewrite? true)))

      (is (= '(bi/$sum (mapv (fn [foo] (bi/* (bi/dot-map foo "P") (bi/dot-map foo "Q"))) (bi/dot-map $v "a")))
             (rew/rewrite* :ptag/exp "$sum($v.a.(P * Q))" :rewrite? true)))

      ;; Miscellaneous other tests.
      (is (= '(-> (fn [$v $i $a] (< (bi/dot-map $v "cbc_InvoicedQuantity") 0))
                  (with-meta {:params '[$v $i $a], :body '(< (bi/dot-map $v "cbc_InvoicedQuantity") 0)}))
             (rew/rewrite* :ptag/fn-def "function($v,$i,$a) { $v.cbc_InvoicedQuantity < 0 }" :rewrite? true)))

      (is (= '(let [$inc (-> (fn [$v] (bi/+ $v 1)) (with-meta {:params '[$v], :body '(bi/+ $v 1)}))]
                (bi/$map [1 2 3] $inc))
             (rew/rewrite* :ptag/code-block "($inc := function($v) { $v + 1}; $map([1, 2, 3], $inc))" :rewrite? true)))

      (is (= '(bi/$reduce (range 1 (inc 5)) (-> (fn [$x $y] (bi/+ $x $y)) (with-meta {:params '[$x $y], :body '(bi/+ $x $y)})) 100)
             (rew/rewrite* :ptag/exp "$reduce([1..5], function($x,$y){$x + $y}, 100)" :rewrite? true)))

      (is (= '($fn1 (bi/dot-map (bi/dot-map ($fn2 $v) "a") "b"))
             (rew/rewrite* :ptag/exp "$fn1($fn2($v).a.b)" :rewrite? true)))

      (is (= '(bi/$sum
               (bi/dot-map
                (bi/dot-map
                 (bi/$filter
                  (bi/dot-map $v "InvoiceLine")
                  (-> (fn [$v $i $a] (< (bi/dot-map $v "Quantity") 0))
                      (with-meta {:params '[$v $i $a], :body '(< (bi/dot-map $v "Quantity") 0)})))
                 "Price")
                "PriceAmount"))
             (rew/rewrite* :ptag/exp "$sum($filter($v.InvoiceLine, function($v,$i,$a) { $v.Quantity < 0 }).Price.PriceAmount)"
                           :rewrite? true))))))

(deftest code-block
  (testing "Testing that code blocks handle binding and special jvars correctly"

    ;; ToDo: I need to look at JSONata use of \; on this one. 
    (is (= '(let [$x 1] ($f $x) ($g $x))
           (rew/rewrite* :ptag/code-block  "( $x := 1; $f($x) $g($x) )" :rewrite? true)))

    ;; A side-effected dummy binding is used for the special $.
    (is (= '(let [$x "foo" _x1 (bi/reset-special! bi/$ (-> {} (assoc "a" 1))) $y "bat" $yy "ybat"] ($f $x $y))
           (rew/rewrite* :ptag/code-block
                         "( $x   :=  'foo';
                            $    := {'a' : 1};
                            $y   := 'bat';
                            $yy  := 'ybat';
                            $f($x, $y) )"
                         :rewrite? true)))))

(deftest nav-compression
  (testing "Testing compression of naviagation"
    (is (= '(bi/+ (bi/step-> ?tl (bi/dot-map "a") (bi/dot-map "b") (bi/dot-map "c"))
                  (bi/step-> ?tl (bi/dot-map "d") (bi/dot-map "e") (bi/dot-map "f")))
           (rew/rewrite* :ptag/exp "a.b.c + d.e.f" :rewrite? true :skip-top? true)))))
