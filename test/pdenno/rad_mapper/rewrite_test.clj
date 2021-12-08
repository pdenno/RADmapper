(ns pdenno.rad-mapper.rewrite-test
  "Test the rewrite of parse trees. rew/rewrite* is a toplevel function"
  (:require
   [pdenno.rad-mapper.rewrite  :as rew]
   [clojure.test       :refer  [deftest is testing]]))

;;; (pdenno.rad-mapper.rewrite-test/for-testing-in-par)
(defn for-testing-in-par
  "Convenience for REPL-based exploration while in app.model.mmp"
  []
  (binding [*ns* (find-ns 'pdenno.rad-mapper.parse)]
    (alias 'bi  'pdenno.rad-mapper.builtins)
    (alias 'rew 'pdenno.rad-mapper.rewrite)))

;;; (not= #"abc" #"abc") so don't bother testing things containing regular expressions.
(deftest expression-rewrites
  (testing "Some :ptag/exp translations to clj"
    (is (= 1                                                    (rew/rewrite* :ptag/exp "1" :rewrite? true)))
    (is (= [1 2 3]                                              (rew/rewrite* :ptag/exp "[1, 2, 3]" :rewrite? true)))
    (is (= '(bi/+ 1 2)                                          (rew/rewrite* :ptag/exp "1 + 2"  :rewrite? true)))
    (is (= '(range 1 (inc 5))                                   (rew/rewrite* :ptag/exp "[1..5]" :rewrite? true)))
    (is (= '(bi/filter-aref $A 1)                               (rew/rewrite* :ptag/exp "$A[1]"  :rewrite? true)))
    (is (= '(bi/$sum (bi/access $v "field"))                    (rew/rewrite* :ptag/exp "$sum($v.field)" :rewrite? true)))
    (is (= '(bi/$sum (bi/access (bi/access "a") "b"))           (rew/rewrite* :ptag/exp "$sum(a.b)" :rewrite? true)))
    (is (= '(bi/+ (bi/access (bi/access (bi/access (bi/access "a") "b") "c") "d") (bi/access (bi/access "e") "f"))
           (rew/rewrite* :ptag/exp "a.b.c.d + e.f" :rewrite? true :debug? true)))
    (is (= '(bi/+ (bi/access "a") (bi/* (bi/access "b") ($f (bi/+ (bi/access "c") (bi/access "d")))))
           (rew/rewrite* :ptag/exp "a + b * $f(c + d)" :rewrite? true)))
    (is (= (rew/rewrite* :ptag/exp "$var.a + b.c.(P * Q)" :rewrite? true)
           '(bi/+ (bi/access $var "a") (bi/map-path (bi/* (bi/access "P") (bi/access "Q")) (bi/access (bi/access "b") "c")))))
    (is (= '(bi/+ (bi/+ (bi/access $var "a") (bi/map-path (bi/* (bi/access "P") (bi/access "Q")) (bi/access (bi/access "b") "c"))) (bi/access "d"))
           (rew/rewrite* :ptag/exp "$var.a + b.c.(P * Q) + d" :rewrite? true)))
    (is (= '(bi/+ (bi/+ (bi/access $var "a") (bi/map-path (bi/* (bi/access "P") (bi/access "Q")) (bi/access (bi/access "b") "c")))
                  (bi/map-path (bi/* (bi/access "M") (bi/access "N")) (bi/access "d")))
           (rew/rewrite* :ptag/exp "$var.a + b.c.(P * Q) + d.(M * N)" :rewrite? true)))
    (is (= '(bi/map-path (bi/* (bi/access "P") (bi/access "Q")) (bi/access (bi/access (bi/access $var "a") "b") "c"))
           (rew/rewrite* :ptag/exp "$var.a.b.c.(P * Q)" :rewrite? true)))
    (is (= '(-> (fn [$v $i $a] (< (bi/access $v "cbc_InvoicedQuantity") 0))
                             (with-meta {:params '[$v $i $a], :body '(< (bi/access $v "cbc_InvoicedQuantity") 0)}))
           (rew/rewrite* :ptag/fn-def "function($v,$i,$a) { $v.cbc_InvoicedQuantity < 0 }" :rewrite? true)))
    (is (= '(fn [] (let [$inc (-> (fn [$v] (bi/+ $v 1)) (with-meta {:params '[$v], :body '(bi/+ $v 1)}))]
                     (bi/$map [1 2 3] $inc)))
           (rew/rewrite* :ptag/CodeBlock "($inc := function($v) { $v + 1}; $map([1, 2, 3], $inc))" :rewrite? true)))
    (is (= '(bi/$sum
             (bi/access
              (bi/access
               (bi/$filter
                (bi/access $v "InvoiceLine")
                (-> (fn [$v $i $a] (< (bi/access $v "Quantity") 0))
                                 (with-meta {:params '[$v $i $a], :body '(< (bi/access $v "Quantity") 0)})))
               "Price")
              "PriceAmount"))
           (rew/rewrite* :ptag/exp "$sum($filter($v.InvoiceLine, function($v,$i,$a) { $v.Quantity < 0 }).Price.PriceAmount)" :rewrite? true)))
    (is (= '(bi/$sum (bi/map-path (bi/* (bi/access "P") (bi/access "Q")) (bi/access $v "a")))
           (rew/rewrite* :ptag/exp "$sum($v.a.(P * Q))" :rewrite? true)))))

(deftest expr-executions
  (testing "execution of small expressions"
    (is (= {"match" "foo", "index" 2, "groups" []}
           (rew/rewrite* :ptag/CodeBlock "$match(\"bbfoovar\", /foo/)" :execute? true)))
    (is (= {"match" "xababy", "index" 6, "groups" ["ab"]}
           (rew/rewrite* :ptag/CodeBlock "$match(\"foobarxababy\",/\\d*x(ab)+y/)" :execute? true)))))

(deftest code-block-executions
  (testing "execution of code bodies"
    (is (= [2 3 4 5 6] (rew/rewrite* :ptag/CodeBlock "($inc := function($i)    {$i + 1};  $map([1..5], $inc))"    :execute? true)))
    (is (= 15          (rew/rewrite* :ptag/CodeBlock "($add := function($i, $j){$i + $j}; $reduce([1..5], $add))" :execute? true)))
    (is (= "b"         (rew/rewrite* :ptag/CodeBlock "($v := ['a', 'b', 'c' 'd']; $v[1])"  :execute? true)))
    (is (= "a"         (rew/rewrite* :ptag/CodeBlock "($v := ['a', 'b', 'c' 'd']; $v[-4])" :execute? true)))
    (is (= "a"         (rew/rewrite* :ptag/CodeBlock "($v := ['a', 'b', 'c' 'd']; $v[0])" :execute? true)))))

(deftest user-guide-tests
  (testing "small code examples from the user's guide"
    (is (= ["20898" "07010-35445" "10878"]
           (rew/rewrite* :ptag/CodeBlock "data/map-examples/iteration/i1.mmp" :file? true :execute? true)))
    (is (= ["20898" "10878"]
           (rew/rewrite* :ptag/CodeBlock "data/map-examples/iteration/i2.mmp" :file? true :execute? true)))
    #_(is (= ["20898" "10878"]
           (rew/rewrite* :ptag/CodeBlock "data/map-examples/iteration/i3.mmp" :file? true :execute? true)))
    (is (= "123-456-7890" (rew/rewrite* :ptag/CodeBlock "data/map-examples/iteration/i4.mmp" :file? true :execute? true)))
    #_(is (= [68.9, 21.67, 137.8, 107.99]
             (rew/rewrite* :ptag/CodeBlock "data/map-examples/iteration/i6.mmp" :file? true :execute? true)))))
