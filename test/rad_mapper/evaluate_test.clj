(ns rad-mapper.evaluate-test
  "Test evaluation (and parsing and rewriting) of RADmapper code."
  (:require
   [clojure.test        :refer  [deftest is testing]]
   [rad-mapper.rewrite  :as rew]))

(defn run [exp & {:keys [simplify? rewrite? debug? debug-parse?]}]
  (let [execute? (not (or simplify? rewrite?))]
    (rew/rewrite* :ptag/exp exp
                  :simplify? simplify?
                  :rewrite? rewrite?
                  :execute? execute?
                  :debug? debug?
                  :debug-parse? debug-parse?)))

(deftest small-things
  (testing "All the, small things (execute):"
    
    (testing "simple access of $"
      (is (= [1 2 3] (run "($ := [{'a' : 1}, {'a' : 2}, {'a' : 3}]; a)"))))

    (testing "simple navigation"
      (is (= 33 (run "( $ := {'a' : {'b' : {'c' : 30, 'f' : 3}}}; a.b.c + a.b.f)"))))

    (testing "simple navigation, more efficiently"
      (is (= 33 (run "( $ := {'a' : {'b' : {'c' : 30, 'f' : 3}}}; a.b.(c +f) )"))))

    (testing "use of $match (1)"
      (is (= {"match" "foo", "index" 2, "groups" []}  (run "$match(\"bbfoovar\", /foo/)"))))

    (testing "use of $match (1)"
      (is (= {"match" "xababy", "index" 6, "groups" ["ab"]} (run "$match(\"foobarxababy\",/\\d*x(ab)+y/)"))))

    (testing "'immediate use' of a function"
      (is (= 4 (run "function($x){$x+1}(3)"))))

    (testing "another sort of immediate use, using the threading macro"
      (is (= 5 (run "4 ~> function($x){$x+1}()"))))

    (testing "reduce."
      (is (= 15 (run "$reduce([1..5], function($i, $j){$i + $j})"))))

    (testing "reduce on one arg"
      (is (= 3 (run "$reduce([3], function($i, $j){$i + $j})"))))

    (testing "reduce on one arg and an initial value"
      (is (= 5 (run "$reduce([3], function($i, $j){$i + $j}, 2)"))))))

(deftest code-block-evaluations true
  (testing "Code block:"
    (testing "simple code-blocks."
      (is (= [2 3 4 5 6] (run "($inc := function($i)    {$i + 1};  $map([1..5], $inc))")))
      (is (= 15          (run "($add := function($i, $j){$i + $j}; $reduce([1..5], $add))")))
      (is (= 115         (run "($add := function($i, $j){$i + $j}; $reduce([1..5], $add, 100))"))))

    (testing "array indexing."
      (is (= "b"         (run "($v := ['a', 'b', 'c' 'd']; $v[1])" )))
      (is (= "a"         (run "($v := ['a', 'b', 'c' 'd']; $v[-4])")))
      (is (= "a"         (run "($v := ['a', 'b', 'c' 'd']; $v[0])" ))))

    (testing "filter 'delimited expressions."
      (is (= [{"type" "mobile", "num" "555-123-4567"} {"type" "mobile", "num" "555-333-4444"}]
             (run "($ := [{'Phone' : {'type' : 'mobile', 'num' : '555-123-4567'}}
                                                    {'Phone' : {'type' : 'work',   'num' : 'XXX-123-4567'}}
                                                    {'Phone' : {'type' : 'mobile', 'num' : '555-333-4444'}}];
                                             Phone[type = 'mobile'] )"))))

    (testing "map 'delimited expressions'."
      (is (= [100 200]
             (run "($ := [{'Product' : {'price' : 50, 'quantity' : 2}}
                                                    {'Product' : {'price' : 50, 'quantity' : 4}}];
                                                  Product.(price * quantity) )"))))))
(deftest why
  (testing "Why:"
    (testing "In JSONata this disregards data and returns 'abc'. Why?"
      (is (= "abc" (run "'abc'[$]"))))

    (testing "Maybe this is no match because the data is neither object nor array???"
      (is (= :no-match (run "'abc'.$"))))))

(deftest design
  (testing "Design (evaluate):"
    (testing "simple use of context variable (1)"
      (is (= "abc" (run "'abc'[0]"))))

    (testing "simple use of context variable (2)"
      (is (= [1 2 3] (run "[1 , 2, 3].$"))))

    (testing "simple use of context variable (3)"
      (is (= 123 (run "( $ := {'a' : {'b' : {'c' : 123}}}; a.b.c.$ )"))))

    (testing "Last part of path expression creates an array."
      (is (= [[1] [2] [3]] (run "[1,2,3].[$]"))))

    (testing "simple use of contex variable, or not (1)"
      (is (= 123 (run "( $ := {'a' : {'b' : {'c' : 123}}}; a.b.c )"))))

    (testing "simple use of contex variable, or not (2)"
      (is (= 123 (run "{'a' : {'b' : {'c' : 123}}}.a.b.c"))))

    (testing "simple use of contex variable, or not (3)"
      (is (= 123 (run "{'a' : {'b' : {'c' : 123}}}.a.b.c.$"))))

    (testing "implicit mapping and strange argument"
      (is (= [100 100 100] (run "['a', 'b', 'c'].$sum([50, 50])"))))

    (testing "implicit mapping with use of $."
      (is (= 6 (run "( $ := [1, 2, 3]; $sum($) )"))))

    (testing "binary precedence and non-advancing context variable (1)."
      (is (= 11 (run "($ := {'a' : 1, 'b' : 2, 'c' : 3, 'd' : 4}; a + b * c + d)"))))

    (testing "binary precedence and non-advancing context variable (2)."
      (is (= 11 (run "{'a' : 1, 'b' : 2, 'c' : 3, 'd' : 4}.(a + b * c + d)"))))

    (testing "'code-block' is just an expression"
      (is (= 22 (run "{'a' : 1, 'b' : 22}.($x := 2; $y:= 4; b)"))))

    (testing "code-blocks allow closures"
      (is (=  8 (run "($incAmt := 3; $inc := function($n){$n + $incAmt}; $inc(5))"))))

    (testing "assignments return values; semicolon is a separator."
      (is (= 1 (run "{'a' : 1, 'b' : 2}.($x := 3)"))))

    (testing "advancing context variable on apply-map."
      (is (= [68.9, 21.67, 137.8, 107.99]
             (run "( $:= $readFile('data/testing/jsonata/try.json');
                                               Account.Order.Product.(Price*Quantity) )"))))

    (testing "like the try.jsonata page"
      (is (= 336.36
             (run "( $:= $readFile('data/testing/jsonata/try.json');
                                               $sum(Account.Order.Product.(Price*Quantity)) )"))))))

(deftest nyi
  (testing "NYI:"
    (testing "reduce using delimiters;  ToDo: the backquote thing."
      (is (= {"Bowler Hat" [68.9, 137.8], "Trilby hat" 21.67, "Cloak" 107.99}
             (run "(  $:= $readFile('data/testing/jsonata/try.json');
                                                Account.Order.Product{`Product Name` : $.(Price*Quantity)} )"))))))

(def addr-data
  "( $ADDR :=
         [{'name'    : 'Peter',
           'street'  : '123 Mockingbird Lane',
           'zipcode' : '20898',
           'phone'   : {'mobile' : '123-456-7890'}},

          {'name'    : 'Bill',
           'street'  : '23 Main Street',
           'zipcode' : '07010-3544'},

          {'name'    : 'Lisa',
           'street'  : '903 Forest Road',
           'zipcode' : '10878'}]; ")

(deftest user-guide-tests
  (testing "small code examples from the user's guide"
    (is (= ["20898" "07010-3544" "10878"]
           (run (str addr-data "$ADDR.zipcode )"))))
    (is (= ["20898" "10878"]
           (run (str addr-data "$ADDR.zipcode[$match(/^[0-9]+$/)] )"))))
    (is (= "123-456-7890"
           (run (str addr-data "$ADDR.phone.mobile )"))))
    (is (= [68.9, 21.67, 137.8, 107.99]
           (run "data/testing/map-examples/iteration/i6.mmp" :file? true)))))

(deftest context
  (testing "Context management:"
    (is (= 33 (run "( $ := {'a' : {'b' : {'c' : 30, 'f' : 3}}}; a.b.c + a.b.f)")))))
