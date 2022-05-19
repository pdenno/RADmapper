(ns rad-mapper.evaluate-test
  "Test evaluation (and parsing and rewriting) of RADmapper code."
  (:require
   [clojure.test        :refer  [deftest is testing]]
   [rad-mapper.builtins :as bi]
   [rad-mapper.rewrite  :as rew]))

(defn run [exp & {:keys [simplify? rewrite? debug? debug-parse?]}]
  (let [execute? (not (or simplify? rewrite?))]
    (rew/rewrite* :ptag/exp exp
                  :simplify? simplify?
                  :rewrite? rewrite?
                  :execute? execute?
                  :debug? debug?
                  :debug-parse? debug-parse?)))

(deftest expr-evaluations true
  (testing "evaluation of small expressions"
    #_(is false) ; Needed for recent versions of CIDER!

    ;; testing whether simple access of $ works.
    (is (= [1 2 3]
           (rew/rewrite* :ptag/code-block "($ := [{'a' : 1}, {'a' : 2}, {'a' : 3}]; a)" :execute? true)))

    ;; This tests use of $match
    (is (= {"match" "foo", "index" 2, "groups" []}  (run "$match(\"bbfoovar\", /foo/)")))

    ;; This tests use of $match
    (is (= {"match" "xababy", "index" 6, "groups" ["ab"]} (run "$match(\"foobarxababy\",/\\d*x(ab)+y/)")))

    ;; This tests 'immediate use' of a function
    (is (= 4 (run "function($x){$x+1}(3)")))

    ;; This tests another sort of immediate use, using the threading macro.
    (is (= 5 (run "4 ~> function($x){$x+1}()")))

    ;; This tests reduce.
    (is (= 15 (run "$reduce([1..5], function($i, $j){$i + $j})")))

    ;; This tests reduce on one arg.
    (is (= 3 (run "$reduce([3], function($i, $j){$i + $j})")))

    ;; This tests reduce on one arg and an initial value.
    (is (= 5 (run "$reduce([3], function($i, $j){$i + $j}, 2)")))))

(deftest code-block-evaluations true
  (testing "Code block:"

    (testing "simple code-blocks."
      (is (= [2 3 4 5 6] (rew/rewrite* :ptag/code-block "($inc := function($i)    {$i + 1};  $map([1..5], $inc))"         :execute? true)))
      (is (= 15          (rew/rewrite* :ptag/code-block "($add := function($i, $j){$i + $j}; $reduce([1..5], $add))"      :execute? true)))
      (is (= 115         (rew/rewrite* :ptag/code-block "($add := function($i, $j){$i + $j}; $reduce([1..5], $add, 100))" :execute? true))))

    (testing "array indexing."
      (is (= "b"         (rew/rewrite* :ptag/code-block "($v := ['a', 'b', 'c' 'd']; $v[1])"  :execute? true)))
      (is (= "a"         (rew/rewrite* :ptag/code-block "($v := ['a', 'b', 'c' 'd']; $v[-4])" :execute? true)))
      (is (= "a"         (rew/rewrite* :ptag/code-block "($v := ['a', 'b', 'c' 'd']; $v[0])"  :execute? true))))

    (testing "filter 'delimited expressions."
      (is (= [{"type" "mobile", "num" "555-123-4567"} {"type" "mobile", "num" "555-333-4444"}]
             (rew/rewrite* :ptag/code-block "($ := [{'Phone' : {'type' : 'mobile', 'num' : '555-123-4567'}}
                                                    {'Phone' : {'type' : 'work',   'num' : 'XXX-123-4567'}}
                                                    {'Phone' : {'type' : 'mobile', 'num' : '555-333-4444'}}];
                                             Phone[type = 'mobile'] )" :execute? true))))

    (testing "map 'delimited expressions'."
      (is (= [100 200]
             (rew/rewrite* :ptag/code-block "($ := [{'Product' : {'price' : 50, 'quantity' : 2}}
                                                    {'Product' : {'price' : 50, 'quantity' : 4}}];
                                                  Product.(price * quantity) )" :execute? true))))))
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
      (is (= 123 (run "( $ := {'a' : {'b' : {'c' : 123}}}; a.b.c )")))

    (testing "simple use of contex variable, or not (2)"
      (is (= 123 (run "{'a' : {'b' : {'c' : 123}}}.a.b.c"))))

    (testing "simple use of contex variable, or not (3)"
      (is (= 123 (run "{'a' : {'b' : {'c' : 123}}}.a.b.c.$"))))

    (testing "implicit mapping and strange argument" ; <========================== This one next.
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

    (testing "Assignments return values; semicolon is a separator."
      (is (= 1 (rew/rewrite* :ptag/code-block "{'a' : 1, 'b' : 2}.($x := 3)"
                             :execute? true))))

    (testing "advancing context variable on apply-map."
      (is (= [68.9, 21.67, 137.8, 107.99]
             (rew/rewrite* :ptag/code-block "( $:= $readFile('data/testing/jsonata/try.json');
                                               Account.Order.Product.(Price*Quantity) )"
                           :execute? true))))

    (testing "Like try.jsonata page."
      (is (= 336.36
             (rew/rewrite* :ptag/code-block "( $:= $readFile('data/testing/jsonata/try.json');
                                               $sum(Account.Order.Product.(Price*Quantity)) )"
                           :execute? true))))))

(deftest nyi
  (testing "NYI:"

    (testing "reduce using delimiters;  ToDo: the backquote thing."
      (is (= {"Bowler Hat" [68.9, 137.8], "Trilby hat" 21.67, "Cloak" 107.99}
             (rew/rewrite* :ptag/code-block "(  $:= $readFile('data/testing/jsonata/try.json');
                                                Account.Order.Product{`Product Name` : $.(Price*Quantity)} )"
                           :execute? true))))))

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
           (rew/rewrite* :ptag/code-block (str addr-data "$ADDR.zipcode )") :execute? true)))
    (is (= ["20898" "10878"]
           (rew/rewrite* :ptag/code-block (str addr-data "$ADDR.zipcode[$match(/^[0-9]+$/)] )") :execute? true)))
    (is (= "123-456-7890"
           (rew/rewrite* :ptag/code-block (str addr-data "$ADDR.phone.mobile )") :execute? true)))
    (is (= [68.9, 21.67, 137.8, 107.99]
           (rew/rewrite* :ptag/code-block "data/testing/map-examples/iteration/i6.mmp" :file? true :execute? true)))))

(defn tryme []
  (-> (bi/step-> (-> {} (assoc "a" (-> {} (assoc "b" (-> {} (assoc "c" 123))))))
                 (bi/dot-map "a")
                 (bi/dot-map "b")
                 (bi/dot-map "c")
                 :sys/$)
      rad-mapper.builtins/finish))
