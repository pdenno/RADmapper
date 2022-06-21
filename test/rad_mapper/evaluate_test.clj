(ns rad-mapper.evaluate-test
  "Test evaluation (and parsing and rewriting) of RADmapper code."
  (:require
   [clojure.test        :refer  [deftest is testing]]
   [rad-mapper.builtins :as bi]
   [rad-mapper.rewrite  :as rew]
   [rad-mapper.devl.devl-util :refer [nicer]]))

(defn run [exp & {:keys [simplify? rewrite? debug? debug-parse?]}]
  (let [execute? (not (or simplify? rewrite?))]
    (rew/rewrite* :ptag/exp exp
                  :simplify? simplify?
                  :rewrite? rewrite?
                  :execute? execute?
                  :debug? debug?
                  :debug-parse? debug-parse?)))

;;; CIDER visual cues:
;;;  * bright red background on 'is' means is failed.
;;;  * washed out background on 'is' means execution failed
;;;  * washed out background elsewhere appears to mark more specifically what failed, typically the 'run'.
(deftest small-things
  (testing "All the, small things (execute):"

    (testing "JSONata doesn't recognize assignment to $ inside a code block."
      (is (nil? (run "($ := [1]; $)"))))

    (testing "simple mapping"
      (is (= [1 2 3] (run "[{'a' : 1}, {'a' : 2}, {'a' : 3}].a"))))

    (testing "simple mapping (2)"
      (is (= [2 3 4] (run "[{'a' : 1}, {'a' : 2}, {'a' : 3}].(a + 1)"))))

    (testing "simple mapping (3)"
      (is (= [2 3 4] (run "($f := function($x){$x+1}; [1,2,3].$f($))"))))

    (testing "simple mapping (4)"
      (is (= 111 (run "{'a' : {'b' : 111}}.a.b"))))

    (testing "simple navigation"
      (is (= 33 (run "{'a' : {'b' : {'c' : 30, 'f' : 3}}}.(a.b.c + a.b.f)"))))

    (testing "simple navigation, more efficiently"
      (is (= 33 (run "{'a' : {'b' : {'c' : 30, 'f' : 3}}}.a.b.(c + f)"))))

    (testing "simple aref (1)" ; JSONata returns no match on this. I think it should match!
      (is (= 1 (run "[{'a' : 1}][0].a)"))))

    (testing "simple aref (2)" ; JSONata is okay with this one.
      (is (= 1 (run "($c := [{'a' : 1}]; $c[0].a)"))))
      
    (testing "simple aref (2)" ; JSONata is okay with this one too.
      (is (= {"a" 1} (run "[{'a' : 1}][0]"))))

    (testing "navigation with aref"
      (is (= 525 (run "{'a' : 5, 'b' : {'e' : 2}, 'c' : [0, 10], 'd' : 500}.(a + b.e * c[1] + d )"))))

    ;; Note that JSONata Exerciser assumes JSON data if
    ;; (1) the data is in the LHS pane, or
    ;; (2) the data is assigned to a $var in a code block.
    ;; Otherwise, it is treated as JSONata data and flattening is applied.
    (testing "jsonata flatten (1); JSON data"
      (is (= [[1 2 3] [4]] (run "($v := [[1, 2, 3], [4]]; $v)"))))

    (testing "jsonata flatten (2) in-line data"
      (is (= [1 2 3 4] (run "[[1,2,3], [4]].$"))))

    (testing "jsonata flatten (3) in-line data"
      (is (= 2 (run "[[1,2,3], 4].$[1]"))))

    (testing "Jsonata's flattening odd consequences (1) in-line data"
      (is (= [1 4] (run "[[1,2,3], 4].$[0][0][0][0]"))))

    (testing "Jsonata's flattening odd consequences! (2) JSON data."
      (is (= 1 (run "($v := [[1,2,3], 4]; $v[0][0][0][0])"))))

    (testing "mapping here means 'run [0] on each element.'"
      (is (= [1 4] (run "[[1,2,3], 4].$[0]"))))

    ;; I get [11] with this one, JSONata gets 11. So what's the reason?
    ;; Because the vector only has one element in it? Nope. See 'contradicts' below.
    #_(testing "Experiment with distinguishing 'paths' from 'bin-ops'"
      (is (=  11 (run "[{'a' : {'b' : {'c' : 1}}, 'd' : {'e' : 10}}].(a.b.c + d.e)"))))

    ;; ToDo: The statement below is true of JSONata. Is it true of my implementation?
    (testing "In the above, you can't run without the [] except by doing something like this"
      (is (= 11 (run "{'a' : {'b' : {'c' : 1}}, 'd' : {'e' : 10}}.(a.b.c + d.e )"))))

    (testing "Jsonata quirk 1: you can't use literal 1 here, but you can set $=1.'"
      (is (= 1 (run "($v := 1; $v[0])"))))

    (testing "Jsonata quirk 1: ToDo: Note that RADmapper doesn't mind."
      (is (= 1 (run "1[0]"))))

    ;; The next two are concern the issue I raised in JSONata slack about "non-compositionality".
    ;; The answer I got is that nums[1] should be viewed as a term. 
    (testing "Jsonata quirk 2a: compare to 2b. If you stop here, you merge results."
      (is (= [1 2 3 4]
             (run "[{'nums' : [1, 2]}, {'nums' : [3, 4]}].nums"))))

    (testing "Jsonata quirk 2b: compare to 2a. Stop later, you assume a different intermediate form."
      (is (= [3 6]
             (run "[{'nums' : [1, 2]}, {'nums' : [3, 4]}].nums[1]"))))

    ;; By the way, Jsonata allows single quotes in the expression; JSON doesn't allow them in the data.
    ; *THIS*
    (testing "Jsonata quirk 2a/2b is about knowing whether the last value was 'collected'???"
      (is (= [1] (run "{'nums' : [[1], 2, 3]}.nums[0]"))))

    ; *THIS*
    (testing "Jsonata quirk 3: Note that it doesn't flatten to singleton here."
      (is (= [1] (run "{'nums' : [[1], 2, 3]}.nums[0]")))) ; Same as above; this one mentions quirk.

    ;; bi/passing-singleton? fixes (only) this one.
    (testing "simple aref"
      (is (= 33 (run "{'number' : [11, 22, 33, 44]}.number[2]"))))

    ;; Mine returns ["b"] because of bi/access-or-map?
    #_(testing "simple filter"
      (is (= "b" (run "{'letter' : ['a', 'b', 'c', 'd']}.letter[$ = 'b']"))))

    (testing "simple filter (2)"
      (is (= [{"x" 2} {"x" 2}]
             (run "[{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}].num[x = 2]"))))

    ;; This one contradicts my theory that what is returned depends on value of bi/access-or-map?
    ;; This one maps but return a single object as value
    #_(testing "simple filter, three objects coming in, yet returns a singleton. Needs thought!"
      (is (= {"num" {"x" 2}}
             (run "[{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}][num.x = 2]"))))

    #_(testing "simple filter, needs thought"
      (is (= [[false] [true] [false]]
             (run "[{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}].[num.x = 2]"))))

    ; *THIS*
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
      (is (= 5 (run "$reduce([3], function($i, $j){$i + $j}, 2)"))))

    ;; ToDo: Not a high priority, I think. The problem is is in parsing, I suppose.
    #_(testing "That you can return functions for built-ins"
        (is (= [bi/$sum bi/$sum] (run "[1,2].$sum"))))))

(deftest code-block-evaluations
  (testing "Code block:"
    (testing "simple code-blocks."
      (is (= [2 3 4 5 6] (run "($inc := function($i)    {$i + 1};  $map([1..5], $inc))")))
      (is (= 15          (run "($add := function($i, $j){$i + $j}; $reduce([1..5], $add))")))
      (is (= 115         (run "($add := function($i, $j){$i + $j}; $reduce([1..5], $add, 100))"))))

    (testing "array indexing."
      (is (= "b"            (run "($v := ['a', 'b', 'c', 'd']; $v[1])" )))
      (is (= "a"            (run "($v := ['a', 'b', 'c', 'd']; $v[-4])")))
      (is (= "a"            (run "($v := ['a', 'b', 'c', 'd']; $v[0])" )))
      (is (= [[1][1][1][1]] (run "['a', 'b', 'c', 'd'].[1]"))))

    ; *THIS*
    (testing "filter 'delimited expressions."
      (is (= [{"type" "mobile", "num" "555-123-4567"} {"type" "mobile", "num" "555-333-4444"}]
             (run "($p := [{'Phone' : {'type' : 'mobile', 'num' : '555-123-4567'}}
                           {'Phone' : {'type' : 'work',   'num' : 'XXX-123-4567'}}
                           {'Phone' : {'type' : 'mobile', 'num' : '555-333-4444'}}];
                      $p.Phone[type = 'mobile'] )"))))

    ; *THIS*
    (testing "map 'delimited expressions'."
      (is (= [100 200]
             (run "($p := [{'Product' : {'price' : 50, 'quantity' : 2}}
                           {'Product' : {'price' : 50, 'quantity' : 4}}];
                    $p.Product.(price * quantity) )"))))))
(deftest why
  (testing "Why:"
    (testing "In JSONata this disregards data and returns 'abc'. Why?"
      (is (= "abc" (run "'abc'[$]"))))

    ; *THIS*
    (testing "Maybe this is no match because the data is neither object nor array???"
      (is (= :no-match (run "'abc'.$"))))))

(deftest design
  (testing "Design (evaluate):"
    (testing "simple use of context variable (1)"
      (is (= "abc" (run "'abc'[0]"))))

    (testing "simple use of context variable (2)"
      (is (= [1 2 3] (run "[1 , 2, 3].$"))))

    (testing "simple use of context variable (3)"
      (is (= 123 (run "( $v := {'a' : {'b' : {'c' : 123}}}; $v.a.b.c.$ )"))))

    ; *THIS*
    (testing "Last part of path expression creates an array."
      (is (= [[1] [2] [3]] (run "[1,2,3].[$]"))))

    (testing "simple use of contex variable, or not (1)"
      (is (= 123 (run "( $v := {'a' : {'b' : {'c' : 123}}}; $v.a.b.c )"))))

    (testing "simple use of contex variable, or not (2)"
      (is (= 123 (run "{'a' : {'b' : {'c' : 123}}}.a.b.c"))))

    (testing "simple use of contex variable, or not (3)"
      (is (= 123 (run "{'a' : {'b' : {'c' : 123}}}.a.b.c.$"))))

    (testing "implicit mapping and strange argument"
      (is (= [100 100 100] (run "['a', 'b', 'c'].$sum([50, 50])"))))

    ; *THIS*
    (testing "implicit mapping with use of $."
      (is (= 6 (run "( $v := [1, 2, 3]; $sum($v) )"))))

    (testing "binary precedence and non-advancing context variable (1)."
      (is (= 11 (run "($v := {'a' : 1, 'b' : 2, 'c' : 3, 'd' : 4}; $v(a + b * c + d) )"))))

    (testing "binary precedence and non-advancing context variable (2)."
      (is (= 11 (run "{'a' : 1, 'b' : 2, 'c' : 3, 'd' : 4}.(a + b * c + d)"))))

    (testing "code block or primary doesn't matter"
      (is (= 1 (run "{'b' : 1}.(b)"))))

    (testing "'code-block' is just an expression"
      (is (= 22 (run "{'a' : 1, 'b' : 22}.($x := 2; $y:= 4; b)"))))

    (testing "code-blocks allow closures"
      (is (=  8 (run "($incAmt := 3; $inc := function($n){$n + $incAmt}; $inc(5))"))))

    ; *THIS*
    (testing "assignments return values; semicolon is a separator."
      (is (= 1 (run "{'a' : 1, 'b' : 2}.($x := 3)"))))

    ; *THIS*
    (testing "advancing context variable on apply-map."
      (is (= [68.9, 21.67, 137.8, 107.99]
             (run "( $:= $readFile('data/testing/jsonata/try.json');
                                               Account.Order.Product.(Price*Quantity) )"))))

    ; *THIS*
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

    ; *THIS*
    (is (= ["20898" "10878"]
           (run (str addr-data "$ADDR.zipcode[$match(/^[0-9]+$/)] )"))))

    (is (= "123-456-7890"
           (run (str addr-data "$ADDR.phone.mobile )"))))

    ; *THIS*
    (is (= [68.9, 21.67, 137.8, 107.99]
           (run "data/testing/map-examples/iteration/i6.mmp" :file? true)))))

#_(defn tryme []
  (bi/map-steps
   (bi/stepable
    (->
     {}
     (assoc "a" 5)
     (assoc "b" (-> {} (assoc "e" 2)))
     (assoc "c" [0 10])
     (assoc "d" 500)))
   (bi/primary
    (bi/+
     (bi/+
      (bi/get-step "a")
      (bi/*
       (bi/map-steps (bi/get-step "b") (bi/get-step "e"))
       (bi/aref (bi/get* "c") (fn [_x1] (bi/with-context _x1 1)))))
     (bi/get-step "d")))))
