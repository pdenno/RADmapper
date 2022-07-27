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

(defn examine [exp]
  (-> (rew/rewrite* :ptag/exp exp :rewrite? true) nicer))

(defmacro run-test
  "Print the test form using testing, run the test."
  [form-string expect]
  `(testing ~(str "(run \"" form-string "\")")
     (is (= ~expect (run ~form-string)))))

(deftest today
  (run-test "[[1,2,3], 4].$[1]" 2)
  (run-test "[[1,2,3], 4].$[0][0]" [1 4])
  (run-test "($v := [[1,2,3], 4]; $v.$[0][0])" [1 4] )
  (run-test "{'num' : [[1,2,3], 4]}.num.$[0][0]" [1 4])
  (run-test "[[[1,2,3], 4]].$" [[1 2 3] 4])
  (run-test "[{'nums' : [1, 2]}, {'nums' : [3, 4]}].nums[1]" [2 4])
  (run-test "{'nums' : [[1], 2, 3]}.nums[0]" [1])
  (run-test "[{'nums' : [1, 2]}, {'nums' : [3, 4]}].nums" [1 2 3 4])
  (run-test "{'number' : [11, 22, 33, 44]}.number[2]" 33)
  (run-test "['a', 'b', 'c'].[1]" [[1][1][1]])
  (run-test "{'a' :1, 'b' :2}.[1]" [1]))


;;; CIDER visual cues:
;;;  * bright red background on 'is' means is failed.
;;;  * washed out background on 'is' means execution failed
;;;  * washed out background elsewhere appears to mark more specifically what failed, typically the 'run'.
(deftest small-things
  (testing "All the, small things (execute):"

    (testing "JSONata doesn't recognize assignment to $ inside a code block."
      (is (nil? (run "($ := [1]; $)"))))

    (testing "simple mapping"
      (run-test "[{'a' : 1}, {'a' : 2}, {'a' : 3}].a" [1 2 3]))

    (testing "simple mapping (2)"
      (run-test "[{'a' : 1}, {'a' : 2}, {'a' : 3}].(a + 1)" [2 3 4]))

    (testing "simple mapping (3)"
      (run-test "($f := function($x){$x+1}; [1,2,3].$f($))" [2 3 4]))

    (testing "simple mapping (4)"
      (run-test "{'a' : {'b' : 111}}.a.b" 111))

    (testing "simple navigation"
      (run-test "{'a' : {'b' : {'c' : 30, 'f' : 3}}}.(a.b.c + a.b.f)" 33))

    (testing "simple navigation, more efficiently"
      (run-test "{'a' : {'b' : {'c' : 30, 'f' : 3}}}.a.b.(c + f)" 33))

    (testing "simple aref (1)" ; JSONata returns no match on this. I think it should match!
      (run-test "[{'a' : 1}][0].a)" 1))

    (testing "simple aref (2)" ; JSONata is okay with this one.
      (run-test "($c := [{'a' : 1}]; $c[0].a)" 1))

    (testing "simple aref (2)" ; JSONata is okay with this one too.
      (run-test "[{'a' : 1}][0]" {"a" 1}))

    (testing "navigation with aref"
      (run-test "{'a' : 5, 'b' : {'e' : 2}, 'c' : [0, 10], 'd' : 500}.(a + b.e * c[1] + d )" 525))

    ;; Note that JSONata Exerciser assumes JSON data if
    ;; (1) the data is in the LHS pane, or
    ;; (2) the data is assigned to a $var in a code block.
    ;; Otherwise, it is treated as JSONata data and flattening is applied.
    (testing "jsonata flatten (1); JSON data"
      (run-test  "($v := [[1, 2, 3], [4]]; $v)" [[1 2 3] [4]]))

    (testing "jsonata flatten (2) in-line data"
      (run-test  "[[1,2,3], [4]].$" [1 2 3 4]))

    (testing "jsonata flatten (3) in-line data"
      (run-test  "[[1,2,3], 4].$[1]" 2))

    ;; ------------------------------
    (testing "odd consequences"
      (testing "(1) .$/filter"
        (run-test  "[[1,2,3], 4].$[0][0]" [1 4])) ; You can use more [0] with no eff [1 4]      ))
  
      (testing "(2) .$/filter"
        (run-test  "($v := [[1,2,3], 4]; $v.$[0][0])" [1 4]      ))
      
      (testing "(3) no .$"
        (run-test  "($v := [[1,2,3], 4]; $v[0][0][0])" 1          ))

      (testing "(4) no .$"
        (run-test  "{'num' : [[1,2,3], 4]}.num[0][0]" 1          ))

      (testing "(5) .$/filter"
        (run-test  "{'num' : [[1,2,3], 4]}.num.$[0][0]" [1 4]      ))

      (testing "(6) .$ doesn't just map, but flattens."
        (run-test  "[[1,2,3], 4].$" [1 2 3 4]  ))

      (testing "(7) Flattened?"
        (run-test  "[[[1,2,3], 4]].$" [[1 2 3] 4])))
    ;; ------------------------------

    ;; I get [11] with this one, JSONata gets 11. So what's the reason?
    ;; Because the vector only has one element in it? Nope. See 'contradicts' below.
    #_(testing "Experiment with distinguishing 'paths' from 'bin-ops'"
      (is (=  11 (run "[{'a' : {'b' : {'c' : 1}}, 'd' : {'e' : 10}}].(a.b.c + d.e)"))))

    ;; ToDo: The statement below is true of JSONata. Is it true of my implementation?
    (testing "In the above, you can't run without the [] except by doing something like this"
      (run-test  "{'a' : {'b' : {'c' : 1}}, 'd' : {'e' : 10}}.(a.b.c + d.e )" 11))

    (testing "Jsonata quirk 1: you can't use literal 1 here, but you can set $=1.'"
      (run-test  "($v := 1; $v[0])" 1))

    (testing "Jsonata quirk 1: ToDo: Note that RADmapper doesn't mind."
      (run-test  "1[0]" 1))

    ;; The next two are concern the issue I raised in JSONata slack about "non-compositionality".
    ;; The answer I got is that nums[1] should be viewed as a term.
    (testing "Jsonata quirk 2a: compare to 2b. If you stop here, you merge results."
      (run-test  "[{'nums' : [1, 2]}, {'nums' : [3, 4]}].nums" [1 2 3 4]
            ))

    (testing "Jsonata quirk 2b: compare to 2a. Stop later, you assume a different intermediate form."
      (run-test  "[{'nums' : [1, 2]}, {'nums' : [3, 4]}].nums[1]" [2 4]
            ))

    ;; By the way, Jsonata allows single quotes in the expression; JSON doesn't allow them in the data.
    ;; This remails unsolved!
    (testing "Jsonata quirk 2a/2b is about knowing whether the last value was 'collected'???"
      (run-test  "{'nums' : [[1], 2, 3]}.nums[0]" [1]))

    ;; This remails unsolved!
    (testing "Jsonata quirk 3: Note that it doesn't flatten to singleton here."
      (run-test  "{'nums' : [[1], 2, 3]}.nums[0]" [1])) ; Same as above; this one mentions qu [1]))

    ;; bi/passing-singleton? fixes (only) this one.
    (testing "simple aref"
      (run-test  "{'number' : [11, 22, 33, 44]}.number[2]" 33))

    ;; Mine returns ["b"] because of bi/access-or-map?
    #_(testing "simple filter"
      (is (= "b" (run "{'letter' : ['a', 'b', 'c', 'd']}.letter[$ = 'b']"))))

    (testing "simple filter (2)"
      (run-test  "[{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}].num[x = 2]" [{"x" 2} {"x" 2}]))

    ;; This one contradicts my theory that what is returned depends on value of bi/access-or-map?
    ;; This one maps but return a single object as value
    #_(testing "simple filter, three objects coming in, yet returns a singleton. Needs thought!"
      (run-test  "[{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}][num.x = 2]" {"num" {"x" 2}}
            ))

    #_(testing "simple filter, needs thought"
      (run-test  "[{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}].[num.x = 2]" [[false] [true] [false]]
            ))

    (testing "use of $match (1); note that JSONata doesn't clean up the empty groups."
      (run-test  "$match('bbfoovar', /foo/)" {"match" "foo", "index" 2 "groups" []}))

    (testing "use of $match (1)"
       (run-test "$match('foobarxababy',/\\d*x(ab)+y/)" {"match" "xababy", "index" 6, "groups" ["ab"]}))

    (testing "'immediate use' of a function"
      (run-test  "function($x){$x+1}(3)" 4))

    (testing "another sort of immediate use, using the threading macro"
      (run-test  "4 ~> function($x){$x+1}()" 5))

    (testing "reduce."
      (run-test  "$reduce([1..5], function($i, $j){$i + $j})" 15))

    (testing "reduce on one arg"
      (run-test  "$reduce([3], function($i, $j){$i + $j})" 3))

    (testing "reduce on one arg and an initial value"
      (run-test  "$reduce([3], function($i, $j){$i + $j}, 2)" 5))

    ;; ToDo: Not a high priority, I think. The problem is is in parsing, I suppose.
    #_(testing "That you can return functions for built-ins"
        (is (= [bi/$sum bi/$sum] (run "[1,2].$sum"))))))

(deftest code-block-evaluations
  (testing "Code block:"
    (testing "simple code-blocks."
      (run-test  "($inc := function($i)    {$i + 1};  $map([1..5], $inc))" [2 3 4 5 6])
      (run-test  "($add := function($i, $j){$i + $j}; $reduce([1..5], $add))" 15)
      (run-test  "($add := function($i, $j){$i + $j}; $reduce([1..5], $add, 100))" 115))

    (testing "array indexing."
      (run-test  "($v := ['a', 'b', 'c', 'd']; $v[1])"  "b")
      (run-test  "($v := ['a', 'b', 'c', 'd']; $v[-4])" "a")
      (run-test  "($v := ['a', 'b', 'c', 'd']; $v[0])"  "a")
      (run-test  "['a', 'b', 'c'].[1]"                  [[1][1][1]]))

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
      (run-test  "'abc'[$]" "abc"))

    ; *THIS*
    (testing "Maybe this is no match because the data is neither object nor array???"
      (run-test  "'abc'.$" :no-match))))

(deftest design
  (testing "Design (evaluate):"
    (testing "simple use of context variable (1)"
      (run-test  "'abc'[0]" "abc"))

    (testing "simple use of context variable (2)"
      (run-test  "[1 , 2, 3].$" [1 2 3]))

    (testing "simple use of context variable (3)"
      (run-test  "( $v := {'a' : {'b' : {'c' : 123}}}; $v.a.b.c.$ )" 123))

    ; *THIS*
    (testing "Last part of path expression creates an array."
      (run-test  "[1,2,3].[$]" [[1] [2] [3]]))

    (testing "simple use of contex variable, or not (1)"
      (run-test  "( $v := {'a' : {'b' : {'c' : 123}}}; $v.a.b.c )" 123))

    (testing "simple use of contex variable, or not (2)"
      (run-test  "{'a' : {'b' : {'c' : 123}}}.a.b.c" 123))

    (testing "simple use of contex variable, or not (3)"
      (run-test  "{'a' : {'b' : {'c' : 123}}}.a.b.c.$" 123))

    (testing "implicit mapping and strange argument"
      (run-test  "['a', 'b', 'c'].$sum([50, 50])" [100 100 100]))

    ; *THIS*
    (testing "implicit mapping with use of $."
      (run-test  "( $v := [1, 2, 3]; $sum($v) )" 6))

    (testing "binary precedence and non-advancing context variable (1)."
      (run-test  "($v := {'a' : 1, 'b' : 2, 'c' : 3, 'd' : 4}; $v(a + b * c + d) )" 11))

    (testing "binary precedence and non-advancing context variable (2)."
      (run-test  "{'a' : 1, 'b' : 2, 'c' : 3, 'd' : 4}.(a + b * c + d)" 11))

    (testing "code block or primary doesn't matter"
      (run-test  "{'b' : 1}.(b)" 1))

    (testing "'code-block' is just an expression"
      (run-test  "{'a' : 1, 'b' : 22}.($x := 2; $y:= 4; b)" 22))

    (testing "code-blocks allow closures"
      (run-test  "($incAmt := 3; $inc := function($n){$n + $incAmt}; $inc(5))" 8))

    ; *THIS*
    (testing "assignments return values; semicolon is a separator."
      (run-test  "{'a' : 1, 'b' : 2}.($x := 3)" 1))

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
