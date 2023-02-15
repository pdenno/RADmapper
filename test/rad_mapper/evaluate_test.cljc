(ns rad-mapper.evaluate-test
  "Test evaluation (and parsing and rewriting) of RADmapper code."
  (:require
   [clojure.test        :refer  [deftest is testing]]
   [rad-mapper.builtin   :as bi]
   [rad-mapper.evaluate  :as ev]
   [dev.dutil-util :refer [run]]
   #?(:clj [dev.dutil-macros :refer [run-test]]))
#?(:cljs (:require-macros [dev.dutil-macros :refer [run-test]])))

(deftest today
  (run-test "-5"-5)
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
  (run-test "{'a' : 1, 'b' : 2}.[1]" [1]))

;;; CIDER visual cues:
;;;  * bright red background on 'is' means it failed.
;;;  * washed out background on 'is' means execution failed
;;;  * washed out background elsewhere appears to mark more specifically what failed, typically the 'run'.
(deftest small-things
    ;; ToDo: It is not so simple. The explantion below is wrong; it is more like the JSONata
    ;;       Exerciser doesn't use the binding to [1]. It will return the old value of $.
    #_(testing "JSONata doesn't recognize assignment to $ inside a code block."
        (is (nil? (run "($ := [1]; $)"))))

    (testing "string concatenation"
      (run-test "1 & 2 & 'abc'" "12abc"))

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

    (testing "simple aref (1)" ; JSONata returns no match on this. I think it should match! <============================= INVESTIGATE!
      (run-test "[{'a' : 1}][0].a" 1))

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
        (run-test  "($v := [[1,2,3], 4]; $v.$[0][0])" [1 4]))

      (testing "(3) no .$"
        (run-test  "($v := [[1,2,3], 4]; $v[0][0][0])" 1))

      (testing "(4) no .$"
        (run-test  "{'num' : [[1,2,3], 4]}.num[0][0]" 1))

      (testing "(5) .$/filter"
        (run-test  "{'num' : [[1,2,3], 4]}.num.$[0][0]" [1 4]))

      (testing "(6) .$ doesn't just map, but flattens."
        (run-test  "[[1,2,3], 4].$" [1 2 3 4]))

      (testing "(7) Flattened?"
        (run-test  "[[[1,2,3], 4]].$" [[1 2 3] 4])))
    ;; ------------------------------

    (testing "Experiment with distinguishing 'paths' from 'bin-ops'"
      (run-test "[{'a' : {'b' : {'c' : 1}}, 'd' : {'e' : 10}}].(a.b.c + d.e)" 11))

    (testing "In the above, you can't run without the [] except by doing something like this"
      (run-test  "{'a' : {'b' : {'c' : 1}}, 'd' : {'e' : 10}}.(a.b.c + d.e )" 11))

    (testing "Jsonata quirk 1: you can't use literal 1 here, but you can set $=1.'"
      (run-test  "($v := 1; $v[0])" 1))

    (testing "Jsonata quirk 1: ToDo: Note that RADmapper doesn't mind."
      (run-test  "1[0]" 1))

    ;; The next two concern the issue I raised in JSONata slack about "non-compositionality".
    ;; The answer I got is that nums[1] should be viewed as a term.
    (testing "Jsonata quirk 2a: compare to 2b. If you stop here, you merge results."
      (run-test  "[{'nums' : [1, 2]}, {'nums' : [3, 4]}].nums" [1 2 3 4]))

    (testing "Jsonata quirk 2b: compare to 2a. Stop later, you assume a different intermediate form."
      (run-test  "[{'nums' : [1, 2]}, {'nums' : [3, 4]}].nums[1]" [2 4]))

    (testing "Jsonata quirk 2a/2b: this returns [1] because it is operating on a JSON array."
      (run-test  "{'nums' : [[1], 2, 3]}.nums[0]" [1]))

    (testing "simple filter"
      (run-test "{'letter' : ['a', 'b', 'c', 'd']}.letter[$ = 'b']" "b"))

    (testing "simple filter (2)"
      (run-test  "[{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}].num[x = 2]"
                 [{"x" 2} {"x" 2}]))

    (testing "simple filter, three objects coming in, yet returns a singleton. Needs thought!"
      (run-test  "[{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}][num.x = 2]"
                   {"num" {"x" 2}}))

    (testing "simple filter, needs thought"
      (run-test  "[{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}].[num.x = 2]"
                 [[false] [true] [false]]))

    (testing "use of $match (1); note that JSONata doesn't clean up the empty groups."
      (run-test  "$match('bbfoovar', /foo/)" {"match" "foo", "index" 2 "groups" []}))

    (testing "use of $match (1)"
      (run-test "$match('foobarxababy',/\\d*x(ab)+y/)"
                {"match" "xababy", "index" 6, "groups" ["ab"]}))

    (testing "'immediate use' of a function"
      (run-test  "function($x){$x+1}(3)" 4) ; This one is true 'immediate use'.
      (run-test  "4 ~> function($x){$x+1}()" 5)
      (run-test  "[1..5] ~> $reverse()" [5 4 3 2 1]))

    (testing "reduce."
      (run-test  "$reduce([1..5], function($i, $j){$i + $j})" 15))

    (testing "reduce on one arg"
      (run-test  "$reduce([3], function($i, $j){$i + $j})" 3))

    (testing "reduce on one arg and an initial value"
      (run-test  "$reduce([3], function($i, $j){$i + $j}, 2)" 5))

    ;; ToDo: Not a high priority, I think. The problem is is in parsing, I suppose.
    #_(testing "That you can return functions for built-ins"
        (run-test "[1,2].$sum"  [bi/$sum bi/$sum])))

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
      (run-test  "['a', 'b', 'c'].[1]"
                 [[1][1][1]]))

    (testing "filter 'delimited expressions."
      (run-test "($p := [{'Phone' : {'type' : 'mobile', 'num' : '555-123-4567'}},
                         {'Phone' : {'type' : 'work',   'num' : 'XXX-123-4567'}},
                         {'Phone' : {'type' : 'mobile', 'num' : '555-333-4444'}}]; /* I'm commenting! */
                  $p.Phone[type = 'mobile'] )"
                [{"type" "mobile", "num" "555-123-4567"} {"type" "mobile", "num" "555-333-4444"}]))

    (testing "map 'delimited expressions'."
      (run-test "($p := [{'Product' : {'price' : 50, 'quantity' : 2}},
                         {'Product' : {'price' : 50, 'quantity' : 4}}];
                  $p.Product.(price * quantity) )"
                [100 200]))))
(deftest why
  (testing "Why:"
    (testing "In JSONata this disregards data and returns 'abc'. Why?"
      (run-test  "'abc'[$]" "abc"))

    (testing "JSONata exerciser gets :no-match on this."
      (run-test  "'abc'.$" "abc"))))

(deftest design
  (testing "Design (evaluate):"
    (testing "simple use of context variable (1)"
      (run-test  "'abc'[0]" "abc"))

    (testing "assignment is an expression"
      (run-test "$var := 3" 3))

    (testing "simple use of context variable (2)"
      (run-test  "[1 , 2, 3].$" [1 2 3]))

    (testing "simple use of context variable (3)"
      (run-test  "( $v := {'a' : {'b' : {'c' : 123}}}; $v.a.b.c.$ )" 123))

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

    (testing "implicit mapping with use of $."
      (run-test  "( $v := [1, 2, 3]; $sum($v) )" 6))

    (testing "binary precedence and non-advancing context variable (1)."
      (run-test  "($v := {'a' : 1, 'b' : 2, 'c' : 3, 'd' : 4}; $v.(a + b * c + d) )" 11))

    (testing "binary precedence and non-advancing context variable (2)."
      (run-test  "{'a' : 1, 'b' : 2, 'c' : 3, 'd' : 4}.(a + b * c + d)" 11))

    (testing "code block or primary doesn't matter"
      (run-test  "{'b' : 1}.(b)" 1))

    (testing "'code-block' is just an expression"
      (run-test  "{'a' : 1, 'b' : 22}.($x := 2; $y:= 4; b)" 22))

    (testing "code-blocks allow closures"
      (run-test  "($incAmt := 3; $inc := function($n){$n + $incAmt}; $inc(5))" 8))

    (testing "assignments return values; semicolon is a separator."
      (run-test  "{'a' : 1, 'b' : 2}.($x := 3)" 3))

    #?(:clj (testing "advancing context variable on apply-map."
              (run-test "( $:= $read('data/testing/jsonata/try.json');
                   Account.Order.Product.(Price*Quantity) )"
                        [68.9, 21.67, 137.8, 107.99])))
    #?(:clj (testing "like the try.jsonata page"
              (run-test "( $:= $read('data/testing/jsonata/try.json');
                   $sum(Account.Order.Product.(Price*Quantity)) )"
                        336.36)))))

#_(deftest nyi
  (testing "NYI:"
    (testing "reduce using delimiters;  ToDo: the backquote thing."
      (run-test "(  $:= $read('data/testing/jsonata/try.json');
                      Account.Order.Product{`Product Name` : $.(Price*Quantity)} )"
                {"Bowler Hat" [68.9, 137.8], "Trilby hat" 21.67, "Cloak" 107.99}))))

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
    (run-test (str addr-data "$ADDR.zipcode )")
              ["20898" "07010-3544" "10878"])

    (testing "$ (EOL) works with regular expressions"
      (run-test "$match('12345-12',/^[0-9]+$/)" nil))

    (run-test (str addr-data "$ADDR.zipcode[$match(/^[0-9]+$/)] )")
              ["20898" "10878"])

    (run-test "[{'phone' : {'mobile' : '123-456-7890'}}].phone.mobile"
              "123-456-7890")))

(deftest user-data
  (testing "Testing use of :user-data in processRM"
    (testing "Testing a simple use case"
      #_(is (= "hello world!"
           (ev/processRM :ptag/exp "$x1" {:user-data "$x1 := 'hello world!';" :execute? true}))))

    (testing "Testing a more extensive use case"
      (is (= {"Alice" {"aData" "Alice-A-data", "bData" "Alice-B-data", "id" 234}
               "Bob"  {"aData" "Bob-A-data",   "bData" "Bob-B-data",   "id" 123}}
             (ev/processRM
              :ptag/exp
              "( $qFn :=  query(){[$DBa ?e1 :id    ?id]
                                  [$DBb ?e2 :id    ?id]
                                  [$DBa ?e1 :name  ?name]
                                  [$DBa ?e1 :aAttr ?aData]
                                  [$DBb ?e2 :bAttr ?bData]};

                 $bSets := $qFn($DBa, $DBb);
                 $eFn := express{{?name : {'aData' : ?aData, 'bData' : ?bData, 'id' : ?id}}};
                 $reduce($bSets, $eFn) )"
              {:execute? true
               :user-data "$DBa := [{'id' : 123, 'aAttr' : 'Bob-A-data',   'name' : 'Bob'},
                                    {'id' : 234, 'aAttr' : 'Alice-A-data', 'name' : 'Alice'}];

                           $DBb := [{'id' : 123, 'bAttr' : 'Bob-B-data'},
                                    {'id' : 234, 'bAttr' : 'Alice-B-data'}]"}))))))

(defn tryme [which]
  (case which
    1 (println (ev/pprint-obj {"a-longer" 1 "b" 2}))
    2 (println (ev/pprint-obj {"a" 1 "b-much-much-much-longer" 2 "ccc" 3} :width 40 :depth 2))
    3 (println (ev/pprint-obj {"a-much-much-much-longer" 1 "b" 2} :width 40 :depth 4))
    4 (println (ev/pprint-obj {"a-much-much-much-longer" 1 "b" 2} :width 20 :depth 4))
    5 (println (ev/pprint-obj ["a-much-much-much-longer" "b"] :width 40 :depth 4))
    6 (println (ev/pprint-obj ["a-much-much-much-longer" "b"] :width 30 :depth 4))
    7 (println (ev/pprint-obj {"key-1" 1
                               "key-2" ["a-much-much-much-longer" "b"]} :width 40 :depth 4))
    8 (println (ev/pprint-obj {"key-1" 1
                               "key-2" ["a-much-much-much-longer" "b"]} :width 50 :depth 4))))
