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

(deftest small-things
  (testing "All the, small things (execute):"

    (testing "simple access of $"
      (is (= [1 2 3] (run "($ := [{'a' : 1}, {'a' : 2}, {'a' : 3}]; a)"))))

    (testing "simple navigation"
      (is (= 33 (run "( $ := {'a' : {'b' : {'c' : 30, 'f' : 3}}}; a.b.c + a.b.f)"))))

    (testing "simple navigation, more efficiently"
      (is (= 33 (run "( $ := {'a' : {'b' : {'c' : 30, 'f' : 3}}}; a.b.(c +f) )"))))

    (testing "navigation with aref"
      (is (= 525 (run "( $ := {'a' : 5, 'b' : {'e' : 2}, 'c' : [0, 10], 'd' : 500};  a + b.e * c[1] + d )"))))

    (testing "jsonata flatten (1)"
      (is (= [1 2 3 4] (run "[[1,2,3], [4]].$"))))

    (testing "jsonata flatten (2)"
      (is (= 2 (run "[[1,2,3], 4].$[1]"))))

    (testing "mapping here means 'run [0] on each element.'"
      (is (= [1 4] (run "[[1,2,3], 4].$[0]"))))

    (testing "Jsonata's flattening idea has odd consequences!"
      (is (= [1 4] (run "[[1,2,3], 4].$[0][0][0][0]"))))

    (testing "Jsonata quirk 1: you can't use literal 1 here, but you can set $=1.'"
      (is (= 1 (run "($ := 1; $[0])"))))

    (testing "Jsonata quirk 1: ToDo: Note that RADmapper doesn't mind."
      (is (= 1 (run "1[0]"))))

    ;; I think the weirdness of 2a/2b suggests that you don't do jsonata-flatten until the end (in bi/finish).
    (testing "Jsonata quirk 2a: compare to 2b. If you stop here, you merge results."
      (is (= [1 2 3 4 5 6]
             (run "[{'nums' : [1, 2, 3]}, {'nums' : [4, 5, 6]}].nums"))))

    (testing "Jsonata quirk 2b: compare to 2a. Stop later, you assume a different intermediate form."
      (is (= [3 6]
             (run "[{'nums' : [1, 2, 3]}, {'nums' : [4, 5, 6]}].nums[2]"))))

    ;; By the way, Jsonata allows single quotes in the expression; JSON doesn't allow them in the data.
    (testing "Jsonata quirk 2a/2b is about knowing whether the last value was 'collected'???"
      (is (= [1] (run "{'nums' : [[1], 2, 3]}.nums[0]"))))

    (testing "Jsonata quirk 3: Note that it doesn't flatten to singleton here."
      (is (= [1] (run "{'nums' : [[1], 2, 3]}.nums[0]")))) ; Same as above; this one mentions quirk.

    ;; bi/passing-singleton? fixes (only) this one.
    (testing "simple aref" 
      (is (= 33 (run "{'number' : [11, 22, 33, 44]}.number[2]"))))

    (testing "simple filter"
      (is (= "b" (run "( $ := {'letter' : ['a', 'b', 'c', 'd']}; letter[$ = 'b'] )"))))

    (testing "simple filter (2)"
      (is (= [{"x" 2} {"x" 2}]
             (run "[{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}].num[x = 2]"))))

    (testing "simple filter, needs thought"
      (is (= {"num" {"x" 2}}
             (run "[{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}][num.x = 2]"))))

    #_(testing "simple filter, needs thought"
      (is (= [[false] [true] [false]]
             (run "[{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}].[num.x = 2]"))))

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

(defn tryme []
  (-> (rew/rewrite* :ptag/exp
                    "($ := [{'Phone' : {'type' : 'mobile', 'num' : '555-123-4567'}}
                      {'Phone' : {'type' : 'work',   'num' : 'XXX-123-4567'}}
                      {'Phone' : {'type' : 'mobile', 'num' : '555-333-4444'}}];
                 Phone[type = 'mobile'] )"
                    :rewrite? true)
      rad-mapper.devl.devl-util/nicer))

(defn tryme2 []
  (bi/finish
   (let [_x1 (bi/set-context! 1)]
     (bi/apply-filter
      (bi/step-> (bi/deref$) (bi/deref$))
      (fn [_x1] 0)))))

(defn tryme3 []
  (bi/finish
   (let [_x1 (bi/set-context! (-> {} (assoc "letter" ["a" "b" "c" "d"])))]
     (let* [fun  (fn [_x1] (= _x1 "b"))
            prix (try (fun bi/$) (catch java.lang.Exception _e__42631__auto__ nil))
            obj  (bi/singlize (bi/dot-map "letter"))]
       (if (number? prix)
         (let [ix (-> prix Math/floor int)
               len (count obj)]
           (if (or (and (pos? ix) (>= ix len))
                   (and (neg? ix) (> (abs ix) len)))
             nil
             (if (neg? ix)
               (nth obj (+ len ix))
               (nth obj ix))))
         (filterv fun obj))))))

(defn tryme4 []
  (bi/finish
   (bi/step->
    [[1 2 3] 4]
    (bi/apply-filter (bi/deref$) (fn [_x1] 1)))))

(defn tryme5 []
  (bi/finish
   (bi/step->
    [[1 2 3] 4]
    (bi/apply-filter (fn [_x1] 0))
    (bi/apply-filter (fn [_x1] 0))
    (bi/apply-filter (fn [_x1] 0))
    (bi/apply-filter (fn [_x1] 0)))))

(defn tryme6 []
  (bi/finish
   (let
       [_x1
        (bi/set-context!
         (->
          {}
          (assoc "a" 5)
          (assoc "b" (-> {} (assoc "e" 2)))
          (assoc "c" [0 10])
          (assoc "d" 500)))]
     (bi/+
      (bi/+
       (bi/dot-map "a")
       (bi/*
        (bi/step-> (bi/dot-map "b") (bi/dot-map "e"))
        (bi/apply-filter (bi/dot-map "c") (fn [_x1] 1))))
      (bi/dot-map "d")))))

;;; [{'num' : {'x' : 1}}, {'num' : {'x' : 2}}, {'num' : {'x' : 3}}].[num.x = 2]
;;; [{"num" : {"x" : 1}}, {"num" : {"x" : 2}}, {"num" : {"x" : 3}}]
(defn tryme7 []
  (bi/finish
   (bi/step->
    [(-> {} (assoc "num" (-> {} (assoc "x" 1))))
     (-> {} (assoc "num" (-> {} (assoc "x" 2))))
     (-> {} (assoc "num" (-> {} (assoc "x" 3))))]
    (bi/apply-filter
     nil
     (fn
       [_x1]
       (println "_x1 =" _x1)
       (bi/step-> _x1 (bi/dot-map "num") (= (bi/dot-map "x") 2)))))))


