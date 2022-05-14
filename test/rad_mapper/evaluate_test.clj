(ns rad-mapper.evaluate-test
  "Test evaluation (and parsing and rewriting) of RADmapper code."
  (:require
   [clojure.test       :refer  [deftest is testing]]
   [rad-mapper.rewrite  :as rew]))

(deftest expr-evaluations true
  (testing "evaluation of small expressions"
    #_(is false) ; Needed for recent versions of CIDER!

    ;; This tests use of $match
    (is (= {"match" "foo", "index" 2, "groups" []}
           (rew/rewrite* :ptag/exp "$match(\"bbfoovar\", /foo/)" :execute? true)))

    ;; This tests use of $match
    (is (= {"match" "xababy", "index" 6, "groups" ["ab"]}
           (rew/rewrite* :ptag/exp "$match(\"foobarxababy\",/\\d*x(ab)+y/)" :execute? true)))

    ;; This tests 'immediate use' of a function
    (is (= 4 (rew/rewrite* :ptag/exp "function($x){$x+1}(3)" :execute? true)))

    ;; This tests another sort of immediate use, using the threading macro.
    (is (= 5 (rew/rewrite* :ptag/exp "4 ~> function($x){$x+1}()" :execute? true)))

    ;; This tests mapping and the singleton behavior.
    (is (= [100 100 100] (rew/rewrite* :ptag/exp "['a', 'b', 'c'].$sum(100)" :execute? true)))

    ;; This tests binary precedence and use of the context variable.
    (is (= 11 (rew/rewrite* :ptag/code-block "($ := {'a' : 1, 'b' : 2, 'c' : 3, 'd' : 4}; a + b * c + d)" :execute? true)))

    ;; This tests reduce.
    (is (= 15 (rew/rewrite* :ptag/exp "$reduce([1..5], function($i, $j){$i + $j})" :execute? true)))

    ;; This tests reduce on one arg.
    (is (= 3 (rew/rewrite* :ptag/exp "$reduce([3], function($i, $j){$i + $j})" :execute? true)))

    ;; This tests reduce on one arg and an initial value.
    (is (= 5 (rew/rewrite* :ptag/exp "$reduce([3], function($i, $j){$i + $j}, 2)" :execute? true)))))

(deftest code-block-evaluations true
  (testing "evaluation of code bodies"
    (is (= [2 3 4 5 6] (rew/rewrite* :ptag/code-block "($inc := function($i)    {$i + 1};  $map([1..5], $inc))"         :execute? true)))
    (is (= 15          (rew/rewrite* :ptag/code-block "($add := function($i, $j){$i + $j}; $reduce([1..5], $add))"      :execute? true)))
    (is (= 115         (rew/rewrite* :ptag/code-block "($add := function($i, $j){$i + $j}; $reduce([1..5], $add, 100))" :execute? true)))

    ;; Tests for array indexing.
    (is (= "b"         (rew/rewrite* :ptag/code-block "($v := ['a', 'b', 'c' 'd']; $v[1])"  :execute? true)))
    (is (= "a"         (rew/rewrite* :ptag/code-block "($v := ['a', 'b', 'c' 'd']; $v[-4])" :execute? true)))
    (is (= "a"         (rew/rewrite* :ptag/code-block "($v := ['a', 'b', 'c' 'd']; $v[0])"  :execute? true)))))

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

(deftest user-guide-tests true
  (testing "small code examples from the user's guide"
    (is (= ["20898" "07010-3544" "10878"]
           (rew/rewrite* :ptag/code-block (str addr-data "$ADDR.zipcode )") :execute? true)))
    (is (= ["20898" "10878"]
           (rew/rewrite* :ptag/code-block (str addr-data "$ADDR.zipcode[$match(/^[0-9]+$/)] )") :execute? true)))
    (is (= "123-456-7890"
           (rew/rewrite* :ptag/code-block (str addr-data "$ADDR.phone.mobile )") :execute? true)))
    (is (= [68.9, 21.67, 137.8, 107.99]
           (rew/rewrite* :ptag/code-block "data/testing/map-examples/iteration/i6.mmp" :file? true :execute? true)))))
