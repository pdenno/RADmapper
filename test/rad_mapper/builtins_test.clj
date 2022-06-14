(ns rad-mapper.builtins-test
  "Test built-in functions"
  (:require
   [rad-mapper.builtins :as bi]
   [clojure.test :refer  [deftest is testing]]))

(deftest builtin-basics
  (testing "Testing that built-ins work."
    (is (= "123-456-7890" (bi/dot-map {"mobile" "123-456-7890"} "mobile")))))

(deftest jsonata-flatten
  (testing "Testing that JSONata flattening rules are obeyed."
    
    (testing "Rule 1"
      (is (nil? (bi/jsonata-flatten []))))

    (testing "Rule 2"
      (is (= 1 (bi/jsonata-flatten [1]))))

    (testing "Rule 3 (The adapted core/flatten function doesn't flatten JSON.)"
      (is (= [1 2 3 [4 5] 6]
             (bi/flatten-except-json [1 [2] 3 (with-meta [4 5] {:bi/type :bi/json-array}) [6]]))))
    
    (testing "Rule 3 (Like above but uses the top-level flattening function.)"
      (is (= [1 2 3 [4 5] 6]
             (bi/jsonata-flatten [1 [2] 3 (with-meta [4 5] {:bi/type :bi/json-array}) [6]]))))

    (testing "Rule 4"
      (is (= [1 2 3 4 5 6]
             (bi/jsonata-flatten [1 [[2]] [3] [[[4 [5] [[6]]]]]]))))))
    




