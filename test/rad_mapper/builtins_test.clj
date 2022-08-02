(ns rad-mapper.builtins-test
  "Test built-in functions"
  (:require
   [rad-mapper.builtins :as bi]
   [clojure.test :refer  [deftest is testing]]))

(deftest jflatten
  (testing "Testing that JSONata flattening rules are obeyed."

    (testing "Rule 1"
      (is (nil? (bi/jflatten (bi/containerize [])))))

    (testing "Rule 1; drop map keys when the value is empty"
      (is (= [{"match" "foo", "index" 2}]
             (bi/jflatten
              (bi/containerize
               {"match" "foo", "index" 2, "groups" []})))))

    (testing "Rule 2"
      (is (= 1 (bi/jflatten (bi/containerize [1])))))

    (testing "Rule 3 (The adapted core/flatten function doesn't flatten JSON.)"
      (is (= [1 2 3 [4 5] 6]
             (bi/jflatten
              (with-meta [1 2 3 [4 5] 6] {:bi/type :bi/json-array})))))

    (testing "Rule 4"
      (is (= [1 2 3 4 5 6]
             (bi/jflatten
              (bi/containerize
               [1 [[2]] [3] [[[4 [5] [[6]]]]]])))))))
