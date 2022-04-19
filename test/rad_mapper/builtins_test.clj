(ns rad-mapper.builtins-test
  "Test built-in functions"
  (:require
   [rad-mapper.builtins :as bi]
   [clojure.test :refer  [deftest is testing]]))

(deftest builtin-basics
  (testing "Testing that built-ins work."
    (is (= "123-456-7890" (bi/access {"mobile" "123-456-7890"} "mobile")))))
