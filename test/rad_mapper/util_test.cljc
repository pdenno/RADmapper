(ns rad-mapper.util-test
  "Test evaluation (and parsing and rewriting) of RADmapper code."
  (:require
   [clojure.test        :refer  [deftest is testing]]
   [rad-mapper.util     :as util]))

#?(:cljs
(deftest grouper
  (testing "Testing the re-matcher-like grouper function."
    (is (= (util/grouper #"[a-z0-9][A-Z]"  "aTaTa")
           [{:groups ["aT"]  :match "aT"  :index 0  :last-index 2  :input "aTaTa" }
            {:groups ["aT"]  :match "aT"  :index 2  :last-index 4  :input "aTaTa" }]))
    (is (= (util/grouper  #"((\d+)-(\d+))" "672-345-456-3212")
           [{:groups ["672-345"  "672-345"  "672" "345" ]  :match "672-345"   :index 0  :last-index  7  :input "672-345-456-3212" }
            {:groups ["456-3212" "456-3212" "456" "3212"]  :match "456-3212"  :index 8  :last-index 16  :input "672-345-456-3212" }])))))
  

