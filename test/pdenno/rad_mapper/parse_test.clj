(ns pdenno.rad-mapper.parse-test
  "Test parsing"
  (:require
   [pdenno.rad-mapper.parse :as par]
   [clojure.test :refer  [deftest is testing]]))

(deftest tokenizer
  (testing "various tokenizer challenges."
    (is (= [{:tkn "This is a string.", :line 1, :col 1} {:tkn :eof, :line 1, :col 20}]
           (par/tokenize "'This is a string.'")))
    (is (= [{:tkn "hello's world", :line 1, :col 1} {:tkn :eof, :line 1, :col 17}]
           (par/tokenize "'hello\\'s world'")))))

(deftest regexp
  (testing "translation of regular expression"
    (is (= "abc123" (re-matches (-> (par/regex-from-string "/^abc\\d+$/") :tkn) "abc123")))))

(deftest continuable
  (testing "whether par/continuable works as expected."
    (is (= {:next-tkns [\. \(], :operand-tag :ptag/field}
           (-> "a.(P * Q)" par/tokenize par/make-pstate par/operand-exp?)))
    ;; Another -nil +nil !
    #_(is (= {:next-tkns [\[ {:field-name "x", :_type :JaField}], :operand-tag :ptag/field}
           (-> "a[x < 5]" par/tokenize par/make-pstate par/operand-exp?)))
    (is (= {:next-tkns [\. \{], :operand-tag :ptag/field}
           (-> "a.{'foo' : 5}" par/tokenize par/make-pstate par/operand-exp?)))))

(deftest parse-structures
  (testing "that parsing does the right things"
    ;; I can't see the problem in the following! (get -nil +nil)
    #_(is (= {:_type :JaBinOpExp,
            :exp1 {:field-name "a", :_type :JaField}, :bin-op \., :exp2 {:_type :JaBinOpExp,
             :exp1 {:field-name "b", :_type :JaField}, :bin-op \., :exp2 {:_type :JaBinOpExp,
              :exp1 {:field-name "c", :_type :JaField}, :bin-op \., :exp2 {:_type :JaBinOpExp,
               :exp1 {:field-name "d", :_type :JaField}, :bin-op \., :exp2 {:field-name "e", :_type :JaField}}}}}
           (m2c/rewrite* :ptag/exp "a.b.c.d.e" :simplify? true)))))
