(ns rad-mapper.parse-test
  "Test parsing"
  (:require
   [rad-mapper.parse    :as par]
   [rad-mapper.rewrite  :as rew]
   [clojure.test :refer  [deftest is testing]]))

(deftest tokenizer
  (testing "Testing various tokenizer challenges."
    (is (= [{:tkn "This is a string.", :line 1, :col 1} {:tkn :eof, :line 1, :col 20}]
           (par/tokenize "'This is a string.'")))
    (is (= [{:tkn "hello's world", :line 1, :col 1} {:tkn :eof, :line 1, :col 17}]
           (par/tokenize "'hello\\'s world'")))))

(deftest regexp
  (testing "Testing translation of regular expression"
    (is (= "abc123" (re-matches (-> (par/regex-from-string "/^abc\\d+$/") :tkn) "abc123")))))

(deftest continuable
  (testing "Testing whether par/continuable works as expected."
    (is (= {:next-tkns [\. \(], :operand-tag :ptag/field}
           (-> "a.(P * Q)" par/tokenize par/make-pstate par/operand-exp?)))
    ;; Another -nil +nil !
    (is (= [[:next-tkns [\[ {:_type :JaField, :field-name "x"}]] [:operand-tag :ptag/field]]
           (-> "a[x < 5]" par/tokenize par/make-pstate par/operand-exp? rew/map-simplify)))
    (is (= {:next-tkns [\. \{], :operand-tag :ptag/field}
           (-> "a.{'foo' : 5}" par/tokenize par/make-pstate par/operand-exp?)))))

(deftest parse-structures
  (testing "Testing that parsing does the right things"
    ;; I can't see the problem in the following! (get -nil +nil)
    (is (= {:_type :JaBinOpExp,
            :exp1 {:field-name "a", :_type :JaField}, :bin-op \., :exp2 {:_type :JaBinOpExp,
             :exp1 {:field-name "b", :_type :JaField}, :bin-op \., :exp2 {:_type :JaBinOpExp,
              :exp1 {:field-name "c", :_type :JaField}, :bin-op \., :exp2 {:_type :JaBinOpExp,
               :exp1 {:field-name "d", :_type :JaField}, :bin-op \., :exp2 {:field-name "e", :_type :JaField}}}}}
             (rew/rewrite* :ptag/exp "a.b.c.d.e" :simplify? true)))))

(def q1
"query(){[?class :rdf/type            'owl/Class']
         [?class :resource/iri        ?class-iri]}")

(def q2
"query(){[?class :rdf/type            'owl/Class']
         [?class :resource/iri        ?class-iri]
         [?class :resource/namespace  ?class-ns]
         [?class :resource/name       ?class-name]
         [?rel   :rdf/type            'owl/ObjectProperty']
         [?rel   :rdfs/domain         ?class-iri]
         [?rel   :rdfs/range          ?rel-range]
         [?rel   :resource/name       ?rel-name]}")

(deftest query-tests
  (testing "Testing that queries parse okay."
    (is (= {:_type :JaTriple,
            :ent {:_type :JaQueryVar, :qvar-name "?x"},
            :rel {:_type :JaTripleRole, :role-name :rdf/type},
            :val-exp "owl/Class"}
           (rew/rewrite* :ptag/triple "[?x :rdf/type 'owl/Class']" :simplify? true)))
    (is (= [{:_type :JaTriple, :ent {:_type :JaQueryVar, :qvar-name "?x"},
             :rel {:_type :JaTripleRole, :role-name :a}, :val-exp "one"}
            {:_type :JaTriple, :ent {:_type :JaQueryVar, :qvar-name "?y"},
             :rel {:_type :JaTripleRole, :role-name :b}, :val-exp "two"}]
           (rew/rewrite* :ptag/triples "[?x :a 'one'] [?y :b 'two']" :simplify? true)))
    (is (= {:_type :JaQueryDef,
            :params [],
            :triples
            [{:_type :JaTriple, :ent {:_type :JaQueryVar, :qvar-name "?class"},
              :rel {:_type :JaTripleRole, :role-name :rdf/type}, :val-exp "owl/Class"}
             {:_type :JaTriple, :ent {:_type :JaQueryVar, :qvar-name "?class"},
              :rel {:_type :JaTripleRole, :role-name :resource/iri},
              :val-exp {:_type :JaQueryVar, :qvar-name "?class-iri"}}]}
           (rew/rewrite* :ptag/exp q1 :simplify? true)))))
