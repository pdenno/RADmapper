(ns rad-mapper.parse-test
  "Test parsing"
  (:require
   [rad-mapper.parse    :as par]
   [rad-mapper.rewrite  :as rew]
   [clojure.test :refer  [deftest is testing]]))

(deftest parsing-ToDos
  (testing "Things that don't quite sit right!"
    ;; See notes about the qvar/conditional expression dilemma.
    (is true #_(= (try (par/tokenize "?") (catch Exception e_ :fix-me)) :fix-me))))

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
            :ent {:_type :JaQvar, :qvar-name "?x"},
            :rel {:_type :JaTripleRole, :role-name :rdf/type},
            :val-exp "owl/Class"}
           (rew/rewrite* :ptag/triple "[?x :rdf/type 'owl/Class']" :simplify? true)))
    (is (= [{:_type :JaTriple, :ent {:_type :JaQvar, :qvar-name "?x"},
             :rel {:_type :JaTripleRole, :role-name :a}, :val-exp "one"}
            {:_type :JaTriple, :ent {:_type :JaQvar, :qvar-name "?y"},
             :rel {:_type :JaTripleRole, :role-name :b}, :val-exp "two"}]
           (rew/rewrite* :ptag/triples "[?x :a 'one'] [?y :b 'two']" :simplify? true)))
    (is (= {:_type :JaQueryDef,
            :params [],
            :triples
            [{:_type :JaTriple, :ent {:_type :JaQvar, :qvar-name "?class"},
              :rel {:_type :JaTripleRole, :role-name :rdf/type}, :val-exp "owl/Class"}
             {:_type :JaTriple, :ent {:_type :JaQvar, :qvar-name "?class"},
              :rel {:_type :JaTripleRole, :role-name :resource/iri},
              :val-exp {:_type :JaQvar, :qvar-name "?class-iri"}}]}
           (rew/rewrite* :ptag/exp q1 :simplify? true)))))

(deftest immediate-use
  (testing "Testing expressions that start by defining an in-line, anonymous function or query."

    ;; This tests parsing function as an immediate-use expression.
    (rew/rewrite* :ptag/exp "function($x){$x+1}(3)" :simplify? true)
    (is (= {:_type :JaImmediateUse,
            :def {:_type :JaFnDef,
                  :vars [{:_type :JaJvar, :jvar-name "$x"}],
                  :body {:_type :JaBinOpExp,
                         :exp1 {:_type :JaJvar, :jvar-name "$x"},
                         :bin-op \+, :exp2 1}},
            :args [3]}
           (rew/rewrite* :ptag/exp "function($x){$x+1}(3)" :simplify? true)))

    ;; This tests parsing query as an immediate-use expression.
    (is (=
         {:_type :JaImmediateUse,
          :def
          {:_type :JaQueryDef,
           :params [{:_type :JaJvar, :jvar-name "$name"}],
           :triples [{:_type :JaTriple,
                      :ent {:_type :JaQvar, :qvar-name "?e"},
                      :rel {:_type :JaTripleRole, :role-name :name},
                      :val-exp {:_type :JaJvar, :jvar-name "$name"}}]},
          :args [{:_type :JaSquareDelimitedExp,
                  :exp [{:_type :JaCurlyDelimitedExp,
                         :exp [{:_type :JaMapPair, :key "name", :val "Bob"}]}]}
                 "Bob"]}
         (rew/rewrite* :ptag/exp "query($name){[?e :name $name]}([{'name' : 'Bob'}], 'Bob')" :simplify? true)))))
