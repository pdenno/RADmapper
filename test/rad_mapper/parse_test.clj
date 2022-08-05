(ns rad-mapper.parse-test
  "Test parsing"
  (:require
   [clojure.java.io     :as io]
   [clojure.spec.alpha  :as s]
   [clojure.test :refer  [deftest is testing]]
   [rad-mapper.parse    :as par]
   [rad-mapper.rewrite  :as rew]))

;;; See also rewrite_test.clj
(defn test-tokenize
  "Run the tokenizer on the argument string."
  [s]
  (with-open [rdr (io/reader (char-array ""))]
    (-> (par/make-pstate rdr)
        (assoc :string-block s)
        par/tokens-from-string
        par/tokenize
        :tokens)))

(deftest parsing-ToDos
  (testing "Things that don't quite sit right!"
    ;; See notes about the qvar/conditional expression dilemma.
    (is true #_(= (try (par/tokenize "?") (catch Exception e_ :fix-me)) :fix-me))))

(deftest tokenizer
  (testing "Tokenizer:"

    (testing "basic tests"
      (is (= [{:tkn "This is a string.", :line 1, :col 1}       {:tkn ::par/eof}]
             (test-tokenize "'This is a string.'")))
      (is (= [{:tkn "hello's world", :line 1, :col 1}           {:tkn ::par/eof}]
             (test-tokenize "'hello\\'s world'")))
      (is (= [{:tkn (par/->JaField "`foo ?`"), :line 1, :col 1} {:tkn ::par/eof}]
             (test-tokenize "`foo ?`"))))

    (testing "token stream"
      (is (= [{:tkn :true, :line 1, :col 1}
              {:tkn \?, :line 1, :col 5}
              {:tkn (par/map->JaTripleRole {:role-name :foo/bar}), :line 1, :col 6}
              {:tkn \:, :line 1, :col 14}
              {:tkn (par/map->JaTripleRole {:role-name :foo/bat}), :line 1, :col 15}
              {:tkn ::par/eof}]
             (test-tokenize "true?:foo/bar::foo/bat"))))

    (testing "\\? can be isolated as a token"
      (is (= [{:tkn :true, :line 1, :col 1}
              {:tkn \?, :line 1, :col 5}
              {:tkn "a", :line 1, :col 6}
              {:tkn \:, :line 1, :col 9}
              {:tkn "b", :line 1, :col 10}
              {:tkn :rad-mapper.parse/eof}]
             (test-tokenize "true?'a':'b'"))))

    ;; ToDo: I can't guess what is wrong here. Even stringifying and reading both sides
    ;;       of the equality does not help.
    #_(testing "triple roles are tokens"
      (is (= [{:tkn :true, :line 1, :col 1}
              {:tkn \?, :line 1, :col 5}
              {:tkn {:role-name :foo/bar}, :line 1, :col 6}
              {:tkn \:, :line 1, :col 14}
              {:tkn {:role-name :foo/bat}, :line 1, :col 15}
              {:tkn :rad-mapper.parse/eof}]
             (-> (test-tokenize "true?:foo/bar::foo/bat")
                 str
                 read-string))))))

(deftest regexp
  (testing "Testing translation of regular expression"
    (is (= "abc123" (re-matches (-> (par/regex-from-string "/^abc\\d+$/") :tkn) "abc123")))))

;;; Fix for BufferedReader
#_(deftest continuable
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
    (is (= {:_type :JaBinOpSeq,
            :seq
            '[{:_type :JaField, :field-name "a"}
              bi/get-step
              {:_type :JaField, :field-name "b"}
              bi/get-step
              {:_type :JaField, :field-name "c"}
              bi/get-step
              {:_type :JaField, :field-name "d"}
              bi/get-step
              {:_type :JaField, :field-name "e"}]}
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
    (is (=  '{:_type :JaImmediateUse,
              :def {:_type :JaFnDef,
                    :vars [{:_type :JaJvar, :jvar-name "$x"}],
                    :body {:_type :JaBinOpSeq,
                           :seq [{:_type :JaJvar,
                                  :jvar-name "$x"}
                                 bi/+ 1]}},
              :args [3]}
           (rew/rewrite* :ptag/exp "function($x){$x+1}(3)" :simplify? true)))

    ;; This tests parsing query as an immediate-use expression.
    (is (= '{:_type :JaImmediateUse,
             :def {:_type :JaQueryDef,
                   :params [{:_type :JaJvar, :jvar-name "$name"}],
                   :triples [{:_type :JaTriple,
                              :ent {:_type :JaQvar, :qvar-name "?e"},
                              :rel {:_type :JaTripleRole, :role-name :name},
                              :val-exp {:_type :JaJvar, :jvar-name "$name"}}]},
             :args [{:_type :JaArray,
                     :exprs [{:_type :JaObjExp,
                              :exp [{:_type :JaKVPair,
                                     :key "name", :val "Bob"}]}]} "Bob"]}
           (rew/rewrite* :ptag/exp "query($name){[?e :name $name]}([{'name' : 'Bob'}], 'Bob')" :simplify? true)))))

;;;=================== parse-ok? tests (doesn't study returned structure) ====================
(s/def ::simplified-parse-structure (s/or :typical (s/keys :req-un [::_type])
                                          :string string?
                                          :number number?
                                          :keyword keyword?))

(defn parse-ok? [exp]
  (try (let [res (rew/rewrite* :ptag/exp exp :simplify? true)]
         (s/valid? ::simplified-parse-structure res))
       (catch Exception _e false)))

(deftest simple
  (testing "Simple (parse):"
    (testing "all the, small things"
      (is (parse-ok? "1"))
      (is (parse-ok? "[1, 2, 3]"))
      (is (parse-ok? "1 + 2" ))
      (is (parse-ok? "[1..5]"))
      (is (parse-ok? "$A[1]" ))
      (is (parse-ok? "$sum($v.field)"))
      (is (parse-ok? "$sum(a.b)"))
      (is (parse-ok? "(A * B)"))
      (is (parse-ok? "4 ~> $f()"))
      (is (parse-ok? "$"  ))
      (is (parse-ok? "$$" ))
      (is (parse-ok? "$foo")))

    (testing "map on paths"
      (is (parse-ok? "$var.(P * Q)"))
      (is (parse-ok? "$data.(A * B)"))
      (is (parse-ok? "a.b.c.d + e.f"))
      (is (parse-ok? "a + b * $f(c + d)"))
      (is (parse-ok? "$var.a + b.c.(P * Q)")))

    (testing "var expressions"
      (is (parse-ok? "$var.a + b.c.(P * Q) + d"))
      (is (parse-ok? "$var.a + b.c.(P * Q) + d.(M * N)"))
      (is (parse-ok? "$var.a.b.c.(P * Q)"))
      (is (parse-ok? "$sum($v.a.(P * Q))")))

    (testing "miscellaneous"
      (is (parse-ok? "function($v,$i,$a) { $v.cbc_InvoicedQuantity < 0 }"))
      (is (parse-ok? "($inc := function($v) { $v + 1}; $map([1, 2, 3], $inc))"))
      (is (parse-ok? "$reduce([1..5], function($x,$y){$x + $y}, 100)"))
      (is (parse-ok? "$fn1($fn2($v).a.b)"))
      (is (parse-ok? "$sum($filter($v.InvoiceLine, function($v,$i,$a) { $v.Quantity < 0 }).Price.PriceAmount)"))

      ;; ToDo: I need to look at JSONata use of \; on this one.
      (is (parse-ok? "( $x := 1; $f($x) $g($x) )")))))

(deftest design
  (testing "Design (parse):"

    (testing "simple use of context variable (1)"
      (is (parse-ok? "'abc'[0]")))

    (testing "simple use of context variable (2)"
      (is (parse-ok? "[1 , 2, 3].$")))

    (testing "simple use of context variable (3)"
      (is (parse-ok? "( $ := {'a' : {'b' : {'c' : 123}}}; a.b.c.$ )")))

    (testing "Last part of path expression creates an array."
      (is (parse-ok? "[1,2,3].[$]")))

    (testing "simple use of contex variable, or not (1)"
      (is (parse-ok? "( $ := {'a' : {'b' : {'c' : 123}}}; a.b.c )")))

    (testing "simple use of contex variable, or not (2)"
      (is (parse-ok? "{'a' : {'b' : {'c' : 123}}}.a.b.c")))

    (testing "simple use of contex variable, or not (3)"
      (is (parse-ok? "{'a' : {'b' : {'c' : 123}}}.a.b.c.$")))

    (testing "implicit mapping and strange argument"
      (is (parse-ok? "['a', 'b', 'c'].$sum([50, 50])")))

    (testing "implicit mapping with use of $."
      (is (parse-ok? "( $ := [1, 2, 3]; $sum($) )")))

    (testing "binary precedence and non-advancing context variable (1)."
      (is (parse-ok? "($ := {'a' : 1, 'b' : 2, 'c' : 3, 'd' : 4}; a + b * c + d)")))

    (testing "binary precedence and non-advancing context variable (2)."
      (is (parse-ok? "{'a' : 1, 'b' : 2, 'c' : 3, 'd' : 4}.(a + b * c + d)")))

    (testing "'code-block' is just an expression" ; <========================== This one next.
      (is (parse-ok? "{'a' : 1, 'b' : 22}.($x := 2; $y:= 4; b)")))

    (testing "code-blocks allow closures"
      (is (parse-ok? "($incAmt := 3; $inc := function($n){$n + $incAmt}; $inc(5))")))

    (testing "Assignments return values; semicolon is a separator."
      (is (parse-ok?  "{'a' : 1, 'b' : 2}.($x := 3)")))

    (testing "advancing context variable on apply-map."
      (is (parse-ok? "( $:= $readFile('data/testing/jsonata/try.json');
                          Account.Order.Product.(Price*Quantity) )")))

    (testing "Like try.jsonata page."
      (is (parse-ok? "( $:= $readFile('data/testing/jsonata/try.json');
                          $sum(Account.Order.Product.(Price*Quantity)) )")))))
