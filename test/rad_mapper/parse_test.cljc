(ns rad-mapper.parse-test
  "Test parsing"
  (:require
   #?(:clj [clojure.java.io :as io])
   [clojure.spec.alpha  :as s]
   [clojure.test        :refer  [deftest is testing]]
   [dev.dutil           :refer [remove-meta]]
   [rad-mapper.evaluate :as ev]
   [rad-mapper.parse    :as par]))

#?(:clj
(defn test-tokenize
  "Run the tokenizer on the argument string."
  [s]
  (with-open [rdr (io/reader (char-array ""))]
    (-> (par/make-pstate rdr)
        (assoc :string-block s)
        par/tokens-from-string
        par/tokenize
        :tokens))))

#?(:cljs
(defn test-tokenize
  "Run the tokenizer on the argument string."
  [s]
  (-> (par/make-pstate s)
      #_(assoc :string-block s)
      par/tokens-from-string
      par/tokenize
      :tokens)))

(deftest parsing-ToDos
  (testing "Things that don't quite sit right!"
    ;; See notes about the qvar/conditional expression dilemma.
    (is true #_(= (try (par/tokenize "?") (catch #?(:clj Exception :cljs :default) e_ :fix-me)) :fix-me))))

(deftest tokenizer
  (testing "Tokenizer:"

    (testing "basic tests"
      (is (= [{:tkn "This is a string.", :line 1, :col 1}       {:tkn ::par/eof}]
             (test-tokenize "'This is a string.'")))
      (is (= [{:tkn "hello's world", :line 1, :col 1}           {:tkn ::par/eof}]
             (test-tokenize "'hello\\'s world'")))
      (is (= [{:tkn {:typ :Field, :field-name "`foo ?`"}, :line 1, :col 1} {:tkn ::par/eof}]
             (test-tokenize "`foo ?`")))
      ;; ToDo: Looks fine.
      #_(is (= [{:tkn {:base "/wo/", :flags {:ignore-case? true}}, :line 1, :col 1}
              {:tkn \), :line 1, :col 6} {:tkn ::par/eof}]
               (test-tokenize "/wo/i)")))
      (is (= [{:tkn \[, :line 1}
              {:tkn 1,  :line 1}
              {:tkn \], :line 1}
              {:tkn ::par/eof}]
             (->> (test-tokenize "/*  This is my
                                      multi-line comment */ [1]")
                  (mapv #(dissoc % :col))))))

    (testing "token stream"
      (is (= [{:tkn :true, :line 1, :col 1}
              {:tkn \?, :line 1, :col 5}
              {:tkn {:typ :TripleRole :role-name :foo/bar}, :line 1, :col 6}
              {:tkn \:, :line 1, :col 14}
              {:tkn {:typ :TripleRole :role-name :foo/bat}, :line 1, :col 15}
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

    (testing "triple roles are tokens"
      (is (= [{:tkn :true, :line 1, :col 1}
              {:tkn \?, :line 1, :col 5}
              {:tkn {:typ :TripleRole, :role-name :foo/bar}, :line 1, :col 6}
              {:tkn \:, :line 1, :col 14}
              {:tkn {:typ :TripleRole, :role-name :foo/bat}, :line 1, :col 15}
              {:tkn :rad-mapper.parse/eof}]
             (test-tokenize "true?:foo/bar::foo/bat"))))))

(deftest regexp
  (testing "Testing translation of regular expression"
    (is (= {:raw "/^abc\\d+$/", :tkn {:typ :RegExp, :base "/^abc\\d+$/", :flags {}}}
           (par/regex-from-string "/^abc\\d+$/")))))

(deftest parse-structures
  (testing "Testing that parsing does the right things"
    (is (= {:typ :BinOpSeq,
            :seq
            '[{:typ :Field, :field-name "a"}
              bi/get-step
              {:typ :Field, :field-name "b"}
              bi/get-step
              {:typ :Field, :field-name "c"}
              bi/get-step
              {:typ :Field, :field-name "d"}
              bi/get-step
              {:typ :Field, :field-name "e"}]}
           (ev/processRM :ptag/exp "a.b.c.d.e")))))

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
    (is (= {:typ :QueryTriple,
            :ent {:typ :Qvar, :qvar-name "?x"},
            :rel {:typ :TripleRole, :role-name :rdf/type},
            :val-exp "owl/Class"}
           (ev/processRM :ptag/q-pattern "[?x :rdf/type 'owl/Class']")))
    (is (= [{:typ :QueryTriple, :ent {:typ :Qvar, :qvar-name "?x"},
             :rel {:typ :TripleRole, :role-name :a}, :val-exp "one"}
            {:typ :QueryTriple, :ent {:typ :Qvar, :qvar-name "?y"},
             :rel {:typ :TripleRole, :role-name :b}, :val-exp "two"}]
           (ev/processRM :ptag/query-patterns "[?x :a 'one'] [?y :b 'two']")))
    (is (= {:typ :QueryDef,
            :params [],
            :triples
            [{:typ :QueryTriple, :ent {:typ :Qvar, :qvar-name "?class"},
              :rel {:typ :TripleRole, :role-name :rdf/type}, :val-exp "owl/Class"}
             {:typ :QueryTriple, :ent {:typ :Qvar, :qvar-name "?class"},
              :rel {:typ :TripleRole, :role-name :resource/iri},
              :val-exp {:typ :Qvar, :qvar-name "?class-iri"}}]}
           (ev/processRM :ptag/exp q1)))))

(deftest immediate-use
  (testing "Testing expressions that start by defining an in-line, anonymous function or query."

    ;; This tests parsing function as an immediate-use expression.
    (ev/processRM :ptag/exp "function($x){$x+1}(3)")
    (is (=  '{:typ :ImmediateUse,
              :def {:typ :FnDef,
                    :vars [{:typ :Jvar, :jvar-name "$x"}],
                    :body {:typ :BinOpSeq,
                           :seq [{:typ :Jvar,
                                  :jvar-name "$x"}
                                 bi/add 1]}},
              :args [3]}
           (ev/processRM :ptag/exp "function($x){$x+1}(3)")))

    ;; This tests parsing query as an immediate-use expression.
    (is (= '{:typ :ImmediateUse,
             :def {:typ :QueryDef,
                   :params [{:typ :Jvar, :jvar-name "$name"}],
                   :triples [{:typ :QueryTriple,
                              :ent {:typ :Qvar, :qvar-name "?e"},
                              :rel {:typ :TripleRole, :role-name :name},
                              :val-exp {:typ :Jvar, :jvar-name "$name"}}]},
             :args [{:typ :Array,
                     :exprs [{:typ :ObjExp, :kv-pairs [{:typ :KVPair,
                                                        :key "name",
                                                        :val "Bob"}]}]} "Bob"]}
           (->
            (ev/processRM :ptag/exp "query($name){[?e :name $name]}([{'name' : 'Bob'}], 'Bob')")
            remove-meta)))))

;;;=================== parse-ok? tests (doesn't study returned structure) ====================
(s/def ::simplified-parse-structure (s/or :typical (s/keys :req-un [::typ])
                                          :string string?
                                          :number number?
                                          :keyword keyword?))

(defn parse-ok? [exp]
  (try (let [res (ev/processRM :ptag/exp exp)]
         (s/valid? ::simplified-parse-structure res))
       #?(:clj  (catch Exception _ false)
          :cljs (catch :default  _ false))))

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

    (testing "'code-block' is just an expression"
      (is (parse-ok? "{'a' : 1, 'b' : 22}.($x := 2; $y:= 4; b)")))

    (testing "code-blocks allow closures"
      (is (parse-ok? "($incAmt := 3; $inc := function($n){$n + $incAmt}; $inc(5))")))

    (testing "Assignments return values; semicolon is a separator."
      (is (parse-ok?  "{'a' : 1, 'b' : 2}.($x := 3)")))

    (testing "advancing context variable on apply-map."
      (is (parse-ok? "( $:= $read('data/testing/jsonata/try.json');
                          Account.Order.Product.(Price*Quantity) )")))

    (testing "Like try.jsonata page."
      (is (parse-ok? "( $:= $read('data/testing/jsonata/try.json');
                          $sum(Account.Order.Product.(Price*Quantity)) )")))))
