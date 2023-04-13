(ns rad-mapper.parse-test
  "Test parsing"
  (:require
   #?(:clj [clojure.java.io :as io])
   [clojure.spec.alpha  :as s]
   [clojure.test        :refer [deftest is testing]]
   [rad-mapper.evaluate :as ev]
   [rad-mapper.parse    :as par]))

;;; ToDo: JSONata uses either quote or backquote for field names with spaces and funky stuff
;;;       I use backquote, and I'm not sure what sort of funky stuff ;^)

(defn tokenize-head
  "test-tokenize (below) doesn't produce tokens (except for head) it creates maps
   containing :tkn, :line and :col. (:head ps) is a token; make it a vector of one of these maps."
  [ps]
  (-> {:line 1 :col 1}
      (assoc :tkn (:head ps))
      vector))

#?(:clj
(defn test-tokenize
  "Run the tokenizer on the argument string."
  [s]
  (with-open [rdr (io/reader (char-array s))]
    (as-> (par/make-pstate rdr) ?ps
      (par/tokens-from-string ?ps)
      (par/tokenize ?ps)
      (into (tokenize-head ?ps) (:tokens ?ps))))))

#?(:cljs
(defn test-tokenize
  "Run the tokenizer on the argument string."
  [s]
  (as-> (par/make-pstate s) ?ps
    (par/tokens-from-string ?ps)
    (par/tokenize ?ps)
    (into (tokenize-head ?ps) (:tokens ?ps)))))

(deftest parsing-ToDos
  (testing "Things that don't quite sit right!"
    ;; See notes about the qvar/conditional expression dilemma.
    (is true #_(= (try (par/tokenize "?") (catch #?(:clj Exception :cljs :default) e_ :fix-me)) :fix-me))))

(deftest tokenizer
  (testing "Tokenizer:"

    (testing "Testing basics"
      (is (= [{:tkn {:typ :StringLit :value "This is a string."}, :line 1, :col 1}       {:tkn ::par/eof}]
             (test-tokenize "'This is a string.'")))
      (is (= [{:tkn {:typ :StringLit :value "hello's world"}, :line 1, :col 1}           {:tkn ::par/eof}]
             (test-tokenize "'hello\\'s world'")))
      (is (= [{:tkn {:typ :Field, :field-name "foo ?"}, :line 1, :col 1} {:tkn ::par/eof}]
             (test-tokenize "`foo ?`")))) ; Fields use backquote; otherwise, it is a StringLit.

    (testing "Testing regex"
      (is (= [{:line 1, :col 1, :tkn {:typ :RegExp, :base "/wo/", :flags {:ignore-case? true}}}
              {:tkn ::par/eof}]
             (test-tokenize "/wo/i"))))

    (testing "Testing multi-line comments"
      (is (= [{:tkn \[} {:tkn 1} {:tkn \]} {:tkn ::par/eof}]
             (->> (test-tokenize "/*  This is my
                                      multi-line comment */ [1]")
                  (mapv #(dissoc % :col :line))))))

    (testing "token stream"
      (is (= [{:tkn :tk/true, :line 1, :col 1}
              {:tkn \?, :line 1, :col 5}
              {:tkn {:typ :PatternRole :role-name :foo/bar}, :line 1, :col 6}
              {:tkn \:, :line 1, :col 14}
              {:tkn {:typ :PatternRole :role-name :foo/bat}, :line 1, :col 15}
              {:tkn ::par/eof}]
             (test-tokenize "true?:foo/bar::foo/bat")))

      (is (= [{:tkn :tk/true, :line 1, :col 1}
              {:tkn \?, :line 1, :col 6}
              {:tkn {:typ :PatternRole :role-name :foo/bar}, :line 1, :col 8}
              {:tkn \:, :line 1, :col 16}
              {:tkn {:typ :PatternRole :role-name :foo/bat}, :line 1, :col 17}
              {:tkn ::par/eof}]
             (test-tokenize "true ? :foo/bar::foo/bat"))))

    (testing "\\? can be isolated as a token"
      (is (= [{:tkn :tk/true, :line 1, :col 1}
              {:tkn \?, :line 1, :col 5}
              {:tkn {:typ :StringLit :value "a"}, :line 1, :col 6}
              {:tkn \:, :line 1, :col 9}
              {:tkn {:typ :StringLit :value "b"}, :line 1, :col 10}
              {:tkn ::par/eof}]
             (test-tokenize "true?'a':'b'"))))

    (testing "pattern roles are tokens"
      (is (= [{:tkn :tk/true, :line 1, :col 1}
              {:tkn \?, :line 1, :col 5}
              {:tkn {:typ :PatternRole, :role-name :foo/bar}, :line 1, :col 6}
              {:tkn \:, :line 1, :col 14}
              {:tkn {:typ :PatternRole, :role-name :foo/bat}, :line 1, :col 15}
              {:tkn ::par/eof}]
             (test-tokenize "true?:foo/bar::foo/bat"))))

    (testing "Problems with comments?" ; ToDo: Currently cljs has multi-line /* comments */ turned off.
         (is (= [{:line 1, :col 1, :tkn 1}
                 {:tkn 2, :line 2, :col 20}
                 {:tkn ::par/eof}]
                (test-tokenize
                 " 1       /*Foo*/
                   2       /*Bar*/"))))))

(deftest regexp
  (testing "Testing translation of regular expression"
    (is (= {:raw "/^abc\\d+$/", :tkn {:typ :RegExp, :base "/^abc\\d+$/", :flags {}}}
           (par/regex-from-string "/^abc\\d+$/")))))

(deftest parse-structures
  (testing "Testing that parsing does the right things"
    (is (= {:typ :BinOpSeq,
            :seq
            '[{:typ :Field, :field-name "a"}
              :op/get-step
              {:typ :Field, :field-name "b"}
              :op/get-step
              {:typ :Field, :field-name "c"}
              :op/get-step
              {:typ :Field, :field-name "d"}
              :op/get-step
              {:typ :Field, :field-name "e"}]}
           (ev/processRM :ptag/exp "a.b.c.d.e")))))

(deftest options-map
  (testing "Parsing an options map"
    (is (= [{:line 1, :col 1, :tkn "<|"}
            {:tkn 'entities, :line 1, :col 4}
            {:tkn \:, :line 1, :col 13}
            {:tkn :tk/true, :line 1, :col 15}
            {:tkn "|>", :line 1, :col 20}
            {:tkn ::par/eof}]
           (test-tokenize "<| entities : true |>")))
    (is (= {:typ :OptionsMap, :kv-pairs [{:typ :OptionKeywordPair, :key 'entities, :val :tk/true}]}
           (ev/processRM :ptag/options-map "<| entities : true |>")))))

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
    (is (= {:typ :QueryPattern,
            :ent {:typ :Qvar, :qvar-name "?x"},
            :rel {:typ :PatternRole, :role-name :rdf/type},
            :val "owl/Class"
            :db nil}
           (ev/processRM :ptag/q-pattern-tuple "[?x :rdf/type 'owl/Class']")))
    (is (= [{:typ :QueryPattern,
             :ent {:typ :Qvar, :qvar-name "?x"},
             :rel {:typ :PatternRole, :role-name :a},
             :val "one"
             :db nil}
            {:typ :QueryPattern,
             :ent {:typ :Qvar, :qvar-name "?y"},
             :rel {:typ :PatternRole, :role-name :b},
             :val "two"
             :db nil}]
           (ev/processRM :ptag/query-patterns "[?x :a 'one'] [?y :b 'two']")))
    (is (= {:typ :QueryDef,
            :params [],
            :options nil,
            :patterns
            [{:typ :QueryPattern,
              :ent {:typ :Qvar, :qvar-name "?class"},
              :rel {:typ :PatternRole, :role-name :rdf/type},
              :val "owl/Class"
              :db nil}
             {:typ :QueryPattern,
              :ent {:typ :Qvar, :qvar-name "?class"},
              :rel {:typ :PatternRole, :role-name :resource/iri},
              :val {:typ :Qvar, :qvar-name "?class-iri"}
              :db nil}]}
           (ev/processRM :ptag/exp q1)))))

(deftest query-immediate-use
  (testing "Testing expressions that start by defining an in-line, anonymous function or query."

    ;; This tests parsing function as an immediate-use expression.
    (is (= '{:typ :ImmediateUse,
             :def {:typ :FnDef,
                   :vars [{:typ :Jvar, :jvar-name "$x"}],
                   :body {:typ :BinOpSeq,
                          :seq [{:typ :Jvar,
                                 :jvar-name "$x"}
                                :op/add 1]}},
             :args [3]}
           (ev/processRM :ptag/exp "function($x){$x+1}(3)")))

    ;; This tests parsing query as an immediate-use expression.
    (is (= '{:typ :ImmediateUse,
             :def {:typ :QueryDef,
                   :options nil,
                   :params [{:typ :Jvar, :jvar-name "$name"}],
                   :patterns [{:typ :QueryPattern,
                               :ent {:typ :Qvar, :qvar-name "?e"},
                               :rel {:typ :PatternRole, :role-name :name},
                               :val {:typ :Jvar, :jvar-name "$name"}
                               :db nil}]},
             :args [{:typ :Array,
                     :exprs [{:typ :ObjExp, :kv-pairs [{:typ :KVPair,
                                                        :key "name",
                                                        :val "Bob"}]}]} "Bob"]}
           (ev/processRM :ptag/exp "query($name){[?e :name $name]}([{'name' : 'Bob'}], 'Bob')")))))

;;;=================== parse-ok? tests (doesn't study returned structure) ====================
(s/def ::parse-structure
  (s/or :typical (s/keys :req-un [::typ])
        :string string?
        :number number?
        :keyword keyword?))

(defn parse-ok? [exp]
  (try (let [res (ev/processRM :ptag/exp exp)]
          (s/valid? ::parse-structure res))
       #?(:clj  (catch Exception _ false)
          :cljs (catch :default  _ false))))

(deftest simple
  (testing "Simple (parse):"
    (testing "all the, small things"
      (is (parse-ok? "1"))
      (is (parse-ok? "5 / 9"))
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
      (is (parse-ok? "$lookup({}, 'a') or 'no match'")) ; Currently NOT okay! (JSONata returns true on execution of this, BTW.)

      (is (parse-ok? "( $x := 1; $f($x); $g($x) )")))))

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
                        $sum(Account.Order.Product.(Price*Quantity)) )")))

    (testing "$mapObject, an idea from DataWeave"
      (is (parse-ok? "$mapObject({'a' : 1, 'b' : 2}, function($k, $v){ {$uppercase($k) : $v} })")))))

(deftest comments
  (testing "Comments are ignored."
    (is (=
         [{:tkn \(}
          {:tkn {:typ :Jvar, :jvar-name "$x"}}
          {:tkn ":="}
          {:tkn 1}
          {:tkn \;}
          {:tkn {:typ :Jvar, :jvar-name "$y"}}
          {:tkn ":="}
          {:tkn 2}
          {:tkn \;}
          {:tkn {:typ :Jvar, :jvar-name "$z"}}
          {:tkn ":="}
          {:tkn 3}
          {:tkn \)}
          {:tkn ::par/eof}]
         (->> (test-tokenize
               "( $x := 1;
                  /* C-style-comment */ $y := 2;
                  // EOL-comment: You do not see me!
                  $z := 3 )")
              (mapv #(dissoc % :line :col)))))))

(deftest objects
  (testing "Testing various constructions of objects"
    (testing "basic"
      (parse-ok? "{'a' : 1, 'b' : 2}"))
    (testing "JSONata requires that keys be strings; we add a few types." ; ToDo: Note this in documentation to RM.
      (parse-ok? "{?x : 'a', ?y : 'b'}"))
    (testing "Testing :ptag/base-exp are allowed as keys or vals." ;  JSONata allows expressions that evaluate to strings.
      (parse-ok? "{'ab' & 'cd' : 123}"))))
