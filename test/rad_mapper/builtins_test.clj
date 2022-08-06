(ns rad-mapper.builtins-test
  "Test built-in functions"
  (:require
   [rad-mapper.builtins :as bi]
   [rad-mapper.parse    :as par]
   [rad-mapper.rewrite  :as rew]
   [rad-mapper.devl.devl-util :refer [nicer]]
   [clojure.test :refer  [deftest is testing]]))

(defn run [exp & {:keys [simplify? rewrite? debug? debug-parse?]}]
  (let [execute? (not (or simplify? rewrite?))]
    (rew/rewrite* :ptag/exp exp
                  :simplify? simplify?
                  :rewrite? rewrite?
                  :execute? execute?
                  :debug? debug?
                  :debug-parse? debug-parse?)))

(defn examine [exp]
  (-> (rew/rewrite* :ptag/exp exp :rewrite? true) nicer))

(defmacro run-test
  "Print the test form using testing, run the test."
  [form-string expect]
  `(testing ~(str "\n(run \"" form-string "\")")
     (is (= ~expect (run ~form-string)))))

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

;;; ==============  Builtin-functions =============================
;;; --------- string functions -----------------------------------
(deftest string-fns
  (testing "string functions"
    (testing "$base64(en|de)code"
      (run-test "$base64encode('myuser:mypass')" "bXl1c2VyOm15cGFzcw==")

      (run-test "$base64decode('bXl1c2VyOm15cGFzcw==')" "myuser:mypass"))

    (testing "$contains"
      (run-test "$contains('abracadabra', 'bra')"   true)
      (run-test "$contains('abracadabra', /a.*a/)"  true)
      (run-test "$contains('abracadabra', /ar.*a/)" false)
      (run-test "$contains('Hello World', /wo/)"    false)
      (run-test "$contains('Hello World', /wo/i)"   true)
      (run-test "( $ := {'Phone' : { 'type' : 'mobile', 'number' : '077 7700 1234'}};
                 Phone[$contains(number, /^077/)] )"
                { "type" "mobile", "number" "077 7700 1234"}))

    (testing "URL stuff"
      (run-test "$decodeUrl('https://mozilla.org/?x=%D1%88%D0%B5%D0%BB%D0%BB%D1%8B')"
                "https://mozilla.org/?x=шеллы")

      (run-test "$encodeUrl('https://mozilla.org/?x=шеллы')"
                "https://mozilla.org/?x=%D1%88%D0%B5%D0%BB%D0%BB%D1%8B")

      (run-test "$decodeUrlComponent('%3Fx%3Dtest')" "?x=test")

      (run-test "$encodeUrlComponent('?x=test')" "%3Fx%3Dtest"))

    (testing "$eval"
      (run-test "$eval('[1,2,3]')" [1 2 3])
      (run-test "$eval('[1,$string(2),3]')" [1 "2" 3]))

    (testing "$join"
      (run-test "$join(['a','b','c'])" "abc")
      (run-test "$join(['a','b','c'], ',')" "a,b,c")
      #_(run-test "$split('too much, punctuation. hard; to read', /[ ,.;]+/, 3)
                ~> $join(', ')"
                  "too, much, punctuation"))

    (testing "$(lower|upper)case"
      (run-test "$lowercase('Hello World')" "hello world")
      (run-test "$uppercase('Hello World')" "HELLO WORLD"))

    (testing "$pad"
      (run-test "$pad('foo',  5)" "foo  ")
      (run-test "$pad('foo', -5)" "  foo")
      (run-test "$pad('foo', -5, '#')" "##foo")
      #_(run-test "$formatBase(35, 2) ~> $pad(-8, '0')" "00100011"))

    (testing "$replace"
      (run-test "$replace('John Smith and John Jones', 'John', 'Mr')"
                "Mr Smith and Mr Jones")
      (run-test "$replace('John Smith and John Jones', 'John', 'Mr', 1)"
                "Mr Smith and John Jones")
      (run-test "$replace('abracadabra', /a.*?a/, '*')"
                "*c*bra")
      (run-test "$replace('John Smith', /(\\w+)\\s(\\w+)/, '$2, $1')"
                "Smith, John")
      (run-test "$replace('265USD', /([0-9]+)USD/, '$$$1')"
                "$265")
      (run-test "(
                   $convert := function($m) { ($number($m.groups[0]) - 32) * 5/9 & 'C' };
                   $replace('temperature = 68F today', /(\\d+)F/, $convert)
                 )"
                "temperature = 20.0C today"))  ;<================ JSONata returns "temperature = 20C today" (not 20.0)

    (testing "$split"
      (run-test "$split('so many words', ' ')"    [ "so", "many", "words" ])
      (run-test "$split('so many words', ' ', 2)" [ "so", "many" ])
      (run-test "$split('too much, punctuation. hard; to read', /[ ,.;]+/)"
                ["too", "much", "punctuation", "hard", "to", "read"]))

    (testing "$substring"
      (run-test "$substring('Hello World', 3)" "lo World")
      (run-test "$substring('Hello World', 3, 5)" "lo Wo")
      (run-test "$substring('Hello World', -4)" "orld")
      (run-test "$substring('Hello World', -4, 2)" "or"))

    (testing "$substring(After|Before)"
      (run-test "$substringAfter('Hello World', ' ')" "World")
      (run-test "$substringBefore('Hello World', ' ')" "Hello"))

    (testing "$trim"
      (run-test "$trim(' Hello \n World ')" "Hello World"))))

(deftest numerical-fns
  (testing "Numerical functions"
    (run-test "$abs(-5)" 5)
    (run-test "$average([3,5])" 4.0)
    (run-test "$ceil(5)" 5)
    (run-test "$ceil(5.3)" 6)
    (run-test "$ceil(5.8)" 6)
    (run-test "$ceil(-5.3)" -5)
    (run-test "$floor(5)" 5)
    (run-test "$floor(5.3)" 5)
    (run-test "$floor(5.8)" 5)
    (run-test "$floor(-5.3)" -6)
    (run-test "$formatBase(100, 2)" "1100100")
    (run-test "$formatBase(2555, 16)" "9fb")
    (run-test "$formatNumber(12345.6, '#,###.00')"  "12,345.60")
    (run-test "$formatNumber(1234.5678, '00.000e0')" "12.346e2")     
    (run-test "$formatNumber(34.555, '#0.00;(#0.00)')" "34.55")     ; <=========== JSONata gets 34.56
    (run-test "$formatNumber(-34.555, '#0.00;(#0.00)')" "(34.55)")  ; <=========== JSONata gets 34.56
    #_(run-test "$formatNumber(0.14, '01%')" "14%")
    #_(run-test "$formatNumber(0.14, '###pm', {'per-mille': 'pm'})" "140pm")
    #_(run-test "$formatNumber(1234.5678, '①①.①①①e①', {'zero-digit': '\u245f'})" "①②.③④⑥e②")))
