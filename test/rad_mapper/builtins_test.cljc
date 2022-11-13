(ns rad-mapper.builtins-test
  "Test built-in functions"
  (:require
   [clojure.test :refer [deftest is testing]]
   #_[dev.dutil :refer [examine]] ; ToDo: This cannot be used. Why?
   [rad-mapper.parse]
   [rad-mapper.rewrite]
   [rad-mapper.builtins :as bi]
   [rad-mapper.builtins-macros :as bm]
   [rad-mapper.query :as qu]
   [dev.dutil-util :refer [run]]
   #?(:clj [dev.dutil-macros :refer [run-test]]))
#?(:cljs (:require-macros [dev.dutil-macros :refer [run-test]])))

(deftest jflatten-test
  (testing "Testing that JSONata flattening rules are obeyed."

    (testing "Rule 1"
      (is (nil? (bm/jflatten (bm/containerize [])))))

    (testing "Rule 1; drop map keys when the value is empty"
      (is (= [{"match" "foo", "index" 2}]
             (bm/jflatten
              (bm/containerize
               {"match" "foo", "index" 2, "groups" []})))))

    (testing "Rule 2"
      (is (= 1 (bm/jflatten (bm/containerize [1])))))

    (testing "Rule 3 (The adapted core/flatten function doesn't flatten JSON.)"
      (is (= [1 2 3 [4 5] 6]
             (bm/jflatten
              (with-meta [1 2 3 [4 5] 6] {:bi/type :bi/json-array})))))

    (testing "Rule 4"
      (is (= [1 2 3 4 5 6]
             (bm/jflatten
              (bm/containerize
               [1 [[2]] [3] [[[4 [5] [[6]]]]]])))))))

;;; ==============  Builtin-functions =============================
;;; --------- string functions -----------------------------------
(deftest string-fns
  (testing "string functions"
    #?(:clj ; ToDo: implement cljs
    (testing "$base64(en|de)code"
      (run-test "$base64encode('myuser:mypass')" "bXl1c2VyOm15cGFzcw==")

      (run-test "$base64decode('bXl1c2VyOm15cGFzcw==')" "myuser:mypass")))

    (testing "$contains"
      (run-test "$contains('', '')"                 true)
      (run-test "$contains('abracadabra', 'bra')"   true)
      (run-test "$contains('abracadabra', /a.*a/)"  true)
      (run-test "$contains('abracadabra', /ar.*a/)" false)
      (run-test "$contains('Hello World', /wo/)"    false)
      (run-test "$contains('Hello World', /wo/i)"   true)
      (run-test "( $v := {'Phone' : { 'type' : 'mobile', 'number' : '077 7700 1234'}} $v.Phone[$contains(number, /^077/)] )"
                {"type" "mobile", "number" "077 7700 1234"}))

    (testing "URL stuff"
      (run-test "$decodeUrl('https://mozilla.org/?x=%D1%88%D0%B5%D0%BB%D0%BB%D1%8B')"
                "https://mozilla.org/?x=шеллы")

      (run-test "$encodeUrl('https://mozilla.org/?x=шеллы')"
                "https://mozilla.org/?x=%D1%88%D0%B5%D0%BB%D0%BB%D1%8B")

      (run-test "$decodeUrlComponent('%3Fx%3Dtest')" "?x=test")

      (run-test "$encodeUrlComponent('?x=test')" "%3Fx%3Dtest"))

    #_(testing "$eval"   ; ToDo: Investigate for SCI and cljs.
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

    (testing "$match"
      (run-test "$match('ababbabbcc',/a(b+)/)"
                [{"match" "ab" , "index" 0, "groups" ["b"]}
                 {"match" "abb", "index" 2, "groups" ["bb"]}
                 {"match" "abb", "index" 5, "groups" ["bb" ]}])
      (run-test "$match('ababbxxxxabbcc',/a(b+)/)"
                [{"match" "ab" , "index" 0, "groups" ["b"]}
                 {"match" "abb", "index" 2, "groups" ["bb"]}
                 {"match" "abb", "index" 9, "groups" ["bb" ]}]))

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

(deftest rm-fns
  (testing "RADmapper functions"
    (run-test "( $db := $db([{'id' : 'find-me', 'attr1' : 1, 'attr2' : 'two', 'anotherAttr' : 'another-value'}]);
                 $id := query( <|keepDBid : true|> ){[?e :id 'find-me']}.?e;
                 $pull($id, $db) )"
              '[{:id "find-me" :attr1' 1 :attr2 "two" :anotherAttr "another-value"}])))

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
    (run-test "$floor(-5.3)" -6))

  (testing "$formatBase"
    (run-test "$formatBase(100, 2)" "1100100")
    (run-test "$formatBase(2555, 16)" "9fb"))

  (testing "$formatNumber"
    (run-test "$formatNumber(12345.6, '#,###.00')"  "12,345.60")
    (run-test "$formatNumber(1234.5678, '00.000E0')" "12.346e2")
    (run-test "$formatNumber(-12345.6, '#,###.00', {'minus-sign' : '*'})" "*12,345.60")
    (run-test "$formatNumber(34.555, '#0.00;(#0.00)')" "34.55")     ; <=========== JSONata gets 34.56 Use ideas from bi/$round.
    (run-test "$formatNumber(-34.555, '#0.00;(#0.00)')" "(34.55)")  ; <=========== JSONata gets 34.56 Use ideas from bi/$round.
    (run-test "$formatNumber(0.14, '00%')" "14%")                   ; <=========== Typo? (JSONata had '01%') Not a Java picture.
    (run-test "$formatNumber(0.14, '###pm', {'per-mille': 'pm'})" "140pm")
    #_(run-test "$formatNumber(1234.5678, '①①.①①①E①', {'zero-digit': '\u245f'})" "①②.③④⑥E②")) ; Needs investigation. Error in exerciser too.

  (testing "$formatInteger"
    ;; https://www.altova.com/xpath-xquery-reference/fn-format-integer
    (run-test "$formatInteger(123, '0000')" "0123")
    (run-test "$formatInteger(123, 'w')" "one hundred twenty-three")
    (run-test "$formatInteger(7, 'a')" "g")
    (run-test "$formatInteger(29, 'A')" "AC")
    (run-test "$formatInteger(57, 'I')" "LVII")
    ;; https://www.altova.com/xpath-xquery-reference/fn-format-integer
    #_(run-test "$formatInteger(1234, '#;##0;')" "1;234")) ; Needs investigation.

  (testing "$parseInteger"
    (run-test "$parseInteger('twelve thousand, four hundred and seventy-six', 'w')" 12476)
    #_(run-test "$parseInteger('12,345,678', '#,##0')" 12345678) ; ToDo: Needs investigation
    (run-test "$parseInteger('three', 'w')" 3)
    (run-test "$parseInteger('thirty', 'w')" 30)
    (run-test "$parseInteger('thirty two', 'w')" 32)
    (run-test "$parseInteger('one hundred', 'w')" 100)
    (run-test "$parseInteger('one hundred sixty two', 'w')" 162)
    (run-test "$parseInteger('six hundred fifty-three thousand, two hundred fifty four', 'w')" 653254)
    (run-test "$parseInteger('nine hundred ninety nine quadrillion', 'w')" 999000000000000000)
    (run-test "$parseInteger('two million, six hundred fifty-three thousand, two hundred fifty four', 'w')" 2653254))

  (testing "$round"
    (run-test "$round(123.456)" 123)
    (run-test "$round(123.456, 2)" 123.46)
    (run-test "$round(123.456, -1)" 120)
    (run-test "$round(123.456, -2)" 100)
    (run-test "$round(11.5)" 12)
    (run-test "$round(12.5)" 12)
    (run-test "$round(125, -1)" 120)))

(deftest boolean-fns
  (testing "boolean functions"
    (testing "$boolean"
      (run-test "$boolean(true)" true)
      (run-test "$boolean(false)" false)
      (run-test "$boolean(1)" true)
      (run-test "$boolean(0)" false)
      (run-test "$boolean('')" false)
      (run-test "$boolean('a')" true)
      (run-test "$boolean({'a' : 1})" true)
      (run-test "$boolean({})" false)
      (run-test "$boolean([0])" false)
      (run-test "$boolean([1])" true))

    (testing "$exists, which I'm not sure I understand."
      (run-test "$exists({\"a\" : 1}.a)" true)
      (run-test "$exists({\"a\" : 1}.b)" false))))

(deftest array-fns
  (testing "array functions"
    (run-test "$append([1,2,3], [4,5,6])" [1,2,3,4,5,6])
    (run-test "$append([1,2,3], 4)" [1,2,3,4])
    (run-test "$append('Hello', 'World')" ["Hello", "World"])
    (run-test "$count([1,2,3,1])" 4)
    (run-test "$count('hello')" 1)
    (run-test "$distinct([1,2,3,3,4,3,5])" [1, 2, 3, 4, 5])
    (run-test "$reverse(['Hello', 'World'])" ["World", "Hello"])
    (run-test "[1..5] ~> $reverse()" [5, 4, 3, 2, 1])
    (run-test "$sort(['x' 'a' 'c' 'b'])" ["a" "b" "c" "x"])
    (run-test "$zip([1,2,3], [4,5,6])"  [[1,4] ,[2,5], [3,6]])
    (run-test "$zip([1,2,3],[4,5],[7,8,9])" [[1,4,7], [2,5,8]])))

(deftest object-fns
  (testing "object functions"
    #_(run-test "$each(Address, function($v, $k) {$k & ' ' & $v})"
              ["Street: Hursley Park", "City: Winchester", "Postcode: SO21 2JN"])

    (run-test "$keys({'a' : 1, 'b' :2})" ["a" "b"])

    (run-test "($ := $read('data/testing/jsonata/try.json');
                Account.Order.Product.$sift(function($v, $k) {$k ~> /^Product/}) )"
              [{"Product Name" "Bowler Hat", "ProductID" 858383}
               {"Product Name" "Trilby hat", "ProductID" 858236}
               {"Product Name" "Bowler Hat", "ProductID" 858383}
               {"Product Name" "Cloak"     , "ProductID" 345664}])

    (run-test "$spread({'a' : 1, 'b' : 2})" [{"a" 1} {"b" 2}])
    (run-test "$spread([{'a' : 1, 'b' : 2},{'a' : 1, 'b' : 2}])"
              [{"a" 1} {"b" 2} {"a" 1} {"b" 2}])))

#?(:clj ; ToDo: Implement cljs
(deftest date-fns
  (testing "datetime functions"
    (testing "$fromMillis"
      (run-test "$fromMillis(1510067557121)" "2017-11-07T15:12:37.121Z")
      (run-test "$fromMillis(1510067557121, '[M01]/[D01]/[Y0001] [h#1]:[m01][P]')"
                "11/07/2017 03:12PM") ; ToDo: Should be 'pm' not 'PM' and 3:12, not 03:12
      (run-test "$fromMillis(1510067557121, '[H01]:[m01]:[s01] [z]', '-0500')"
                "10:12:37 -0500"))))) ; ToDo: Example shows "10:12:37 GMT-05:00"

#_(defn tryme []
   (bi/reset-env)
 (bi/again?
  (bi/primary-m
   (let
    [$db
     (with-meta
      [(->
        {}
        (assoc "id" "find-me")
        (assoc "attr1" 1)
        (assoc "attr2" "two")
        (assoc "anotherAttr" "another-value"))]
      #:bi{:json-array? true})
     $q
     (bi/query '[] '[[?e :id "find-me"]] {:keepDBid true})
     $id
     (bi/run-steps
      (bi/init-step-m (bi/fncall {:args [$db], :func $q}))
      (bi/get-step (with-meta '?e {:qvar? true})))]
    (bi/$pull $id $db)))))
