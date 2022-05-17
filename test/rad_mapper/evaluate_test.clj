(ns rad-mapper.evaluate-test
  "Test evaluation (and parsing and rewriting) of RADmapper code."
  (:require
   [clojure.test        :refer  [deftest is testing]]
   [rad-mapper.builtins :as bi]
   [rad-mapper.rewrite  :as rew]))

(def try-data "Default data from https://try.jsonata.org."
"{
  'Account': {
    'Account Name': 'Firefly',
    'Order': [
      {
        'OrderID': 'order103',
        'Product': [
          {
            'Product Name': 'Bowler Hat',
            'ProductID': 858383,
            'SKU': '0406654608',
            'Description': {
              'Colour': 'Purple',
              'Width': 300,
              'Height': 200,
              'Depth': 210,
              'Weight': 0.75
            },
            'Price': 34.45,
            'Quantity': 2
          },
          {
            'Product Name': 'Trilby hat',
            'ProductID': 858236,
            'SKU': '0406634348',
            'Description': {
              'Colour': 'Orange',
              'Width': 300,
              'Height': 200,
              'Depth': 210,
              'Weight': 0.6
            },
            'Price': 21.67,
            'Quantity': 1
          }
        ]
      },
      {
        'OrderID': 'order104',
        'Product': [
          {
            'Product Name': 'Bowler Hat',
            'ProductID': 858383,
            'SKU': '040657863',
            'Description': {
              'Colour': 'Purple',
              'Width': 300,
              'Height': 200,
              'Depth': 210,
              'Weight': 0.75
            },
            'Price': 34.45,
            'Quantity': 4
          },
          {
            'ProductID': 345664,
            'SKU': '0406654603',
            'Product Name': 'Cloak',
            'Description': {
              'Colour': 'Black',
              'Width': 30,
              'Height': 20,
              'Depth': 210,
              'Weight': 2
            },
            'Price': 107.99,
            'Quantity': 1
          }
        ]
      }
    ]
  }
}")

(defn reset-try-data
  "Set the context before every test that uses the above data."
  []
  (reset! bi/$ (rew/rewrite* :ptag/exp try-data :execute? true)))

(deftest expr-evaluations true
  (testing "evaluation of small expressions"
    #_(is false) ; Needed for recent versions of CIDER!

    ;; testing whether simple access of $ works.
    (is (= [1 2 3]
           (rew/rewrite* :ptag/code-block "($ := [{'a' : 1}, {'a' : 2}, {'a' : 3}]; a)" :execute? true)))

    ;; This tests use of $match
    (is (= {"match" "foo", "index" 2, "groups" []}
           (rew/rewrite* :ptag/exp "$match(\"bbfoovar\", /foo/)" :execute? true)))

    ;; This tests use of $match
    (is (= {"match" "xababy", "index" 6, "groups" ["ab"]}
           (rew/rewrite* :ptag/exp "$match(\"foobarxababy\",/\\d*x(ab)+y/)" :execute? true)))

    ;; This tests 'immediate use' of a function
    (is (= 4 (rew/rewrite* :ptag/exp "function($x){$x+1}(3)" :execute? true)))

    ;; This tests another sort of immediate use, using the threading macro.
    (is (= 5 (rew/rewrite* :ptag/exp "4 ~> function($x){$x+1}()" :execute? true)))

    ;; This tests mapping and the singleton behavior.
    (is (= [100 100 100] (rew/rewrite* :ptag/exp "['a', 'b', 'c'].$sum(100)" :execute? true)))


    (is (= [1, 2, 3] (rew/rewrite* :ptag/exp "[1, 2, 3].$sum($)" :execute? true)))

    ;; This tests binary precedence and use of the context variable.
    (is (= 11 (rew/rewrite* :ptag/code-block "($ := {'a' : 1, 'b' : 2, 'c' : 3, 'd' : 4}; a + b * c + d)" :execute? true)))

    ;; This tests reduce.
    (is (= 15 (rew/rewrite* :ptag/exp "$reduce([1..5], function($i, $j){$i + $j})" :execute? true)))

    ;; This tests reduce on one arg.
    (is (= 3 (rew/rewrite* :ptag/exp "$reduce([3], function($i, $j){$i + $j})" :execute? true)))

    ;; This tests reduce on one arg and an initial value.
    (is (= 5 (rew/rewrite* :ptag/exp "$reduce([3], function($i, $j){$i + $j}, 2)" :execute? true)))))

(deftest code-block-evaluations true
  (testing "evaluation of code bodies"
    (is (= [2 3 4 5 6] (rew/rewrite* :ptag/code-block "($inc := function($i)    {$i + 1};  $map([1..5], $inc))"         :execute? true)))
    (is (= 15          (rew/rewrite* :ptag/code-block "($add := function($i, $j){$i + $j}; $reduce([1..5], $add))"      :execute? true)))
    (is (= 115         (rew/rewrite* :ptag/code-block "($add := function($i, $j){$i + $j}; $reduce([1..5], $add, 100))" :execute? true)))

    ;; Tests for array indexing.
    (is (= "b"         (rew/rewrite* :ptag/code-block "($v := ['a', 'b', 'c' 'd']; $v[1])"  :execute? true)))
    (is (= "a"         (rew/rewrite* :ptag/code-block "($v := ['a', 'b', 'c' 'd']; $v[-4])" :execute? true)))
    (is (= "a"         (rew/rewrite* :ptag/code-block "($v := ['a', 'b', 'c' 'd']; $v[0])"  :execute? true)))

    ;; Test of filter 'delimited expressions'
    (is (= [{"type" "mobile", "num" "555-123-4567"} {"type" "mobile", "num" "555-333-4444"}]
           (rew/rewrite* :ptag/code-block "($ := [{'Phone' : {'type' : 'mobile', 'num' : '555-123-4567'}}
                                                  {'Phone' : {'type' : 'work',   'num' : 'XXX-123-4567'}}
                                                  {'Phone' : {'type' : 'mobile', 'num' : '555-333-4444'}}];
                                             Phone[type = 'mobile'] )" :execute? true)))

    ;; Test of map 'delimited expressions'
    (is (= [100 200]
           (rew/rewrite* :ptag/code-block "($ := [{'Product' : {'price' : 50, 'quantity' : 2}}
                                                  {'Product' : {'price' : 50, 'quantity' : 4}}];
                                             Product.(price * quantity) )" :execute? true)))

    ;; Another test of map 'delimited expressions'; this one from try.jsonata
    (is (= [68.9, 21.67, 137.8, 107.99]
           (do (reset-try-data)
               (rew/rewrite* :ptag/exp "Account.Order.Product.(Price*Quantity)" :execute? true))))

    ;; Test of reduce using delimiters. ToDo: Implement the backquote thing.
    (is (= {"Bowler Hat" [68.9, 137.8], "Trilby hat" 21.67, "Cloak" 107.99}
           (do (reset-try-data)
               (rew/rewrite* :ptag/exp  "Account.Order.Product{`Product Name` : $.(Price*Quantity)}"))))))

(def addr-data
  "( $ADDR :=
         [{'name'    : 'Peter',
           'street'  : '123 Mockingbird Lane',
           'zipcode' : '20898',
           'phone'   : {'mobile' : '123-456-7890'}},

          {'name'    : 'Bill',
           'street'  : '23 Main Street',
           'zipcode' : '07010-3544'},

          {'name'    : 'Lisa',
           'street'  : '903 Forest Road',
           'zipcode' : '10878'}]; ")

(deftest user-guide-tests true
  (testing "small code examples from the user's guide"
    (is (= ["20898" "07010-3544" "10878"]
           (rew/rewrite* :ptag/code-block (str addr-data "$ADDR.zipcode )") :execute? true)))
    (is (= ["20898" "10878"]
           (rew/rewrite* :ptag/code-block (str addr-data "$ADDR.zipcode[$match(/^[0-9]+$/)] )") :execute? true)))
    (is (= "123-456-7890"
           (rew/rewrite* :ptag/code-block (str addr-data "$ADDR.phone.mobile )") :execute? true)))
    (is (= [68.9, 21.67, 137.8, 107.99]
           (rew/rewrite* :ptag/code-block "data/testing/map-examples/iteration/i6.mmp" :file? true :execute? true)))))

(defn tryme []
  (let [_x1 (bi/reset-special! bi/$ [(-> {} (assoc "Product" (-> {}
                                                                 (assoc "price" 50)
                                                                 (assoc "quantity" 2))))
                                     (-> {} (assoc "Product" (-> {} (assoc "price" 50)
                                                                 (assoc "quantity" 4))))])]
    (bi/apply-map (bi/dot-map "Product")
                  (fn [foo] (bi/* (bi/get-field foo "price") (bi/get-field foo "quantity"))))))
