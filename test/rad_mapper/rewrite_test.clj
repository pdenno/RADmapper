(ns rad-mapper.rewrite-test
  "Test the rewrite of parse trees. rew/rewrite* is a toplevel function"
  (:require
   [clojure.test       :refer  [deftest is testing]]
   [rad-mapper.builtins :as bi]
   [rad-mapper.devl.devl-util :refer [#_nicer nicer-sym]]
   [rad-mapper.rewrite  :as rew]))

(deftest value-map-rewrite
  (testing "that things like ['a','b','c'].[0] translate correctly."
    (is (= '[{:_type :JaArray, :exprs ["a" "b" "c"]}
             bi/value-step
             {:_type :JaValueStep, :body 1}]
           (rew/rewrite-value-step [{:_type :JaArray, :exprs ["a" "b" "c"]}
                                    'bi/get-step
                                    {:_type :JaApplyFilter, :body 1}])))))

(defn rew
  "Default rewriting function"
  [exp & {:keys [simplify? execute? debug? debug-parse? skip-top?]}]
  (let [rewrite? (not (or simplify? execute?))]
    (-> (rew/rewrite* :ptag/exp exp
                      :simplify? simplify?
                      :rewrite? rewrite?
                      :execute? execute?
                      :debug? debug?
                      :skip-top? skip-top?
                      :debug-parse? debug-parse?)
        nicer-sym)))

(defmacro run-test
  "Print the test form using testing, run the test."
  [form-string expect]
  `(testing ~(str "\n(rew \"" form-string "\")")
     (is (= ~expect (rew ~form-string)))))

;;; (not= #"abc" #"abc") so don't bother testing things containing regular expressions.
(deftest simple
  (testing "Simple (rewrite):"
    (testing "miscellaneous simple rewrites"
      (binding [bi/*test-sym* 'foo]
        (run-test "1" 1)
        (run-test "[1, 2, 3]"  [1 2 3])
        (run-test "1 + 2" '(bi/+ 1 2))
        (run-test "[1..5]" '(range 1 (inc 5)))
        (run-test "$A[1]" '(bi/run-steps (bi/init-step $A) (bi/filter-step (fn [_x1] (bi/with-context _x1 1)))))
        (run-test "$sum($v.field)" '(bi/$sum (bi/run-steps (bi/init-step $v) (bi/get-step "field"))))
        (run-test "$sum(a.b)"      '(bi/$sum (bi/run-steps (bi/get-step "a") (bi/get-step "b"))))
        (run-test "(A * B)"        '(bi/primary (bi/* (bi/get-scoped "A") (bi/get-scoped "B"))))
        (run-test "4 ~> $f()"      '(bi/thread 4 ($f)))
        (run-test "$" '(bi/deref$))
        (run-test "$foo" '$foo)))

    (testing "tokenizer"
      (run-test "true?'a':'b'" '(if true "a" "b")))

    ;; ToDo: This one doesn't work yet, but it tokenizes. See parse_test.clj.
    ;; I think the problem is that triple-roles are literals yet. I'm not sure that they should be.
    #_(testing "triple-role in conditional"
        (is (= '(if true :foo/bar :foo/bat) (run-test "true?:foo/bar::foo/bat"))))

    (testing "the synax reordering of map/filter on paths"
      (run-test "$var.(P * Q)"
                '(bi/run-steps
                  (bi/init-step $var)
                  (bi/primary (bi/* (bi/get-scoped "P") (bi/get-scoped "Q")))))

      (run-test "$data.(A * B)"
                '(bi/run-steps
                  (bi/init-step $data)
                  (bi/primary (bi/* (bi/get-scoped "A") (bi/get-scoped "B")))))

      (run-test "a.b.c.d + e.f"
                '(bi/+ (bi/run-steps
                        (bi/get-step "a")
                        (bi/get-step "b")
                        (bi/get-step "c")
                        (bi/get-step "d"))
                       (bi/run-steps (bi/get-step "e")
                                     (bi/get-step "f"))))
      (run-test "a + b * $f(c + d)"
                '(bi/+ (bi/get-step "a")
                       (bi/* (bi/get-step "b")
                             ($f (bi/+ (bi/get-step "c") (bi/get-step "d"))))))

      (run-test "$var.a + b.c.(P * Q)"
                '(bi/+ (bi/run-steps
                        (bi/init-step $var)
                        (bi/get-step "a"))
                       (bi/run-steps
                        (bi/get-step "b")
                        (bi/get-step "c")
                        (bi/primary (bi/* (bi/get-scoped "P") (bi/get-scoped "Q"))))))

      (run-test "$var.a + b.c.(P * Q) + d"
                '(bi/+ (bi/+ (bi/run-steps
                              (bi/init-step $var)
                              (bi/get-step "a"))
                             (bi/run-steps
                              (bi/get-step "b")
                              (bi/get-step "c")
                              (bi/primary (bi/* (bi/get-scoped "P") (bi/get-scoped "Q")))))
                       (bi/get-step "d")))

      (run-test "$var.a + b.c.(P * Q) + d.(M * N)"
                '(bi/+ (bi/+ (bi/run-steps
                              (bi/init-step $var)
                              (bi/get-step "a"))
                             (bi/run-steps
                              (bi/get-step "b")
                              (bi/get-step "c")
                              (bi/primary (bi/* (bi/get-scoped "P") (bi/get-scoped "Q")))))
                       (bi/run-steps (bi/get-step "d") (bi/primary (bi/* (bi/get-scoped "M") (bi/get-scoped "N"))))))

      (run-test "$var.a.b.c.(P * Q)"
                '(bi/run-steps
                  (bi/init-step $var)
                  (bi/get-step "a")
                  (bi/get-step "b")
                  (bi/get-step "c")
                  (bi/primary (bi/* (bi/get-scoped "P") (bi/get-scoped "Q")))))

      (run-test "$sum($v.a.(P * Q))"
                '(bi/$sum
                  (bi/run-steps
                   (bi/init-step $v)
                   (bi/get-step "a")
                   (bi/primary (bi/* (bi/get-scoped "P") (bi/get-scoped "Q")))))))


    (testing "miscellaneous"
      (run-test "function($v,$i,$a) { $v.cbc_InvoicedQuantity < 0 }"
                '(with-meta
                   (fn [$v $i $a]
                     (bi/< (bi/run-steps
                            (bi/init-step $v)
                            (bi/get-step "cbc_InvoicedQuantity")) 0))
                   #:bi{:type :bi/user-fn, :params '[$v $i $a]}))

      (run-test "($inc := function($v) { $v + 1}; $map([1, 2, 3], $inc))"
                '(bi/primary (let [$inc
                                   (with-meta (fn [$v] (bi/+ $v 1))
                                     #:bi{:type :bi/user-fn, :params (quote [$v])})]
                               (bi/$map [1 2 3] $inc))))

      (run-test "$reduce([1..5], function($x,$y){$x + $y}, 100)"
                '(bi/$reduce (range 1 (inc 5))
                             (with-meta
                               (fn [$x $y] (bi/+ $x $y))
                               #:bi{:type :bi/user-fn, :params (quote [$x $y])}) 100))

      (run-test "$fn1($fn2($v).a.b)"
                '($fn1 (bi/run-steps (bi/init-step ($fn2 $v)) (bi/get-step "a") (bi/get-step "b"))))

      (run-test "$sum($filter($v.InvoiceLine, function($v,$i,$a) { $v.Quantity < 0 }).Price.PriceAmount)"
                '(bi/$sum
                  (bi/run-steps
                   (bi/init-step
                    (bi/$filter (bi/run-steps
                                 (bi/init-step $v)
                                 (bi/get-step "InvoiceLine"))
                                (with-meta (fn [$v $i $a]
                                             (bi/< (bi/run-steps
                                                    (bi/init-step $v)
                                                    (bi/get-step "Quantity")) 0))
                                  #:bi{:type :bi/user-fn, :params (quote [$v $i $a])})))
                   (bi/get-step "Price") (bi/get-step "PriceAmount")))))))

(deftest code-block
  (testing "Testing that code blocks handle binding and special jvars correctly"

    ;; ToDo: I need to look at JSONata use of \; on this one.
    (run-test  "( $x := 1; $f($x) $g($x) )"
               '(bi/primary (let [$x 1] ($f $x) ($g $x))))

    ;; A side-effected dummy binding is used for the special $.
    (run-test  "( $x   :=  'foo';
                  $    := {'a' : 1};
                  $y   := 'bat';
                  $yy  := 'ybat';
                  $f($x, $y) )"
               '(bi/primary
                 (let [$x "foo"
                       _x1 (bi/set-context! (-> {} (assoc "a" 1)))
                       $y "bat"
                       $yy "ybat"]
                   ($f $x $y))))))
