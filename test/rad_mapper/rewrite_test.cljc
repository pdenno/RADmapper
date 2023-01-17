(ns rad-mapper.rewrite-test
  "Test the rewrite of parse trees. rew/processRM is a toplevel function"
  (:require
   [clojure.test :refer [deftest is testing]]
   [rad-mapper.evaluate :as ev]
   [rad-mapper.rewrite  :as rew]
   [dev.dutil-util :refer [run]] ; Needed; ignore clj-kondo warning.
  #?(:clj [dev.dutil-macros :as dutil]))
#?(:cljs (:require-macros [dev.dutil-macros :as dutil])))

(defn run-test
  "run-test for rewrite sets :rewrite? true."
  [exp expect & {:keys [keep-meta?]}]
  (dutil/run-test exp expect :rewrite? true :keep-meta? keep-meta?))

(deftest value-map-rewrite
  (testing "that things like ['a','b','c'].[1] translate correctly."
    (is (= '[{:typ :Array, :exprs ["a" "b" "c"]}
             bi/value-step
             {:typ :ValueStep, :body 1}]
           (rew/rewrite-value-step [{:typ :Array, :exprs ["a" "b" "c"]}
                                    :op/get-step
                                    {:typ :ApplyFilter, :body 1}])))))

;;; (not= #"abc" #"abc") so don't bother testing things containing regular expressions.
(deftest simple
  (testing "Simple (rewrite):"
    (testing "miscellaneous simple rewrites"
      (run-test "1" 1)
      (run-test "[1, 2, 3]"  [1 2 3])
      (run-test "1 + 2" '(bi/add 1 2))
      (run-test "[1..5]" '(-> (range 1 (inc 5)) vec))
      ;(run-test "$A[1]" '(bi/run-steps (bi/init-step $A) (bi/filter-step (fn [_x1] (bi/with-context _x1 1))))) ; I think I got rid of bi/w-c.
      (run-test "$sum($v.field)" '(bi/$sum (bi/run-steps (bi/init-step $v) (bi/get-step "field"))))
      (run-test "$sum(a.b)"      '(bi/$sum (bi/run-steps (bi/get-step "a") (bi/get-step "b"))))
      (run-test "(A * B)"        '(bi/primary (bi/multiply (bi/get-scoped "A") (bi/get-scoped "B"))))
      (run-test "4 ~> $f()"      '(bi/thread 4 (fn [_x1] (bi/fncall _x1 {:args [], :func $f}))))
      (run-test "$" '(bi/deref$))
      (run-test "$foo" '$foo))

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
                  (bi/primary (bi/multiply (bi/get-scoped "P") (bi/get-scoped "Q")))))

      (run-test "$data.(A * B)"
                '(bi/run-steps
                  (bi/init-step $data)
                  (bi/primary (bi/multiply (bi/get-scoped "A") (bi/get-scoped "B")))))

      (run-test "a.b.c.d + e.f"
                '(bi/add (bi/run-steps
                        (bi/get-step "a")
                        (bi/get-step "b")
                        (bi/get-step "c")
                        (bi/get-step "d"))
                       (bi/run-steps (bi/get-step "e")
                                     (bi/get-step "f"))))
      (run-test "a + b * $f(c + d)"
                '(bi/add (bi/get-step "a")
                         (bi/multiply (bi/get-step "b")
                                      (bi/fncall {:args [(bi/add (bi/get-step "c") (bi/get-step "d"))],
                                                  :func $f}))))

      (run-test "$var.a + b.c.(P * Q)"
                '(bi/add (bi/run-steps
                        (bi/init-step $var)
                        (bi/get-step "a"))
                       (bi/run-steps
                        (bi/get-step "b")
                        (bi/get-step "c")
                        (bi/primary (bi/multiply (bi/get-scoped "P") (bi/get-scoped "Q"))))))

      (run-test "$var.a + b.c.(P * Q) + d"
                '(bi/add (bi/add (bi/run-steps
                              (bi/init-step $var)
                              (bi/get-step "a"))
                             (bi/run-steps
                              (bi/get-step "b")
                              (bi/get-step "c")
                              (bi/primary (bi/multiply (bi/get-scoped "P") (bi/get-scoped "Q")))))
                       (bi/get-step "d")))

      (run-test "$var.a + b.c.(P * Q) + d.(M * N)"
                '(bi/add (bi/add (bi/run-steps
                              (bi/init-step $var)
                              (bi/get-step "a"))
                             (bi/run-steps
                              (bi/get-step "b")
                              (bi/get-step "c")
                              (bi/primary (bi/multiply (bi/get-scoped "P") (bi/get-scoped "Q")))))
                       (bi/run-steps (bi/get-step "d") (bi/primary (bi/multiply (bi/get-scoped "M") (bi/get-scoped "N"))))))

      (run-test "$var.a.b.c.(P * Q)"
                '(bi/run-steps
                  (bi/init-step $var)
                  (bi/get-step "a")
                  (bi/get-step "b")
                  (bi/get-step "c")
                  (bi/primary (bi/multiply (bi/get-scoped "P") (bi/get-scoped "Q")))))

      (run-test "$sum($v.a.(P * Q))"
                '(bi/$sum
                  (bi/run-steps
                   (bi/init-step $v)
                   (bi/get-step "a")
                   (bi/primary (bi/multiply (bi/get-scoped "P") (bi/get-scoped "Q")))))))


    (testing "miscellaneous"
      (run-test "function($v,$i,$a) { $v.cbc_InvoicedQuantity < 0 }"
                '(with-meta
                   (fn [$v $i $a]
                     (bi/lt (bi/run-steps
                            (bi/init-step $v)
                            (bi/get-step "cbc_InvoicedQuantity")) 0))
                   #:bi{:type :bi/user-fn, :params '[$v $i $a]}))

      (run-test "($inc := function($v) { $v + 1}; $map([1, 2, 3], $inc))"
                '(bi/primary (let [$inc
                                   (with-meta (fn [$v] (bi/add $v 1))
                                     #:bi{:type :bi/user-fn, :params (quote [$v])})]
                               (bi/$map [1 2 3] $inc))))

      (run-test "$reduce([1..5], function($x,$y){$x + $y}, 100)"
                '(bi/$reduce (-> (range 1 (inc 5)) vec)
                             (with-meta
                               (fn [$x $y] (bi/add $x $y))
                               #:bi{:type :bi/user-fn, :params (quote [$x $y])}) 100)
                :keep-meta? true)

      (run-test "$fn1($fn2($v).a.b)"
                '(bi/fncall {:args [(bi/run-steps
                                     (bi/init-step
                                      (bi/fncall {:args [$v], :func $fn2}))
                                     (bi/get-step "a")
                                     (bi/get-step "b"))],
                             :func $fn1}))

      (run-test "$sum($filter($v.InvoiceLine, function($v,$i,$a) { $v.Quantity < 0 }).Price.PriceAmount)"
                '(bi/$sum
                  (bi/run-steps
                   (bi/init-step
                    (bi/$filter (bi/run-steps
                                 (bi/init-step $v)
                                 (bi/get-step "InvoiceLine"))
                                (with-meta (fn [$v $i $a]
                                             (bi/lt (bi/run-steps
                                                    (bi/init-step $v)
                                                    (bi/get-step "Quantity")) 0))
                                  #:bi{:type :bi/user-fn, :params (quote [$v $i $a])})))
                   (bi/get-step "Price") (bi/get-step "PriceAmount")))))))

(deftest code-block
  (testing "Testing that code blocks handle binding and special jvars correctly"

    ;; ToDo: I need to look at JSONata use of \; on this one.
    (run-test  "( $x := 1; $f($x); $g($x) )"
               '(bi/primary
                 (let [$x 1]
                   (bi/fncall {:args [$x],
                               :func $f})
                   (bi/fncall {:args [$x],
                               :func $g}))))

    ;; A side-effected dummy binding is used for the special $.
    (run-test  "( $x   :=  'foo';
                  $    := {'a' : 1};
                  $y   := 'bat';
                  $yy  := 'ybat';
                  $f($x, $y) )"
               '(bi/primary
                 (let [$x "foo"
                       _x1 (bim/set-context! (-> {} (assoc "a" 1)))
                       $y "bat"
                       $yy "ybat"]
                   (bi/fncall {:args [$x $y], :func $f}))))))

(deftest options-map
  (testing "rewriting an options map"
    (is (= '{:keepDBid true, :otherStuff true}
           (ev/processRM :ptag/options-map "<|keepDBid   : true,
                                              otherStuff : true|>"
                         {:rewrite? true})))))

(deftest literal-bsets
  (testing "rewriting a literal-bsets"
    #?(:cljs (is (= '(cljs.core/-> {}
                                   (cljs.core/assoc '?idKey "KeyVal")
                                   (cljs.core/assoc '?idKeyref "KeyrefVal")
                                   (cljs.core/assoc '?instruct "some instruction")
                                   (cljs.core/assoc '?method "some method"))
                    (ev/processRM :ptag/exp
                                  "{?idKey    : 'KeyVal',
                           ?idKeyref : 'KeyrefVal',
                           ?instruct : 'some instruction',
                           ?method   : 'some method'}"
                                  {:rewrite? true}))))))

(deftest express-def
  (testing "rewriting express definitions containing the key construct"
    (is true)
    ;; ToDo: Find where a quote is missing in the the following ;^)
    #_(is (= ('bi/express
            {:schema
             '{:_rm/ROOT #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
               :box/keyword-val #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
               :_rm/t*type--owners|?ownerName|t*type
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :_rm/cat-key ["owners" ?ownerName "t/type"],
                :_rm/self :_rm/t*type--owners|?ownerName|t*type,
                :_rm/user-key "t/type"},
               :owner/id {:db/unique :db.unique/identity, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :_rm/cat-key ["owners" ?ownerName], :_rm/self :owner/id, :_rm/user-key ?ownerName, :_rm/exp-key? true},
               :_rm/user-key #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
               :box/string-val #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
               :_rm/t*type--owners|?ownerName|owner*systems|?systemName|system*devices|?deviceName|t*type
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :_rm/cat-key ["owners" ?ownerName "owner/systems" ?systemName "system/devices" ?deviceName "t/type"],
                :_rm/self :_rm/t*type--owners|?ownerName|owner*systems|?systemName|system*devices|?deviceName|t*type,
                :_rm/user-key "t/type"},
               :system/id
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :_rm/cat-key ["owners" ?ownerName "owner/systems" ?systemName],
                :_rm/self :system/id,
                :_rm/user-key ?systemName,
                :_rm/exp-key? true},
               :_rm/vals #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
               :box/boolean-val #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean},
               :_rm/owners--owners
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :_rm/cat-key ["owners" ?ownerName "owner/systems" ?systemName],
                :_rm/self :_rm/owners--owners,
                :_rm/user-key "owners"},
               :_rm/device*status--owners|?ownerName|owner*systems|?systemName|system*devices|?deviceName|device*status
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :_rm/cat-key ["owners" ?ownerName "owner/systems" ?systemName "system/devices" ?deviceName "device/status"],
                :_rm/self :_rm/device*status--owners|?ownerName|owner*systems|?systemName|system*devices|?deviceName|device*status,
                :_rm/user-key "device/status"},
               :_rm/owner*systems--owners|?ownerName|owner*systems
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :_rm/cat-key ["owners" ?ownerName "owner/systems" ?systemName "system/devices"],
                :_rm/self :_rm/owner*systems--owners|?ownerName|owner*systems,
                :_rm/user-key "owner/systems"},
               :box/number-val #:db{:cardinality :db.cardinality/one, :valueType :db.type/number},
               :_rm/val #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref},
               :_rm/t*type--owners|?ownerName|owner*systems|?systemName|t*type
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :_rm/cat-key ["owners" ?ownerName "owner/systems" ?systemName "t/type"],
                :_rm/self :_rm/t*type--owners|?ownerName|owner*systems|?systemName|t*type,
                :_rm/user-key "t/type"},
               :_rm/attrs #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
               :device/id
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :_rm/cat-key ["owners" ?ownerName "owner/systems" ?systemName "system/devices" ?deviceName],
                :_rm/self :device/id,
                :_rm/user-key ?deviceName,
                :_rm/exp-key? true},
               :_rm/ek-val #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref},
               :_rm/system*devices--owners|?ownerName|owner*systems|?systemName|system*devices
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :_rm/cat-key ["owners" ?ownerName "owner/systems" ?systemName "system/devices" ?deviceName],
                :_rm/self :_rm/system*devices--owners|?ownerName|owner*systems|?systemName|system*devices,
                :_rm/user-key "system/devices"}},
             :reduce-body
             '[#:_rm{:owners--owners (:rm/express-key "owners"),
                     :user-key "owners",
                     :attrs
                     {:owner/id (:rm/express-key "owners" ?ownerName),
                      :_rm/user-key "owner/id",
                      :_rm/ek-val ?ownerName,
                      :_rm/attrs
                      [#:_rm{:t*type--owners|?ownerName|t*type (:rm/express-key "owners" ?ownerName "t/type"), :user-key "t/type", :val "OWNER"}
                       #:_rm{:owner*systems--owners|?ownerName|owner*systems (:rm/express-key "owners" ?ownerName "owner/systems"),
                             :user-key "owner/systems",
                             :vals
                             [{:system/id (:rm/express-key "owners" ?ownerName "owner/systems" ?systemName),
                               :_rm/user-key "system/id",
                               :_rm/ek-val ?systemName,
                               :_rm/attrs
                               [#:_rm{:t*type--owners|?ownerName|owner*systems|?systemName|t*type (:rm/express-key "owners" ?ownerName "owner/systems" ?systemName "t/type"), :user-key "t/type", :val "SYSTEM"}
                                #:_rm{:system*devices--owners|?ownerName|owner*systems|?systemName|system*devices (:rm/express-key "owners" ?ownerName "owner/systems" ?systemName "system/devices"),
                                      :user-key "system/devices",
                                      :vals
                                      [{:device/id (:rm/express-key "owners" ?ownerName "owner/systems" ?systemName "system/devices" ?deviceName),
                                        :_rm/user-key "device/id",
                                        :_rm/ek-val ?deviceName,
                                        :_rm/attrs
                                        [#:_rm{:t*type--owners|?ownerName|owner*systems|?systemName|system*devices|?deviceName|t*type
                                               (:rm/express-key "owners" ?ownerName "owner/systems" ?systemName "system/devices" ?deviceName "t/type"),
                                               :user-key "t/type",
                                               :val "DEVICE"}
                                         #:_rm{:device*status--owners|?ownerName|owner*systems|?systemName|system*devices|?deviceName|device*status
                                               (:rm/express-key "owners" ?ownerName "owner/systems" ?systemName "system/devices" ?deviceName "device/status"),
                                               :user-key "device/status",
                                               :val ?status}]}]}]}]}]}}],
             :params '(),
             :key-order ["owners" "t/type" "owner/id" "owner/systems" "t/type" "system/id" "system/devices" "t/type" "device/id" "device/status"],
             :options 'nil,
             :base-body
             '{"owners"
               {"t/type" "OWNER",
                "owner/id" (:rm/express-key ?ownerName),
                "owner/systems" [{"t/type" "SYSTEM", "system/id" (:rm/express-key ?systemName), "system/devices" [{"t/type" "DEVICE", "device/id" (:rm/express-key ?deviceName), "device/status" ?status}]}]}}})


           (ev/processRM :ptag/express-def
                         "express{{'owners': {'t/type'       : 'OWNER',
                                              'owner/id'     : key(?ownerName),
                                              'owner/systems': [{'t/type'         : 'SYSTEM',
                                                                 'system/id'      : key(?systemName),
                                                                 'system/devices' : [{'t/type'         : 'DEVICE',
                                                                                      'device/id'      : key(?deviceName),
                                                                                      'device/status'  : ?status}]}]}}}"
                         {:rewrite? true})))))
