(ns rad-mapper.rewrite-test
  "Test the rewrite of parse trees. rew/processRM is a toplevel function"
  (:require
   [clojure.test :refer [deftest is testing]]
   [rad-mapper.evaluate :as ev]
   [rad-mapper.rewrite  :as rew]
   [dev.dutil-util :refer [run remove-meta]]
   #?(:clj [dev.dutil-macros :as dutilm :refer [run-test unquote-body]]))
  #?(:cljs (:require-macros [dev.dutil-macros :as dutilm :refer [run-test unquote-body]])))

(defn run-rew
  "run-test for rewrite sets :rewrite? true."
  [exp expect & {:keys [keep-meta?]}]
  (dutilm/run-test exp expect :rewrite? true :keep-meta? keep-meta?))

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
      (run-rew "1" 1)
      (run-rew "[1, 2, 3]"  [1 2 3])
      (run-rew "1 + 2" '(bi/add 1 2))
      (run-rew "[1..5]" '(-> (range 1 (inc 5)) vec))
      ;(run-rew "$A[1]" '(bi/run-steps (bi/init-step $A) (bi/filter-step (fn [_x1] (bi/with-context _x1 1))))) ; I think I got rid of bi/w-c.
      (run-rew "$sum($v.field)" '(bi/$sum (bi/run-steps (bi/init-step $v) (bi/get-step "field"))))
      (run-rew "$sum(a.b)"      '(bi/$sum (bi/run-steps (bi/get-step "a") (bi/get-step "b"))))
      (run-rew "(A * B)"        '(bi/primary (bi/multiply (bi/get-scoped "A") (bi/get-scoped "B"))))
      (run-rew "4 ~> $f()"      '(bi/thread 4 (fn [_x1] (bi/fncall _x1 {:args [], :func $f}))))
      (run-rew "$" '(bi/deref$))
      (run-rew "$foo" '$foo))

    (testing "tokenizer"
      (run-rew "true?'a':'b'" '(bi/conditional true "a" "b")))

    ;; ToDo: This one doesn't work yet, but it tokenizes. See parse_test.clj.
    ;; I think the problem is that triple-roles are literals yet. I'm not sure that they should be.
    #_(testing "triple-role in conditional"
        (is (= '(if true :foo/bar :foo/bat) (run-rew "true?:foo/bar::foo/bat"))))

    (testing "the synax reordering of map/filter on paths"
      (run-rew "$var.(P * Q)"
                '(bi/run-steps
                  (bi/init-step $var)
                  (bi/primary (bi/multiply (bi/get-scoped "P") (bi/get-scoped "Q")))))

      (run-rew "$data.(A * B)"
                '(bi/run-steps
                  (bi/init-step $data)
                  (bi/primary (bi/multiply (bi/get-scoped "A") (bi/get-scoped "B")))))

      (run-rew "a.b.c.d + e.f"
                '(bi/add (bi/run-steps
                        (bi/get-step "a")
                        (bi/get-step "b")
                        (bi/get-step "c")
                        (bi/get-step "d"))
                       (bi/run-steps (bi/get-step "e")
                                     (bi/get-step "f"))))
      (run-rew "a + b * $f(c + d)"
                '(bi/add (bi/get-step "a")
                         (bi/multiply (bi/get-step "b")
                                      (bi/fncall {:args [(bi/add (bi/get-step "c") (bi/get-step "d"))],
                                                  :func $f}))))

      (run-rew "$var.a + b.c.(P * Q)"
                '(bi/add (bi/run-steps
                        (bi/init-step $var)
                        (bi/get-step "a"))
                       (bi/run-steps
                        (bi/get-step "b")
                        (bi/get-step "c")
                        (bi/primary (bi/multiply (bi/get-scoped "P") (bi/get-scoped "Q"))))))

      (run-rew "$var.a + b.c.(P * Q) + d"
                '(bi/add (bi/add (bi/run-steps
                              (bi/init-step $var)
                              (bi/get-step "a"))
                             (bi/run-steps
                              (bi/get-step "b")
                              (bi/get-step "c")
                              (bi/primary (bi/multiply (bi/get-scoped "P") (bi/get-scoped "Q")))))
                       (bi/get-step "d")))

      (run-rew "$var.a + b.c.(P * Q) + d.(M * N)"
                '(bi/add (bi/add (bi/run-steps
                              (bi/init-step $var)
                              (bi/get-step "a"))
                             (bi/run-steps
                              (bi/get-step "b")
                              (bi/get-step "c")
                              (bi/primary (bi/multiply (bi/get-scoped "P") (bi/get-scoped "Q")))))
                       (bi/run-steps (bi/get-step "d") (bi/primary (bi/multiply (bi/get-scoped "M") (bi/get-scoped "N"))))))

      (run-rew "$var.a.b.c.(P * Q)"
                '(bi/run-steps
                  (bi/init-step $var)
                  (bi/get-step "a")
                  (bi/get-step "b")
                  (bi/get-step "c")
                  (bi/primary (bi/multiply (bi/get-scoped "P") (bi/get-scoped "Q")))))

      (run-rew "$sum($v.a.(P * Q))"
                '(bi/$sum
                  (bi/run-steps
                   (bi/init-step $v)
                   (bi/get-step "a")
                   (bi/primary (bi/multiply (bi/get-scoped "P") (bi/get-scoped "Q")))))))


    (testing "miscellaneous"
      (run-rew "function($v,$i,$a) { $v.cbc_InvoicedQuantity < 0 }"
                '(with-meta
                   (fn [$v $i $a]
                     (bi/lt (bi/run-steps
                            (bi/init-step $v)
                            (bi/get-step "cbc_InvoicedQuantity")) 0))
                   #:bi{:type :bi/user-fn, :params '[$v $i $a]}))

      (run-rew "($inc := function($v) { $v + 1}; $map([1, 2, 3], $inc))"
               `(letfn [($inc [$v] (bi/add $v 1))]
                  (bi/$map [1 2 3] $inc)))

      (run-rew "$reduce([1..5], function($x,$y){$x + $y}, 100)"
                '(bi/$reduce (-> (range 1 (inc 5)) vec)
                             (with-meta
                               (fn [$x $y] (bi/add $x $y))
                               #:bi{:type :bi/user-fn, :params (quote [$x $y])}) 100)
                :keep-meta? true)

      (run-rew "$fn1($fn2($v).a.b)"
                '(bi/fncall {:args [(bi/run-steps
                                     (bi/init-step
                                      (bi/fncall {:args [$v], :func $fn2}))
                                     (bi/get-step "a")
                                     (bi/get-step "b"))],
                             :func $fn1}))

      (run-rew "$sum($filter($v.InvoiceLine, function($v,$i,$a) { $v.Quantity < 0 }).Price.PriceAmount)"
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
  (testing "Testing that code blocks handle binding, letfn, and special jvars correctly"

    (run-rew "( $a := 1; $b := 2; $f := function($x){$x+1}; $c := 3 )"
             '(let [$a 1 $b 2] (letfn [($f [$x] (bi/add $x 1))] (let [$c 3] $c))))

    ;; ToDo: There are no side-effects, so I think this should signal an error when rewriting.
    #_(run-rew  "( $x := 1; $f($x); $g($x) )"
              '(let [$x 1]
                 (bi/fncall {:args [$x],
                             :func $f})
                 (bi/fncall {:args [$x],
                             :func $g})))

    ;; A side-effected dummy binding is used for the special $.
    (run-rew  "( $x   :=  'foo';
                 $    := {'a' : 1};
                 $y   := 'bat';
                 $yy  := 'ybat';
                 $f($x, $y) )"
              '(let [$x "foo"
                     _x1 (bim/set-context! {"a" 1})
                     $y "bat"
                     $yy "ybat"]
                 (bi/fncall {:args [$x $y], :func $f})))))

(deftest options-map
  (testing "rewriting an options map"
    (is (= '{:keepDBid true, :otherStuff true}
           (ev/processRM :ptag/options-map "<|keepDBid   : true,
                                              otherStuff : true|>"
                         {:rewrite? true})))))

;;; ToDo: Investigate this rewrite that contains :op/or {:typ :ReduceExp, :kv-pairs []}.
;;;       The ReduceExp isn't what I intended.
;;;       Note also that JSONata return false for "$lookup({}, 'a') or {}" (which isn't what I need either)!
#_(deftest empty-object-in-exp
  (testing "Empty object in an expression"
    (run-rew "$assoc($x, $c, $lookup($shape($c, $spc), $c) or {})"
             "Whatever, it doesn't rewrite.")))

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
             '{:redex/ROOT #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
               :box/keyword-val #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
               :redex/t*type--owners|?ownerName|t*type
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :redex/cat-key ["owners" ?ownerName "t/type"],
                :redex/self :redex/t*type--owners|?ownerName|t*type,
                :redex/user-key "t/type"},
               :owner/id {:db/unique :db.unique/identity, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :redex/cat-key ["owners" ?ownerName], :redex/self :owner/id, :redex/user-key ?ownerName, :redex/exp-key? true},
               :redex/user-key #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
               :box/string-val #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
               :redex/t*type--owners|?ownerName|owner*systems|?systemName|system*devices|?deviceName|t*type
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :redex/cat-key ["owners" ?ownerName "owner/systems" ?systemName "system/devices" ?deviceName "t/type"],
                :redex/self :redex/t*type--owners|?ownerName|owner*systems|?systemName|system*devices|?deviceName|t*type,
                :redex/user-key "t/type"},
               :system/id
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :redex/cat-key ["owners" ?ownerName "owner/systems" ?systemName],
                :redex/self :system/id,
                :redex/user-key ?systemName,
                :redex/exp-key? true},
               :redex/vals #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
               :box/boolean-val #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean},
               :redex/owners--owners
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :redex/cat-key ["owners" ?ownerName "owner/systems" ?systemName],
                :redex/self :redex/owners--owners,
                :redex/user-key "owners"},
               :redex/device*status--owners|?ownerName|owner*systems|?systemName|system*devices|?deviceName|device*status
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :redex/cat-key ["owners" ?ownerName "owner/systems" ?systemName "system/devices" ?deviceName "device/status"],
                :redex/self :redex/device*status--owners|?ownerName|owner*systems|?systemName|system*devices|?deviceName|device*status,
                :redex/user-key "device/status"},
               :redex/owner*systems--owners|?ownerName|owner*systems
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :redex/cat-key ["owners" ?ownerName "owner/systems" ?systemName "system/devices"],
                :redex/self :redex/owner*systems--owners|?ownerName|owner*systems,
                :redex/user-key "owner/systems"},
               :box/number-val #:db{:cardinality :db.cardinality/one, :valueType :db.type/number},
               :redex/val #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref},
               :redex/t*type--owners|?ownerName|owner*systems|?systemName|t*type
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :redex/cat-key ["owners" ?ownerName "owner/systems" ?systemName "t/type"],
                :redex/self :redex/t*type--owners|?ownerName|owner*systems|?systemName|t*type,
                :redex/user-key "t/type"},
               :redex/more #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
               :device/id
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :redex/cat-key ["owners" ?ownerName "owner/systems" ?systemName "system/devices" ?deviceName],
                :redex/self :device/id,
                :redex/user-key ?deviceName,
                :redex/exp-key? true},
               :redex/ek-val #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref},
               :redex/system*devices--owners|?ownerName|owner*systems|?systemName|system*devices
               {:db/unique :db.unique/identity,
                :db/valueType :db.type/string,
                :db/cardinality :db.cardinality/one,
                :redex/cat-key ["owners" ?ownerName "owner/systems" ?systemName "system/devices" ?deviceName],
                :redex/self :redex/system*devices--owners|?ownerName|owner*systems|?systemName|system*devices,
                :redex/user-key "system/devices"}},
             :reduce-body
             '[#:_rm{:owners--owners (:rm/express-key "owners"),
                     :user-key "owners",
                     :attrs
                     {:owner/id (:rm/express-key "owners" ?ownerName),
                      :redex/user-key "owner/id",
                      :redex/ek-val ?ownerName,
                      :redex/more
                      [#:_rm{:t*type--owners|?ownerName|t*type (:rm/express-key "owners" ?ownerName "t/type"), :user-key "t/type", :val "OWNER"}
                       #:_rm{:owner*systems--owners|?ownerName|owner*systems (:rm/express-key "owners" ?ownerName "owner/systems"),
                             :user-key "owner/systems",
                             :vals
                             [{:system/id (:rm/express-key "owners" ?ownerName "owner/systems" ?systemName),
                               :redex/user-key "system/id",
                               :redex/ek-val ?systemName,
                               :redex/more
                               [#:_rm{:t*type--owners|?ownerName|owner*systems|?systemName|t*type (:rm/express-key "owners" ?ownerName "owner/systems" ?systemName "t/type"), :user-key "t/type", :val "SYSTEM"}
                                #:_rm{:system*devices--owners|?ownerName|owner*systems|?systemName|system*devices (:rm/express-key "owners" ?ownerName "owner/systems" ?systemName "system/devices"),
                                      :user-key "system/devices",
                                      :vals
                                      [{:device/id (:rm/express-key "owners" ?ownerName "owner/systems" ?systemName "system/devices" ?deviceName),
                                        :redex/user-key "device/id",
                                        :redex/ek-val ?deviceName,
                                        :redex/more
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
