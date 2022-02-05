(ns pdenno.rad-mapper.query-test
  (:require
   [clojure.test :refer  [deftest is testing]]
   [datahike.api                  :as d]
   [pdenno.rad-mapper.builtins    :as bi]
   [pdenno.rad-mapper.evaluate    :as ev]
   [pdenno.rad-mapper.query       :as qu]
   [pdenno.rad-mapper.rewrite     :as rew]))

;;;====================================================================================== NYI
(def test-schema
    [{:schema  {:db/attrs   [{:schema/name    {:db/type  :db/string, :db/cardinality  :one}}],
                :db/key     [:schema/name]}}

     {:table   {:db/attrs   [{:table/name     {:db/type :db/string, :db/cardinality :one },
                              :table/schema   {:db/type :db/object, :db/cardinality :one, :db/in-line? true},
                              :table/columns  {:db/type :db/object, :db/cardinality :many}}], ; Just to make it interesting.
                :db/key     [:table/schema, :table/name]}}

     {:column  {:db/attrs   [{:column/name  {:db/type  :db/string, :db/cardinality  :one}},
                             {:column/type  {:db/type  :db/string, :db/cardinality  :one}},
                             {:column/table {:db/type  :db/object, :db/cardinality  :one}}],
                :db/key     [:column/table, :column/name]}}])


;;; ToDo: Dissoc is temporary; boxing.
(def test-data
  "Get maps of everything in the DOLCE (dol) namespace. "
  (->> "data/testing/dolce-1.edn"
       slurp
       read-string
       (mapv #(dissoc % :rdfs/subClassOf :owl/equivalentClass))))

(def test-data-json (bi/key2str test-data))

(deftest db-for-tests-1
  (testing "Testing that basic db-for! (and its schema-making) work"
    (let [conn (qu/db-for! test-data)
          binding-set (d/q '[:find ?class ?class-iri ?class-ns ?class-name ?rel ?rel-name ?rel-range
                             :keys class class-iri class-ns class-name rel rel-name rel-range
                             :where
                             [?class :rdf/type            :owl/Class]
                             [?class :resource/iri        ?class-iri]
                             [?class :resource/namespace  ?class-ns]
                             [?class :resource/name       ?class-name]
                             [?rel   :rdfs/domain         ?class-iri]
                             [?rel   :rdf/type            :owl/ObjectProperty]
                             [?rel   :rdfs/range          ?rel-range]
                             [?rel   :resource/name       ?rel-name]]
                           conn)]
      (is (== 70 (count binding-set)))
      (is (== 33 (->> binding-set (filter #(= :dol/particular (:class-iri %))) count))))))

(deftest db-for-tests-2
  (testing "Testing that basic db-for! (and its schema-making) work"
    (let [conn (qu/db-for! test-data)
          binding-set (d/q '[:find ?class ?class-iri
                             :keys class class-iri
                             :where
                             [?class :rdf/type            :owl/Class]
                             [?class :resource/iri        ?class-iri]]
                           conn)]
      (is (= #{:dol/endurant :dol/spatio-temporal-region :dol/abstract-region :dol/physical-region :dol/non-physical-endurant
               :dol/region :dol/quality :dol/physical-quality :dol/quale :dol/particular :dol/physical-endurant :dol/perdurant
               :dol/feature :dol/time-interval}
             (->> binding-set (map :class-iri) set))))))

(deftest query-basics
  (testing "Testing $query parsing, rewriting and execution."
    (is (= '(bi/$query '[[?class :rdf/type "owl/Class"] [?class :resource/iri ?class-iri]])
           (rew/rewrite* :ptag/exp
            "$query( [?class :rdf/type     'owl/Class']
                     [?class :resource/iri  ?class-iri])"
            :rewrite? true)))
    (is (= '(let [$data (bi/$readFile "data/testing/dolce-2.edn")]
              (bi/access $data (bi/$query '[[?class :rdf/type "owl/Class"]
                                            [?class :resource/iri ?class-iri]])))
           (rew/rewrite* :ptag/code-block
            "( $data := $readFile('data/testing/dolce-2.edn');
               $data.$query([?class :rdf/type     'owl/Class']
                            [?class :resource/iri ?class-iri]) )"
            :rewrite? true)))
    (is (= [#:db{:id 1, :cardinality :db.cardinality/one, :ident :person/fname, :valueType :db.type/string}
            #:db{:id 2, :cardinality :db.cardinality/one, :ident :person/lname, :valueType :db.type/string}
            {:db/id 3, :person/fname "Bob", :person/lname "Clark"}]
           (rew/rewrite* :ptag/exp "$DBfor({'person/fname' : 'Bob', 'person/lname' : 'Clark'})" :execute? true)))
    (is (=  {:fname "Bob", :lname "Clark"}
            (rew/rewrite* :ptag/code-block
                          "( $data := {'person/fname' : 'Bob', 'person/lname' : 'Clark'};
                             $data.$query([?person :person/fname ?fname]
                                          [?person :person/lname ?lname]) )"
                           :execute? true)))
    (is (= #{:dol/endurant :dol/spatio-temporal-region :dol/abstract-region :dol/physical-region :dol/non-physical-endurant
             :dol/region :dol/quality :dol/physical-quality :dol/quale :dol/particular :dol/physical-endurant :dol/perdurant
             :dol/feature :dol/time-interval}
           (->> ((bi/$query '[[?class :rdf/type "owl/Class"] [?class :resource/iri  ?class-iri]]) test-data-json)
                (map :class-iri)
                (map keyword)
                set)))
    ;; This one is the same as db-for-tests-2 but mostly in RADmapper language.
    (is (= #{:dol/endurant :dol/spatio-temporal-region :dol/abstract-region :dol/physical-region :dol/non-physical-endurant
             :dol/region :dol/quality :dol/physical-quality :dol/quale :dol/particular :dol/physical-endurant :dol/perdurant
             :dol/feature :dol/time-interval}
           (->> (rew/rewrite* :ptag/code-block ; ToDo: This can use dolce-1.edn once heterogeneous data is handled.
                              "( $data := $readFile('data/testing/dolce-2.edn');
                                 $data.$query([?class :rdf/type     'owl/Class']
                                              [?class :resource/iri  ?class-iri]) )"
                              :execute? true)
                (map :class-iri)
                (map keyword)
                set)))))

(def q1
"$query( [?class rdf/type            'owl/Class']
         [?class resource/iri        ?class-iri]
         [?class resource/namespace  ?class-ns]
         [?class resource/name       ?class-name]
         [?rel   rdf/type            'owl/ObjectProperty']
         [?rel   rdfs/domain         ?class-iri]
         [?rel   rdfs/range          ?rel-range]
         [?rel   resource/name       ?rel-name] )")

(def t1
"$transform(
   $query( [?class rdf/type            'owl/Class']
           [?class resource/iri        ?class-iri]
           [?class resource/namespace  ?class-ns]
           [?class resource/name       ?class-name]
           [?rel   rdf/type            'owl/ObjectProperty']
           [?rel   rdfs/domain         ?class-iri]
           [?rel   rdfs/range          ?rel-range]
           [?rel   resource/name       ?rel-name] )
   $enforce( {'table/name'     ?class-name,
              'table/schema'   {'schema/name'  ?ns},
              'table/columns'  {'column/name'  ?rel-name,
                               'column/type'  ?rel-range,
                               'column/table' ?table-ent}} :as ?table-ent))")

(deftest enforce-basics
  (testing "Testing that $enforce works"
    (is true) #_(= :NYI  (rew/rewrite* :ptag/code-block))
#_#_#_"($schema =
   [{'schema' : {'db/attrs'  : [{'schema/name'   : {'db/type' : 'string', 'db/cardinality' : 'one'}}],
                 'db/key'    : ['schema/name']}}

    {'table'  : {'db/attrs'  : [{'table/name'    : {'db/type' : 'string', 'db/cardinality' : 'one' },
                                 'table/schema'  : {'db/type' : 'object', 'db/cardinality' : 'one',
                                                    'db/in-line?' : true},
                                 'table/columns' : {'db/type' : 'object', 'db/cardinality' : 'many'}}],
                 'db/key'    : ['table/schema', 'table/name']}}

    {'column' : {'db/attrs'  : [{'column/name'   : {'db/type' : 'string', 'db/cardinality' : 'one'}},
                                {'column/type'   : {'db/type' : 'string', 'db/cardinality' : 'one'}},
                                {'column/table'  : {'db/type' : 'object', 'db/cardinality' : 'one'}}],
                 'db/key'    : ['column/table', 'column/name']}}]
 )" :execute? true))
