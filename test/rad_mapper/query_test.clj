(ns rad-mapper.query-test
  (:require
   [clojure.pprint :refer [pprint cl-format]]
   [clojure.test :refer  [deftest is testing]]
   [datahike.api           :as d]
   [datahike.pull-api      :as dp]
   [owl-db-tools.resolvers :refer [pull-resource]]
   [rad-mapper.builtins    :as bi]
   [rad-mapper.query       :as qu]
   [rad-mapper.rewrite     :as rew]))

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
;;; dolce-1.edn is both :owl/Class and :owl/ObjectProperty.
(def test-data
  "Get maps of everything in the DOLCE (dol) namespace. "
  (->> "data/testing/dolce-1.edn"
       slurp
       read-string
       (mapv #(dissoc % :rdfs/subClassOf :owl/equivalentClass))))

(def test-data-json (qu/json-like test-data))

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

    ;; The attr of a triple (middle item) can be queried and results do not include db/schema content.
    (is (= [{:ent 3, :attr :person/lname, :val "Dee"} {:ent 3, :attr :person/fname, :val "Peter"}]
           (rew/rewrite* :ptag/code-block
                         "( $data := [{'person/fname' : 'Peter', 'person/lname' : 'Dee'}];
                            $data.$query([?ent ?attr ?val]))"
                         :execute? true)))

    (is (= [#:db{:id 1, :cardinality :db.cardinality/one, :ident :person/fname, :valueType :db.type/string}
            #:db{:id 2, :cardinality :db.cardinality/one, :ident :person/lname, :valueType :db.type/string}
            {:db/id 3, :person/fname "Bob", :person/lname "Clark"}]
           (rew/rewrite* :ptag/exp "$DBfor({'person/fname' : 'Bob', 'person/lname' : 'Clark'})" :execute? true)))

    ;; ToDo: Isn't this suppose to return a vector?
    ;; (d/q '[:find ?f :keys f :where [_ :foo ?f]] (qu/db-for! [{:foo 1} {:foo 2}])) ==> [{:f 2} {:f 1}]
    (is (=  {:person 3, :fname "Bob", :lname "Clark"}
            (rew/rewrite* :ptag/code-block
                          "( $data := {'person/fname' : 'Bob', 'person/lname' : 'Clark'};
                             $data.$query([?person :person/fname ?fname]
                                          [?person :person/lname ?lname]) )"
                          :execute? true)))

    ;; I think the above isn't returning a vector because of the JSONata 'singleton thing'.
    ;; Here is an example from the spec.
    (is (= [{:person 4, :fname "Peter", :lname "Dee"} {:person 3, :fname "Bob", :lname "Clark"}]
           (rew/rewrite* :ptag/code-block
                         "( $data := [{'Person/firstname' : 'Bob'  , 'Person/lastname' : 'Clark'},
                                      {'Person/firstname' : 'Peter', 'Person/lastname' : 'Dee'}];
                            $data.$query([?person :Person/firstname ?fname]
                                         [?person :Person/lastname  ?lname]))"
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

;;;====================================================================
;; OWL example from the Draft OAGi Interoperable Mapping Specification
;;;====================================================================
;;; I'm using here the OWL DB that I also use for owl-db-tools.
;;; To make this DB see data/testing/make-data/make_data.clj.
(def db-cfg
  {:store {:backend :file :path (str (System/getenv "HOME") "/Databases/datahike-owl-db")}
   :keep-history? false
   :schema-flexibility :write})

(def conn (-> db-cfg d/connect deref))

(def owl-test-data "the simplified objects used in the Draft OAGi Interoperable Mapping Specification example"
  (reduce (fn [res obj]
            (conj res (as-> (pull-resource obj conn) ?o
                        (cond-> ?o
                          (contains? ?o :rdfs/comment)    (update :rdfs/comment (fn [c] (mapv #(str (subs % 0 40) "...") c)))
                          (contains? ?o :rdfs/domain)     (update :rdfs/domain first)
                          (contains? ?o :rdfs/range)      (update :rdfs/range first)
                          (contains? ?o :rdfs/subClassOf) (update :rdfs/subClassOf first)))))
          []
          [:dol/endurant :dol/participant :dol/participant-in]))

(defn write-pretty-file
  "Transform/filter and sort objects; write them to fname."
  [fname objs & {:keys [transform] :or {transform identity}}]
  (spit fname
        (with-out-str
          (println "[")
          (doseq [obj (->> objs transform (sort-by :resource/iri))]
            (println "\n")
            (pprint obj))
          (println "]"))))

(write-pretty-file "data/testing/owl-example.edn" owl-test-data)

(def owl-q1 "an expression used in testing below"
"$query( [?class :rdf/type            'owl/Class']
         [?class :resource/iri        ?class-iri]
         [?class :resource/namespace  ?class-ns]
         [?class :resource/name       ?class-name])")

(def owl-e1 "an expression used in testing below"
"enforce(){  {'instance-of'  : 'insert-row',
                   'table'        : 'ObjectDefinition',
                   'content'      : [{'resourceIRI'       : ?iri},
                                     {'resourceLabel'     : ?label},
                                     {'resourceNamespace' : ?ns}]}  
              }")

(def owl-full-example "The whole thing looks like this:"
"($data   := $readFile('data/testing/owl-example.edn');
 $target := [];
 $reduce($data.$query( [?class :rdf/type 'owl/Class']
                       [?class :resource/iri        ?class-iri]
                       [?class :resource/namespace  ?class-ns]
                       [?class :resource/name       ?class-name] ),
         enforce()
           {  
             {'instance-of'  : 'insert-row',
              'table'        : 'ObjectDefinition',
              'content'      : [{'resourceIRI'       : ?iri},
                                {'resourceLabel'     : ?label},
                                {'resourceNamespace' : ?ns}]}  
           },
        $target)
 )")

(deftest owl-example-parse
  (testing "Test parsing the owl example in the spec"
    (is (= '(:name :preamble :bound-vars :body)
           (-> (rew/rewrite* :ptag/code-block owl-full-example :parse? true) keys)))))

(deftest owl-example-rewrite
  (testing "Test rewriting the OWL example in the spec."
    ;; This is a test of rewriting a $query expression.
    (is (= '(bi/$query
             '[[?class :rdf/type "owl/Class"]
               [?class :resource/iri ?class-iri]
               [?class :resource/namespace ?class-ns]
               [?class :resource/name ?class-name]])
           (rew/rewrite* :ptag/fn-call owl-q1 :rewrite? true)))
    
    ;;; This is a test of rewriting an enforce.  
    (is (=
         '(->
           (bi/enforce
            []
            (-> {}
                (assoc "instance-of" "insert-row")
                (assoc "table" "ObjectDefinition")
                (assoc "content" [(-> {} (assoc "resourceIRI" ?iri))
                                  (-> {} (assoc "resourceLabel" ?label))
                                  (-> {} (assoc "resourceNamespace" ?ns))])))
           (with-meta
             {:params '[],
              :enforce? true,
              :body
              '(-> {}
                   (assoc "instance-of" "insert-row")
                   (assoc "table" "ObjectDefinition")
                   (assoc "content" [(-> {} (assoc "resourceIRI" ?iri))
                                     (-> {} (assoc "resourceLabel" ?label))
                                     (-> {} (assoc "resourceNamespace" ?ns))]))}))
         (rew/rewrite* :ptag/exp owl-e1 :rewrite? true )))))


(deftest owl-example-executes
  (testing "Testing execution of $query and enforce."
    
    ;; This is a test of executing the above $query expression against data from the example in the spec.
    (is (= {:class 12, :class-iri "dol/endurant", :class-ns "dol", :class-name "endurant"}
           (rew/rewrite*
            (with-out-str
              (cl-format *out* "($data := $readFile('data/testing/owl-example.edn');~% $data.~A)"
                         owl-q1))
            :ptag/code-block
            :execute? true)))
    
    ;; This is a test of execute $map($query,enforce) against data from the example in the spec.
    (is (= :todo
           (rew/rewrite*
            :ptag/code-block
            (with-out-str
              (cl-format *out* "($data := $readFile('data/testing/owl-example.edn');~% $map($data.~A,~%      ~A)" 
                         owl-q1 owl-e1))
            :ptag/code-block
            :execute? true)))))

(deftest use-of-owl-db-tools-query
  (testing "owl-db-tools is USED in development. This is here mostly to ensure it has needed functionality."
    (is (=
         {:resource/iri :dol/perdurant,
          :resource/name "perdurant",
          :resource/namespace "dol",
          :owl/disjointWith [:dol/endurant :dol/abstract :dol/quality],
          :rdf/type :owl/Class,
          :rdfs/subClassOf
          [:dol/spatio-temporal-particular
           {:owl/onProperty :dol/has-quality, :owl/allValuesFrom [:dol/temporal-quality], :rdf/type :owl/Restriction}
           {:owl/onProperty :dol/has-quality, :owl/someValuesFrom [:dol/temporal-location_q], :rdf/type :owl/Restriction}
           {:owl/onProperty :dol/part, :owl/allValuesFrom [:dol/perdurant], :rdf/type :owl/Restriction}
           {:owl/onProperty :dol/participant, :owl/someValuesFrom [:dol/endurant], :rdf/type :owl/Restriction}
           {:owl/onProperty :dol/specific-constant-constituent, :owl/allValuesFrom [:dol/perdurant], :rdf/type :owl/Restriction}]}
         ;; Returns a sorted-map, thus str and read-string.
         (-> (pull-resource :dol/perdurant conn) (dissoc :rdfs/comment) str read-string)))))
