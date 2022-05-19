(ns rad-mapper.query-test
  (:require
   [clojure.pprint         :refer [pprint]]
   [clojure.test           :refer  [deftest is testing]]
   [datahike.api           :as d]
   [datahike.pull-api      :as dp]
   [owl-db-tools.resolvers :refer [pull-resource]]
   [rad-mapper.builtins    :as bi]
   [rad-mapper.query       :as qu]
   [rad-mapper.rewrite     :as rew]))

;;; ToDo: I think I intended to test whether some data (from enforce?) results in this?
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
(def dolce-test-data
  "Get maps of everything in the DOLCE (dol) namespace. "
  (->> "data/testing/dolce-1.edn"
       slurp
       read-string
       (mapv #(dissoc % :rdfs/subClassOf :owl/equivalentClass))))

(deftest db-for-tests-1
  (testing "Testing that basic db-for! (and its schema-making) work"
    (let [conn (qu/db-for! dolce-test-data)
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
    (let [conn (qu/db-for! dolce-test-data)
          binding-set (d/q '[:find ?class ?class-iri
                             :keys class class-iri
                             :where
                             [?class :rdf/type            :owl/Class]
                             [?class :resource/iri        ?class-iri]]
                           conn)]
      (is (= #{:dol/endurant :dol/spatio-temporal-region :dol/abstract-region :dol/physical-region :dol/non-physical-endurant
               :dol/region :dol/quality :dol/physical-quality :dol/quale :dol/particular :dol/physical-endurant :dol/perdurant
               :dol/feature :dol/time-interval}
             (->> binding-set (map :class-iri) set))))

    ;; This test the content of a complete DB, including the schema learned.
    (is (= [#:db{:id 1, :cardinality :db.cardinality/one, :ident :person/fname, :valueType :db.type/string}
            #:db{:id 2, :cardinality :db.cardinality/one, :ident :person/lname, :valueType :db.type/string}
            {:db/id 3, :person/fname "Bob", :person/lname "Clark"}]
           (let [conn (qu/db-for! {:person/fname "Bob" :person/lname "Clark"})]
             (dp/pull-many conn '[*] (range 1 (-> conn :max-eid inc))))))))

;;;================================ testing query ==================================
(deftest query-basics
  (testing "Testing query parsing, rewriting and execution."
    (is (= '(bi/query '[] '[[?class :rdf/type "owl/Class"] [?class :resource/iri ?class-iri]])
           (rew/rewrite* :ptag/exp
            "query(){[?class :rdf/type     'owl/Class']
                     [?class :resource/iri  ?class-iri]}"
            :rewrite? true)))

    ;; The attr of a triple (middle item) can be queried and results do not include db/schema content.
    (is (= [{:?ent 3, :?attr :person/lname, :?val "Dee"} {:?ent 3, :?attr :person/fname, :?val "Peter"}]
           (rew/rewrite* :ptag/code-block
                         "( $data := [{'person/fname' : 'Peter', 'person/lname' : 'Dee'}];
                            $q := query(){[?ent ?attr ?val]};
                            $q($data))"
                         :execute? true)))

    ;; This tests rewriting of an in-line query.
    (is (= '((bi/query '[] '[[?ent ?attr ?val]]) [(-> {} (assoc "person/fname" "Peter") (assoc "person/lname" "Dee"))])
           (rew/rewrite* :ptag/exp
                         "query(){[?ent ?attr ?val]}([{'person/fname' : 'Peter', 'person/lname' : 'Dee'}])"
                         :rewrite? true)))

    ;; This tests rewriting of an in-line query with a parameter.
    (is (= '(let [$qBob ((bi/query '[$name] '[[?e :name $name]]) "Bob")] ($qBob [(-> {} (assoc "name" "Bob"))]))
           (rew/rewrite* :ptag/code-block
                         "($qBob := query($name){[?e :name $name]}('Bob');
                           $qBob([{'name' : 'Bob'}]))"
                         :rewrite? true)))

    ;; This tests execution of an in-line query.
    (is (= [{:?ent 3, :?attr :person/lname, :?val "Dee"} {:?ent 3, :?attr :person/fname, :?val "Peter"}]
           (rew/rewrite* :ptag/exp
                         "query(){[?ent ?attr ?val]}([{'person/fname' : 'Peter', 'person/lname' : 'Dee'}])"
                         :execute? true)))

    ;; This tests execution of an in-line query with a parameter.
    (is (= [{:?e 2}]
           (rew/rewrite* :ptag/code-block
                         "($qBob := query($name){[?e :name $name]}('Bob');
                           $qBob([{'name' : 'Bob'}]))"
                         :execute? true)))

    ;; This tests rewriting a query using a mapping context with the default name 'source-data-1'.
    (is (= '(let [$mc (bi/thread (bi/$MCnewContext)
                                 (bi/$MCaddSource [(-> {} (assoc "person/fname" "Bob")
                                                       (assoc "person/lname" "Clark"))]))
                  $q (bi/query '[] '[[?person :person/fname ?fname] [?person :person/lname ?lname]])]
              ($q (bi/$MCgetSource "source-data-1")))
           (rew/rewrite*
            :ptag/code-block
            "( $mc := $MCnewContext() ~> $MCaddSource([{'person/fname' : 'Bob', 'person/lname' : 'Clark'}]);
                $q := query(){[?person :person/fname ?fname]
                              [?person :person/lname ?lname]};
                $q($MCgetSource('source-data-1')) )"
             :rewrite? true)))

    ;; This tests execution of a query using a mapping context with the default name 'source-data-1'.
    (is (=  [{:?person 3, :?fname "Bob", :?lname "Clark"}]
            (rew/rewrite*
             :ptag/code-block
             "( $mc := $MCnewContext() ~> $MCaddSource([{'person/fname' : 'Bob', 'person/lname' : 'Clark'}]);
                $q := query(){[?person :person/fname ?fname]
                              [?person :person/lname ?lname]};
                $q($MCgetSource($mc, 'source-data-1')) )"
             :execute? true)))

    ;; Here is an example from the spec.
    ;; This tests execution of a query using a mapping context with the default name 'source-data-1'.
    (is (= [{:?person 4, :?fname "Peter", :?lname "Dee"}
            {:?person 3, :?fname "Bob",   :?lname "Clark"}]
           (rew/rewrite* :ptag/code-block
                         "( $data := [{'Person/firstname' : 'Bob'  , 'Person/lastname' : 'Clark'},
                                      {'Person/firstname' : 'Peter', 'Person/lastname' : 'Dee'}];
                            $q := query(){[?person :Person/firstname ?fname]
                                          [?person :Person/lastname  ?lname]};
                            $q($data) )"
                         :execute? true)))

    ;; This tests query on data (rather than directly on a DB).
    (is (= #{:dol/endurant :dol/spatio-temporal-region :dol/abstract-region :dol/physical-region :dol/non-physical-endurant
             :dol/region :dol/quality :dol/physical-quality :dol/quale :dol/particular :dol/physical-endurant :dol/perdurant
             :dol/feature :dol/time-interval}
           (->> ((bi/query [] '[[?class :rdf/type :owl/Class] [?class :resource/iri  ?class-iri]]) dolce-test-data)
                (map :?class-iri)
                (map keyword)
                set)))

    ;; This one is the same as db-for-tests-2 but mostly in RADmapper language.
    (is (= #{:dol/endurant :dol/spatio-temporal-region :dol/abstract-region :dol/physical-region :dol/non-physical-endurant
             :dol/region :dol/quality :dol/physical-quality :dol/quale :dol/particular :dol/physical-endurant :dol/perdurant
             :dol/feature :dol/time-interval}
           (->> (rew/rewrite* :ptag/code-block ; ToDo: This can use dolce-1.edn once heterogeneous data is handled.
                              "( $mc := $MCnewContext() ~> $MCaddSource($readFile('data/testing/dolce-2.edn'));
                                 $q := query(){[?class :rdf/type     'owl/Class']
                                               [?class :resource/iri  ?class-iri]};
                                 $q($MCgetSource($mc, 'source-data-1')) )"
                              :execute? true)
                (map :?class-iri)
                (map keyword)
                set)))))

;;;================================ testing enforce ==================================
(def e0 "enforce()
            {  {'instance-of'  : 'example',
                'content'      : ?class-iri }
            }")

(def e1 "enforce($type)
            {  {'instance-of'  : $type,
                'content'      : ?class-iri }
            }")

(deftest enforce-basics
  (testing "Testing that enforce basically works."

    ;; This tests rewriting; you don't see what goes on (creating the fn, setting meta-data).
    (is (= '(bi/enforce
             {:params [],
              :body (-> {}
                        (assoc "instance-of" "example")
                        (assoc "content" (bi/get-from-b-set b-set :?class-iri)))})
           (rew/rewrite* :ptag/exp e0 :rewrite? true)))

    ;; This creates the function. :params are empty, so this is the "immediate" type.
    (let [qfn (bi/enforce {:params [],
                           :body (-> {}
                                     (assoc "instance-of" "FixedType")
                                     (assoc "content" (bi/get-from-b-set b-set :?class-iri)))})]
      
      ;; The function has metadata.
      (is (= '{:params [b-set], :enforce? true} (meta qfn)))
      
      ;; The function is executable with a binding set, creating content. 
      (is (= {"instance-of" "FixedType", "content" "IRI"}
             (qfn {:?class-iri "IRI"}))))

    ;;----- Rewrite for the parametric ("higher-order") is similar; :params and closed over $type differ.
    (is (= '(bi/enforce {:params [$type],
                         :body (-> {}
                                   (assoc "instance-of" $type)
                                   (assoc "content" (bi/get-from-b-set b-set :?class-iri)))})
           (rew/rewrite* :ptag/exp e1 :rewrite? true)))

    ;; Now however, the function returned is higher-order...
    (let [ho-qfn (bi/enforce {:params [$type],
                              :body (-> {}
                                        (assoc "instance-of" $type)
                                        (assoc "content" (bi/get-from-b-set b-set :?class-iri)))})
          ;; ... So we get a query function for it with $type = "MyType"
          qfn (ho-qfn "MyType")]

      ;; The function has the same metadata.
      (is (= '{:params [b-set], :enforce? true} (meta qfn)))

      ;; Rather than 'FixedType', the instance-of is 'MyType'.
      (is (= {"instance-of" "MyType", "content" "IRI"}
             (qfn {:?class-iri "IRI"}))))))

;;; ToDo: This might be a good example for the spec.
(defn enforce-reduce-demo
  "This demonstrates rewriting and basics operation; some _stuff below is not used."
  []
  (let [_b-sets   [{:?ent 1 :?word-1 "Hello" :?word-2 "world!"}
                   {:?ent 2 :?word-1 "Nice"  :?word-2 "day!"}]
        ;; Same thing produced by running a query with in-line data.
        b-sets (rew/rewrite* :ptag/exp
                             "query(){[?ent :greeting/word-1 ?word-1] [?ent :greeting/word-2 ?word-2]}
                                       ([{'greeting/word-1' : 'Hello', 'greeting/word-2' : 'world!'}
                                         {'greeting/word-1' : 'Nice',  'greeting/word-2' : 'day!'}])"
                             :execute? true)
        ;; Body will be used by bi/enforce to define a function and associate metadata with it.
        _e-fn :nyi #_(bi/enforce {:params [],
                           :body (-> {}
                                     (assoc "target/word-1" (bi/get-from-b-set b-set :?word-1))
                                     (assoc "target/word-2" (bi/get-from-b-set b-set :?word-2)))})
        ;; Same thing
        e-fn (rew/rewrite* :ptag/exp
                           "enforce (){ {'target/word-1' : ?word-1, 'target/word-2' : ?word-2} }"
                           :execute? true)]
        ;; Run it!
    (bi/$reduce b-sets e-fn [])))

(deftest the-enforce-reduce-demo
  (testing "Testing the basic use of enforce in $reduce."
    (is (= [#:target{:word-1 "Nice", :word-2 "day!"} #:target{:word-1 "Hello", :word-2 "world!"}]
           (enforce-reduce-demo)))))

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

(def owl-full-immediate "The whole thing looks like this:"
"
  (   $mc := $MCnewContext() ~> $MCaddSource($readFile('data/testing/owl-example.edn'),'owl-source');

      $qClass := query()
                   { [?class :rdf/type            'owl/Class']
                     [?class :resource/iri        ?class-iri]
                     [?class :resource/namespace  ?class-ns]
                     [?class :resource/name       ?class-name]
                   };  // Defines a higher-order function

      $bsets := $qClass($MCgetSource($mc, 'owl-source'));
      $reduce($bsets,
              enforce()
                 {  {'instance-of'  : 'insert-row',
                     'table'        : 'ObjectDefinition',
                     'content'      : {'resourceIRI'       : ?class-iri,
                                       'resourceNamespace' : ?class-ns,
                                       'resourceLabel'     : ?class-name}}
                 },
              [] // In updating the target, there would be data here.
              )
   )")

(def owl-full-parametric
"
  (   $mc := $MCnewContext() ~> $MCaddSource($readFile('data/testing/owl-example.edn'), 'owl-source');

      $qtype  := query($type)
                   { [?class :rdf/type            $type]
                     [?class :resource/iri        ?class-iri]
                     [?class :resource/namespace  ?class-ns]
                     [?class :resource/name       ?class-name]
                   };  // Defines a higher-order function

      $qClass := $qtype('owl/Class');
      $qProp  := $qtype('owl/ObjectProperty');

      $bsets := $qClass($MCgetSource($mc, 'owl-source')); // Run query; return a collection of binding sets.
                                                          // Could use ~> here; instead, I'm passing $bsets.
      $reduce($bsets,
              enforce()
                 {  {'instance-of'  : 'insert-row',
                     'table'        : 'ObjectDefinition',
                     'content'      : {'resourceIRI'       : ?class-iri,
                                       'resourceNamespace' : ?class-ns,
                                       'resourceLabel'     : ?class-name}}
                 },
              [] // In updating the target, there would be data here.
              )
   )")

(def owl-immediate-e1
  "enforce()
     {  {'instance-of'  : 'insert-row',
         'table'        : 'ObjectDefinition',
         'content'      : {'resourceIRI'       : ?class-iri,
                           'resourceNamespace' : ?class-ns,
                           'resourceLabel'     : ?class-name}}
      }")

(deftest owl-example-rewrite
  (testing "Test rewriting the OWL example in the spec."

    ;; This is a test of rewriting a query expression. Note quotes; bi/query isn't a macro.
    (is (= '(bi/query '[$type] '[[?class :rdf/type $type] [?class :resource/iri ?class-iri]])
           (rew/rewrite* :ptag/exp "query($type){[?class :rdf/type $type]
                                                   [?class :resource/iri ?class-iri]}"
                         :rewrite? true)))

    ;; This is a test of rewriting an enforce.
    (is (= '(bi/enforce
             {:params [],
              :body (->  {}
                         (assoc "instance-of" "insert-row")
                         (assoc "table" "ObjectDefinition")
                         (assoc "content" (-> {}
                                              (assoc "resourceIRI" (bi/get-from-b-set b-set :?class-iri))
                                              (assoc "resourceNamespace" (bi/get-from-b-set b-set :?class-ns))
                                              (assoc "resourceLabel" (bi/get-from-b-set b-set :?class-name)))))})
           (rew/rewrite* :ptag/enforce-def owl-immediate-e1 :rewrite? true)))

    ;; This is an example of rewriting the whole example.
    (is (= '(let [$mc (bi/thread (bi/$MCnewContext) (bi/$MCaddSource (bi/$readFile "data/testing/owl-example.edn") "owl-source"))
                  $qClass (bi/query '[] '[[?class :rdf/type "owl/Class"]
                                          [?class :resource/iri ?class-iri]
                                          [?class :resource/namespace ?class-ns]
                                          [?class :resource/name ?class-name]])
                  $bsets ($qClass (bi/$MCgetSource $mc "owl-source"))]
              (bi/$reduce $bsets
                          (bi/enforce
                           {:params [],
                            :body
                            (->
                             {}
                             (assoc "instance-of" "insert-row")
                             (assoc "table" "ObjectDefinition")
                             (assoc "content" (-> {}
                                                  (assoc "resourceIRI" (bi/get-from-b-set b-set :?class-iri))
                                                  (assoc "resourceNamespace" (bi/get-from-b-set b-set :?class-ns))
                                                  (assoc "resourceLabel" (bi/get-from-b-set b-set :?class-name)))))})
                          []))
           (rew/rewrite* :ptag/code-block owl-full-immediate :rewrite? true)))))

(def owl-query-immediate
"
  (   $mc := $MCnewContext() ~> $MCaddSource($readFile('data/testing/owl-example.edn'), 'owl-source');

      $qClass := query()
                   { [?class :rdf/type            'owl/Class']
                     [?class :resource/iri        ?class-iri]
                     [?class :resource/namespace  ?class-ns]
                     [?class :resource/name       ?class-name]
                   };  // Defines a higher-order function

      $qClass($MCgetSource($mc, 'owl-source'))  // Run query; return a collection of binding sets.
  )")

(def owl-query-parametric
"
  (   $mc := $MCnewContext() ~> $MCaddSource($readFile('data/testing/owl-example.edn'), 'owl-source');

      $qtype  := query($type)
                   { [?class :rdf/type            'owl/Class']
                     [?class :resource/iri        ?class-iri]
                     [?class :resource/namespace  ?class-ns]
                     [?class :resource/name       ?class-name]
                   };  // Defines a function that returns a higher-order function.

      $qClass := $qtype('owl/Class'); // Make a query function by specifying parameter values.

      $qClass($MCgetSource($mc, 'owl-source'))  // Run query; return a collection of binding sets.
  )")

(deftest owl-example-executes
  (testing "Testing execution of $query and enforce."

    ;; bi/query is a higher-order function that returns either a query function, or a higher-order function
    ;; that takes a parameter to customize a 'parametric' query function.
    (let [data (-> (bi/$MCnewContext) (bi/$MCaddSource [{:name "Bob"}]))]
      (is (= [{:?e 2}] ((bi/query  []        '[[?e :name "Bob"]]) (bi/$MCgetSource data "source-data-1"))))
      (is (= [{:?e 2}] (((bi/query '[$name]  '[[?e :name $name]]) "Bob") (bi/$MCgetSource data "source-data-1"))))
      (is (= []       (((bi/query '[$name]  '[[?e :name $name]]) "xxx") (bi/$MCgetSource data "source-data-1")))))

    ;; This tests making a new data source and use of a special.
    ;; I just check the type because the actual DB can't be tested for equality.
    (is (= datahike.db.DB
           (do (rew/rewrite* :ptag/code-block
                             "($ := $MCnewContext() ~> $MCaddSource($readFile('data/testing/owl-example.edn'), 'owl-data');)"
                             :execute? true)
               (-> @bi/$ :sources (get "owl-data") type))))

    ;; This one executes an immediate query.
    (is (= [{:?class 12, :?class-iri "dol/endurant", :?class-ns "dol", :?class-name "endurant"}]
           (rew/rewrite* :ptag/code-block owl-query-immediate :execute? true)))

    ;; This one executes a parametric query; same results as above.
    (is (= [{:?class 12, :?class-iri "dol/endurant", :?class-ns "dol", :?class-name "endurant"}]
           (rew/rewrite* :ptag/code-block owl-query-parametric :execute? true)))

    ;; This executes query + enforce where an immediate query is being used.
    (is (= [{:instance-of "insert-row",
             :table "ObjectDefinition",
             :content {:resourceIRI "dol/endurant"
                       :resourceNamespace "dol"
                       :resourceLabel "endurant"}}]
           (rew/rewrite* :ptag/code-block owl-full-immediate :execute? true)))

    ;; This executes query + enforce where a parametric query is being used.
    (is (=  [{:instance-of "insert-row",
              :table "ObjectDefinition",
              :content {:resourceIRI "dol/endurant"
                        :resourceNamespace "dol"
                        :resourceLabel "endurant"}}]
            (rew/rewrite* :ptag/code-block owl-full-parametric :execute? true)))))

;;;================================================================================================================
;;; "owl-db-tools is used in only development (See deps.edn.)  It is here mostly to ensure it has needed functionality."
(deftest use-of-owl-db-tools-query
  (testing "owl-db-tools/pull-resource"
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

(def owl-full-enforce-extra
  "In this one, which is what I think should be in the spec, I'm not bothering with MCs."
"
( $data := $readFile('data/testing/owl-example.edn');

  $qtype  := query($rdfType, $extraTrips)
               { [?class :rdf/type            $rdfType]
                 [?class :resource/iri        ?class-iri]
                 [?class :resource/namespace  ?class-ns]
                 [?class :resource/name       ?class-name]
                 // ToDo: $extraTrips
               };  // Defines a higher-order function, a template of sorts.

  $etype  := enforce($tableType)
              {  {'instance-of'  : 'insert-row',
                  'table'        : $tableType,
                  'content'      : {'resourceIRI'       : ?class-iri,
                                    'resourceNamespace' : ?class-ns,
                                    'resourceLabel'     : ?class-name}}
                           }; // Likewise, for an enforce template.
                              // The target tables for objects and relations a very similar.

  $quClass := $qtype('owl/Class');     // Use the template, here and the next three assignments.

  // This one doesn't just specify a value for $rdfType, but for $extraTrips.
  $quProp       := $qtype('owl/ObjectProperty'); // ToDo: ,queryTriples{[?class :rdfs/domain ?domain] [?class :rdfs/range ?range]});
  $enClassTable := $etype('ClassDefinition');
  $enPropTable  := $etype('PropertyDefinition');

  // Run the class query; return a collection of binding sets about classes.
  $clasBsets := $quClass($data);

  // We start enforcing with no data, thus the third argument is [].
  $tar_data := $reduce($clasBsets, $enClassTable, []);

  // Get bindings sets for the ObjectProperties and make similar tables.
  $propBsets := $quProp($data);

  // We pass in the target data created so far.
  $reduce($propBsets, $enPropTable, $tar_data) // The code block returns the target data.
)")

(deftest maybe-spec-example
  (testing "Testing the above long example"
    (is (= '(let [$data (bi/$readFile "data/testing/owl-example.edn")
                  $qtype (bi/query '[$rdfType $extraTrips]
                                   '[[?class :rdf/type $rdfType]
                                     [?class :resource/iri ?class-iri]
                                     [?class :resource/namespace ?class-ns]
                                     [?class :resource/name ?class-name]])
                  $etype (bi/enforce {:params [$tableType],
                                      :body
                                      (->
                                       {}
                                       (assoc "instance-of" "insert-row")
                                       (assoc "table" (bi/get-jvar jvars $tableType))
                                       (assoc "content" (-> {}
                                                            (assoc "resourceIRI" (bi/get-from-b-set b-set :?class-iri))
                                                            (assoc "resourceNamespace" (bi/get-from-b-set b-set :?class-ns))
                                                            (assoc "resourceLabel" (bi/get-from-b-set b-set :?class-name)))))})
                  $quClass ($qtype "owl/Class")
                  $quProp ($qtype "owl/ObjectProperty")
                  $enClassTable ($etype "ClassDefinition")
                  $enPropTable ($etype "PropertyDefinition")
                  $clasBsets ($quClass $data)
                  $tar_data (bi/$reduce $clasBsets $enClassTable [])
                  $propBsets ($quProp $data)]
              (bi/$reduce $propBsets $enPropTable $tar_data)))
        (rew/rewrite* :ptag/code-block owl-full-enforce-extra :rewrite? true))))

(defn tryme []
  (let [$data (bi/$readFile "data/testing/owl-example.edn")
        $qtype (bi/query '[$rdfType $extraTrips]
                         '[[?class :rdf/type $rdfType]
                           [?class :resource/iri ?class-iri]
                           [?class :resource/namespace ?class-ns]
                           [?class :resource/name ?class-name]])
        $etype (bi/enforce {:params [$tableType],
                            :body (-> {}
                                      (assoc "instance-of" "insert-row")
                                      (assoc "table" $tableType)
                                      (assoc "content" (-> {}
                                                           (assoc "resourceIRI" (bi/get-from-b-set b-set :?class-iri))
                                                           (assoc "resourceNamespace" (bi/get-from-b-set b-set :?class-ns))
                                                           (assoc "resourceLabel" (bi/get-from-b-set b-set :?class-name)))))})
        $quClass ($qtype "owl/Class")
        $quProp ($qtype "owl/ObjectProperty")
        $enClassTable ($etype "ClassDefinition")
        $enPropTable ($etype "PropertyDefinition")
        $clasBsets ($quClass $data)
        $tar_data (bi/$reduce $clasBsets $enClassTable [])
        $propBsets ($quProp $data)]
    (bi/$reduce $propBsets $enPropTable $tar_data)))

;;;====================== Temporary, work on AST idea ===================================
(def diag (atom nil))
(declare rw-ast)

(def type2name
  {:JaConditionalExp "Conditional"
   :JaField          "FieldAccess"
   :JaFnCall         "FnCall"
   :JaBinOpExp       "BinaryExp"
   :JaFnDef          "FnDef"
   :JaJvar           "VarDef"
   :JaMapPair        "KVpair"

   :ArrayConstruction    "Array"
   :ObjectConstruction   "Object"
   :JaParenDelimitedExp  "ELIMINATED?"})

(def needs-analysis? #{:JaSquareDelimitedExp :JaCurlyDelimitedExp :JaParenDelimitedExp})

(defn actual-type
  "The objects for which needs-analysis? is true are rewritten to see what
  is actually being done."
  [m]
  (case (:_type m)
    :JaSquareDelimitedExp (if (:operand m)
                            (assoc m :_type :FilterExp)
                            (-> m
                                (assoc :_type :ArrayConstruction)
                                (assoc :elem (:exp m))
                                (dissoc :exp)))
    :JaParenDelimitedExp  (if (:operand m)
                            (assoc m :_type :MapExp)
                            (:exp m)) ; It is a "primary"
    :JaCurlyDelimitedExp  (-> m
                              (assoc :_type :ObjectConstruction)
                              (assoc :kv-pair (:exp m))
                              (dissoc :exp))))

(def char2op
  {\. :dot-map
   \& :str-concat
   \> :>
   \< :<
   \= :equality
   \+ :plus
   \- :minus
   \* :times
   \/ :divide})

(defn lookup-op [c]
  (if (contains? char2op c)
    (char2op c)
    (keyword (str "unknown-op" c))))

(defn rw-ast
  "Rewrite the AST (or more accurately concrete syntax tree) to
    (1) box primitives and regular expressions,
    (2) determine the actual type of xDelimtedExps, and
    (3) rename keys."
  [o]
  (cond (string? o)                           {:table/type :BoxedStr :BoxedStr/val o},
        (number? o)                           {:table/type :BoxedNum :BoxedNum/val o},
        (instance? java.util.regex.Pattern o) {:table/type :RegExp   :RegExp/val (str o)},
        (char?   o)                           (lookup-op o),
        (vector? o) (mapv rw-ast o),
        (map?    o) (as-> o ?o
                        (if (needs-analysis? (:_type ?o)) (actual-type ?o) ?o)
                        (if-let [obj-type (-> ?o :_type type2name)]
                          (reduce-kv (fn [m k v]
                                       (if (= k :_type)
                                         (assoc m :table/type (keyword obj-type))
                                         (assoc m (keyword obj-type (name k)) (rw-ast v))))
                                     {}
                                     ?o)
                          (rw-ast ?o))) ; If here actual-type probably turned it into a primitive.
        :else o))

;;; ToDo: :FnCall/fn-name needs this but doesn't work like the others!
(defn adjust-exp
  "Correct a few annoying things about rw-ast: vars and fields define strings;
   they don't have to be boxed."
  [o]
  (let [typ (:table/type o)]
    (cond (and (map? o) (#{:VarDef :FieldAccess :FnCall} typ))
          (->> (case typ
                 :FnCall (assoc o :FnCall/fn-name (-> o :FnCall/fn-name :BoxedStr/val))
                 :VarDef (-> o
                             (assoc  :VarDef/var-name (-> o :VarDef/jvar-name :BoxedStr/val))
                             (dissoc :VarDef/jvar-name))
                 :FieldAccess (assoc o :FieldAccess/field-name (-> o :FieldAccess/field-name :BoxedStr/val)))
               (reduce-kv (fn [m k v] (assoc m k (adjust-exp v))) {})),
          (map? o)      (reduce-kv (fn [m k v] (assoc m k (adjust-exp v))) {} o)
          (vector? o)   (mapv adjust-exp o)
          :else o)))

(defn set-indexes
  "A few object types, such as function calls and arrays, hold an ordered collection of elements.
   This returns the object with the elements of those sub-objects indexed."
  [o]
  (letfn [(updat [obj attr attr-ix]
            (update obj
                    attr
                    #(mapv (fn [e i] (assoc e attr-ix i))
                           %
                           (range 1 (-> % count inc)))))]
    (let [typ (:table/type o)]
      (cond (and (map? o) (#{:Array :FnCall :Object} typ))
            (->> (case typ
                   :Array  (updat o :Array/elem     :Array-elem/index)
                   :FnCall (updat o :FnCall/args    :FnCall-args/index)
                   :Object (updat o :Object/kv-pair :Object-kvpair/index))
                 (reduce-kv (fn [m k v] (assoc m k (set-indexes v))) {})),
            (map? o)      (reduce-kv (fn [m k v] (assoc m k (set-indexes v))) {} o),
            (vector? o)   (mapv set-indexes o),
            :else o))))

(defn mark-toplevel
  "Mark the toplevel expression as such."
  [obj]
  (cond (map? obj)            (assoc obj :table/toplevel-exp? true)
        (vector? obj)  (mapv #(assoc %   :table/toplevel-exp? true) obj)
        :else (throw (ex-info "Toplevel is a primitive type?" {:obj obj}))))

(def scott-result (rew/rewrite* :ptag/exp "data/testing/map-examples/shipped-item-instance-clean.json" :file? true :simplify? true))
(defn tryme
  []
  (-> scott-result
      rw-ast
      mark-toplevel
      adjust-exp
      set-indexes
      qu/db-for!))

(defn tryme-2
  []
  (-> scott-result
      rw-ast
      mark-toplevel
      adjust-exp
      set-indexes
      qu/learn-schema-walking
      #_qu/db-for!))

;;; ToDo: remove the :resource/iri stuff here
;;; ToDo: I think this thing gets stuck on cyclic data! Break cycles.
(defn resolve-obj
  "Resolve :db/id in the argument map."
  [m conn & {:keys [keep-db-ids?]}]
  (letfn [(subobj [x]
            (cond (and (map? x) (contains? x :db/id) (== (count x) 1))                 ; It is an object ref...
                  (or (and (map? x)
                           (contains? x :db/id)
                           (d/q `[:find ?id . :where [~(:db/id x) :resource/iri ?id]] conn)) ; ...return keyword if it is a resource...
                      (subobj (dp/pull conn '[*] (:db/id x)))),                              ; ...otherwise it is some other structure.
                  (map? x) (reduce-kv
                            (fn [m k v] (if (and (= k :db/id) (not keep-db-ids?)) m (assoc m k (subobj v))))
                            {} x),
                  (vector? x) (mapv subobj x),
                  :else x))]
    (-> (reduce-kv (fn [m k v] (assoc m k (subobj v))) {} m)
        (dissoc :db/id))))

(defn get-schema
  "Return the db schema."
  [db]
  (->> (dp/pull-many
        db
        '[*]
        (d/q '[:find [?eid ...] :where [?eid :db/ident ?id]] db))
       (sort-by #(-> % :db/ident namespace))))

(defn create-table
  [db-ident])
  
