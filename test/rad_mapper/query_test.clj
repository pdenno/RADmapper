(ns rad-mapper.query-test
  (:require
   [clojure.pprint         :refer [pprint]]
   [clojure.test           :refer  [deftest is testing]]
   [clojure.walk]
   [datahike.api           :as d]
   [datahike.pull-api      :as dp]
   [owl-db-tools.resolvers :refer [pull-resource]]
   [rad-mapper.builtins    :as bi]
   [rad-mapper.query       :as qu]
   [rad-mapper.rewrite     :as rew]
   [rad-mapper.devl.devl-util :as devl :refer [run-test nicer nicer- run run-rew examine remove-meta]]))

(defmacro run-test-rew
  "Use this to expand devl/run-test with :rewrite? true."
  [exp gold]
  `(run-test ~exp ~gold :rewrite? true))

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
    (let [conn @(qu/db-for! dolce-test-data)
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
    (let [conn @(qu/db-for! dolce-test-data)
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
           (let [conn @(qu/db-for! {:person/fname "Bob" :person/lname "Clark"})]
             (dp/pull-many conn '[*] (range 1 (-> conn :max-eid inc))))))))

;;;================================ testing query ==================================
(deftest query-basics
  (testing "Testing query parsing, rewriting and execution."
    (testing "simple"
      (run-test-rew
       "query(){[?class :rdf/type     'owl/Class']
              [?class :resource/iri  ?class-iri]}"
       '(bi/query '[] '[[?class :rdf/type "owl/Class"] [?class :resource/iri ?class-iri]])))

    (testing "the attr of a triple (middle item) can be queried and results do not include db/schema content."
      (run-test
       "( $data := [{'person/fname' : 'Peter', 'person/lname' : 'Dee'}];
           $q := query(){[?ent ?attr ?val]};
           $q($data))"
       '[{:?ent 3, :?attr :person/lname, :?val "Dee"}
         {:?ent 3, :?attr :person/fname, :?val "Peter"}]))

  (testing "rewriting of an in-line query"
    (run-test-rew
     "query(){[?ent ?attr ?val]}([{'person/fname' : 'Peter', 'person/lname' : 'Dee'}])"
     '((bi/query '[] '[[?ent ?attr ?val]])
       [(-> {}
            (assoc "person/fname" "Peter")
            (assoc "person/lname" "Dee"))])))

  (testing "rewriting of an in-line query with a parameter."
    (run-test-rew
     "($qBob := query($name){[?e :name $name]}('Bob');
         $qBob([{'name' : 'Bob'}]))"
     '(bi/primary
       (let [$qBob ((bi/query '[$name] '[[?e :name $name]]) "Bob")]
         ($qBob [(-> {} (assoc "name" "Bob"))])))))

  (testing "execution of an in-line query"
    (run-test
     "query(){[?ent ?attr ?val]}([{'person/fname' : 'Peter', 'person/lname' : 'Dee'}])"
     [{:?ent 3, :?attr :person/lname, :?val "Dee"} {:?ent 3, :?attr :person/fname, :?val "Peter"}]))


  (testing "execution of an in-line query with a parameter"
    (run-test
     "($qBob := query($name){[?e :name $name]}('Bob');
       $qBob([{'name' : 'Bob'}]))"
     [{:?e 2}]))

  (testing "rewriting a query using a mapping context with the default name 'source-data-1'"
    (run-test-rew
     "( $mc := $newContext() ~> $addSource([{'person/fname' : 'Bob', 'person/lname' : 'Clark'}]);
         $q := query(){[?person :person/fname ?fname]
                       [?person :person/lname ?lname]};
         $q($getSource('source-data-1')) )"
     '(bi/primary
       (let [$mc (bi/thread (bi/$newContext)
                            (fn [_x1] (bi/$addSource
                                       _x1
                                       (with-meta [(-> {}
                                                       (assoc "person/fname" "Bob")
                                                       (assoc "person/lname" "Clark"))]
                                         #:bi{:json-array? true}))))
             $q (bi/query '[] '[[?person :person/fname ?fname] [?person :person/lname ?lname]])]
         ($q (bi/$getSource "source-data-1"))))))

  (testing "execution of a query using a mapping context with the default name 'source-data-1'."
    (run-test
     "( $mc := $newContext() ~> $addSource([{'person/fname' : 'Bob', 'person/lname' : 'Clark'}]);
         $q := query(){[?person :person/fname ?fname]
                       [?person :person/lname ?lname]};
         $q($getSource($mc, 'source-data-1')) )"
     [{:?person 3, :?fname "Bob", :?lname "Clark"}]))

  (testing "tests execution of a query using a mapping context with the default name 'source-data-1' (in spec)"
    (run-test
     "( $data := [{'Person/firstname' : 'Bob'  , 'Person/lastname' : 'Clark'},
                  {'Person/firstname' : 'Peter', 'Person/lastname' : 'Dee'}];
           $q := query(){[?person :Person/firstname ?fname]
                         [?person :Person/lastname  ?lname]};
        $q($data) )"
     [{:?person 4, :?fname "Peter", :?lname "Dee"}
      {:?person 3, :?fname "Bob",   :?lname "Clark"}]))

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
         (->> (rew/rewrite* :ptag/exp ; ToDo: This can use dolce-1.edn once heterogeneous data is handled.
                            "( $mc := $newContext() ~> $addSource($readFile('data/testing/dolce-2.edn'));
                                 $q := query(){[?class :rdf/type     'owl/Class']
                                               [?class :resource/iri  ?class-iri]};
                                 $q($getSource($mc, 'source-data-1')) )"
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

;;; This creates a function. :params are empty, so this is the "immediate" type.
(def qfn (bi/enforce {:params [],
                      :options '{separate true}
                      :body '{"instance-of" "FixedType"
                              "content" :?class-iri}}))

;;; This creates a higher-order function. This is the "immediate" type.
(def param-qfn (bi/enforce '{:params [$type],
                             :body {"instance-of" $type
                                    "content" :?class-iri}}))

(def pqfn (param-qfn "MyType"))

(deftest enforce-basics
  (testing "Testing basic enforce"
    (testing "rewriting"
      #_(run-test e0
                '(bi/enforce {:params [],
                              :body (-> {}
                                        (assoc "instance-of" "example")
                                        (assoc "content" (bi/get-from-b-set b-set :?class-iri)))})))

    (testing "The function has metadata."
      (is (= '#:bi{:params [b-set], :enforce? true, :body {"instance-of" "FixedType", "content" :?class-iri}}
             (meta qfn))))

    (testing "The function is executable with a binding set, creating content."
      (is (= {"instance-of" "FixedType", "content" "IRI"}
             (-> (qfn {:?class-iri "IRI"}) remove-meta))))

    (testing "e1 : rewrite for the parametric ('higher-order') is similar :params and closed over $type differ."
      (run-test-rew
       e1
       '(bi/enforce {:map-body '{"instance-of" $type, "content" ?class-iri}, :params '($type), :options 'nil})))

    (testing "the function returned is higher-order..."

      (testing "The function has the same metadata."
        (is (= '#:bi{:params [b-set], :enforce? true, :body {"instance-of" $type, "content" :?class-iri}}
               (meta pqfn))))

      (testing "Rather than 'FixedType', the instance-of is 'MyType'."
        (is (= {"instance-of" "MyType", "content" "IRI"}
               (pqfn {:?class-iri "IRI"})))))))

;;; ToDo: This might be a good example for the spec.
(defn enforce-reduce-demo
  "This demonstrates rewriting and basics operation; some _stuff below is not used."
  []
  (let [_b-sets   [{:?ent 1 :?word1 "Hello" :?word2 "world!"}
                   {:?ent 2 :?word1 "Nice"  :?word2 "day!"}]
        ;; Same thing produced by running a query with in-line data.
        b-sets (rew/rewrite* :ptag/exp
                             "query(){[?ent :greeting/word1 ?word1] [?ent :greeting/word2 ?word2]}
                                       ([{'greeting/word1' : 'Hello', 'greeting/word2' : 'world!'},
                                         {'greeting/word1' : 'Nice',  'greeting/word2' : 'day!'}])"
                             :execute? true)
        ;; Body will be used by bi/enforce to define a function and associate metadata with it.
        _e-fn :nyi #_(bi/enforce {:params [],
                           :body (-> {}
                                     (assoc "target/word1" (bi/get-from-b-set b-set :?word1))
                                     (assoc "target/word2" (bi/get-from-b-set b-set :?word2)))})
        ;; Same thing
        e-fn (rew/rewrite* :ptag/exp
                           "enforce (){ {'target/word1' : ?word1, 'target/word2' : ?word2} }"
                           :execute? true)]
        ;; Run it!
    (bi/$reduce b-sets e-fn [])))

(deftest the-enforce-reduce-demo
  (testing "Testing the basic use of enforce in $reduce."
    (is (= [#:target{:word1 "Nice", :word2 "day!"} #:target{:word1 "Hello", :word2 "world!"}]
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
  (   $mc := $newContext() ~> $addSource($readFile('data/testing/owl-example.edn'),'owl-source');

      $qClass := query()
                   { [?class :rdf/type            'owl/Class']
                     [?class :resource/iri        ?class-iri]
                     [?class :resource/namespace  ?class-ns]
                     [?class :resource/name       ?class-name]
                   };  /* Defines a higher-order function */

      $bsets := $qClass($getSource($mc, 'owl-source'));
      $reduce($bsets,
              enforce()
                 {  {'instance-of'  : 'insert-row',
                     'table'        : 'ObjectDefinition',
                     'content'      : {'resourceIRI'       : ?class-iri,
                                       'resourceNamespace' : ?class-ns,
                                       'resourceLabel'     : ?class-name}}
                 },
              [] /* In updating the target, there would be data here. */
              )
   )")

(def owl-full-parametric
"
  (   $mc := $newContext() ~> $addSource($readFile('data/testing/owl-example.edn'), 'owl-source');

      $qtype  := query($type)
                   { [?class :rdf/type            $type]
                     [?class :resource/iri        ?class-iri]
                     [?class :resource/namespace  ?class-ns]
                     [?class :resource/name       ?class-name]
                   };  /* Defines a higher-order function */
      $qClass := $qtype('owl/Class');
      $qProp  := $qtype('owl/ObjectProperty');
      $bsets := $qClass($getSource($mc, 'owl-source')); /* Run query; return a collection of binding sets. */
                                                        /* Could use ~> here; instead, I'm passing $bsets. */
      $reduce($bsets,
              enforce()
                 {  {'instance-of'  : 'insert-row',
                     'table'        : 'ObjectDefinition',
                     'content'      : {'resourceIRI'       : ?class-iri,
                                       'resourceNamespace' : ?class-ns,
                                       'resourceLabel'     : ?class-name}}
                 },
              [] /* In updating the target, there would be data here. */
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
  (testing "OWL example in the spec"
    (testing "rewriting a query expression. Note quotes; bi/query isn't a macro"
      (run-test-rew
       "query($type){[?class :rdf/type $type]
                     [?class :resource/iri ?class-iri]}"
     '(bi/query '[$type] '[[?class :rdf/type $type] [?class :resource/iri ?class-iri]])))

    (testing "rewriting an enforce"
      (run-test-rew
       owl-immediate-e1  ; was :ptag/enforce-def
       '(bi/enforce
         {:params [],
          :body (->  {}
                     (assoc "instance-of" "insert-row")
                     (assoc "table" "ObjectDefinition")
                     (assoc "content" (-> {}
                                          (assoc "resourceIRI" (bi/get-from-b-set b-set :?class-iri))
                                          (assoc "resourceNamespace" (bi/get-from-b-set b-set :?class-ns))
                                          (assoc "resourceLabel" (bi/get-from-b-set b-set :?class-name)))))})))

    (testing "rewriting the whole example"
      (run-test-rew
       owl-full-immediate
       '(bi/primary
         (let [$mc (bi/thread (bi/$newContext)
                              (fn [_x1] (bi/$addSource _x1 (bi/$readFile "data/testing/owl-example.edn") "owl-source")))
               $qClass (bi/query '[] '[[?class :rdf/type "owl/Class"]
                                       [?class :resource/iri ?class-iri]
                                       [?class :resource/namespace ?class-ns]
                                       [?class :resource/name ?class-name]])
               $bsets ($qClass (bi/$getSource $mc "owl-source"))]
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
                       [])))))))

;;; ToDo: Drop the addSource stuff?
(def owl-query-immediate
"
  (   $mc := $newContext() ~> $addSource($readFile('data/testing/owl-example.edn'), 'owl-source');

      $qClass := query()
                   { [?class :rdf/type            'owl/Class']
                     [?class :resource/iri        ?class-iri]
                     [?class :resource/namespace  ?class-ns]
                     [?class :resource/name       ?class-name]
                   };  /* Defines a higher-order function */

      $qClass($getSource($mc, 'owl-source'))  /* Run query; return a collection of binding sets. */
  )")

(def owl-query-parametric
"
  (   $mc := $newContext() ~> $addSource($readFile('data/testing/owl-example.edn'), 'owl-source');

      $qtype  := query($type)
                   { [?class :rdf/type            'owl/Class']
                     [?class :resource/iri        ?class-iri]
                     [?class :resource/namespace  ?class-ns]
                     [?class :resource/name       ?class-name]
                   };  /* Defines a function that returns a higher-order function. */

      $qClass := $qtype('owl/Class'); /* Make a query function by specifying parameter values. */

      $qClass($getSource($mc, 'owl-source'))  /* Run query; return a collection of binding sets. */
  )")

(deftest owl-example-executes
  (testing "Testing execution of $query and enforce."

    ;; bi/query is a higher-order function that returns either a query function, or a higher-order function
    ;; that takes a parameter to customize a 'parametric' query function.
    (let [data (-> (bi/$newContext) (bi/$addSource [{:name "Bob"}]))]
      (is (= [{:?e 2}] ((bi/query  []        '[[?e :name "Bob"]]) (bi/$getSource data "source-data-1"))))
      (is (= [{:?e 2}] (((bi/query '[$name]  '[[?e :name $name]]) "Bob") (bi/$getSource data "source-data-1"))))
      (is (= []        (((bi/query '[$name]  '[[?e :name $name]]) "xxx") (bi/$getSource data "source-data-1")))))

    ;; This tests making a new data source and use of a special.
    ;; I just check the type because the actual DB can't be tested for equality.
    (is (= datahike.db.DB
           (do (rew/rewrite* :ptag/exp
                             "($ := $newContext() ~> $addSource($readFile('data/testing/owl-example.edn'), 'owl-data');)"
                             :execute? true)
               (-> @bi/$ :sources (get "owl-data") type))))

    (testing "executing owl-query-immediate"
      (run-test
       owl-query-immediate
       [{:?class 12, :?class-iri "dol/endurant", :?class-ns "dol", :?class-name "endurant"}]))

    (testing "executing owl-query-parametric"
      (run-test owl-query-parametric
                [{:?class 12, :?class-iri "dol/endurant", :?class-ns "dol", :?class-name "endurant"}]))

    (testing "executing owl-full-immediate"
      (run-test owl-full-immediate
                [{:instance-of "insert-row",
                  :table "ObjectDefinition",
                  :content {:resourceIRI "dol/endurant"
                            :resourceNamespace "dol"
                            :resourceLabel "endurant"}}]))

    (testing "executing owl-full-parametric"
      (run-test owl-full-parametric
                '[{:instance-of "insert-row",
                   :table "ObjectDefinition",
                   :content {:resourceIRI "dol/endurant"
                             :resourceNamespace "dol"
                             :resourceLabel "endurant"}}]))))

;;;================================================================================================================
;;; "owl-db-tools is used only in development (See deps.edn.)  It is here mostly to ensure it has needed functionality."
(deftest use-of-owl-db-tools-query
  (testing "owl-db-tools/pull-resource"
    (is (=
         {:resource/iri :dol/perdurant,
          :resource/name "perdurant",
          :resource/namespace "dol",
          :owl/disjointWith [:dol/endurant :dol/abstract :dol/quality],
          :rdf/type :owl/Class,
          :rdfs/subClassOf
          #{{:owl/onProperty :dol/participant, :owl/someValuesFrom [:dol/endurant], :rdf/type :owl/Restriction}
            {:owl/onProperty :dol/has-quality, :owl/someValuesFrom [:dol/temporal-location_q], :rdf/type :owl/Restriction}
            {:owl/onProperty :dol/specific-constant-constituent, :owl/allValuesFrom [:dol/perdurant], :rdf/type :owl/Restriction}
            :dol/spatio-temporal-particular
            {:owl/onProperty :dol/part, :owl/allValuesFrom [:dol/perdurant], :rdf/type :owl/Restriction}
            {:owl/onProperty :dol/has-quality, :owl/allValuesFrom [:dol/temporal-quality], :rdf/type :owl/Restriction}}}
         ;; Returns a sorted-map, thus str and read-string.
         (-> (pull-resource :dol/perdurant conn)
             (dissoc :rdfs/comment)
             str
             read-string
             (update :rdfs/subClassOf set))))))

(def owl-full-enforce-extra
  "In this one, which is what I think should be in the spec, I'm not bothering with MCs."
"
( $data := $readFile('data/testing/owl-example.edn');

  $qtype  := query($rdfType, $extraTrips)
               { [?class :rdf/type            $rdfType]
                 [?class :resource/iri        ?class-iri]
                 [?class :resource/namespace  ?class-ns]
                 [?class :resource/name       ?class-name]
                 /* ToDo: $extraTrips */
               };  /* Defines a higher-order function, a template of sorts. */

  $etype  := enforce($tableType)
              {  {'instance-of'  : 'insert-row',
                  'table'        : $tableType,
                  'content'      : {'resourceIRI'       : ?class-iri,
                                    'resourceNamespace' : ?class-ns,
                                    'resourceLabel'     : ?class-name}}
                           }; /* Likewise, for an enforce template. */
                              /* The target tables for objects and relations a very similar. */

  $quClass := $qtype('owl/Class');     /* Use the template, here and the next three assignments. */

  /* This one doesn't just specify a value for $rdfType, but for $extraTrips. */
  $quProp       := $qtype('owl/ObjectProperty'); /* ToDo: ,queryTriples{[?class :rdfs/domain ?domain] [?class :rdfs/range ?range]}); */
  $enClassTable := $etype('ClassDefinition');
  $enPropTable  := $etype('PropertyDefinition');

  /* Run the class query; return a collection of binding sets about classes. */
  $clasBsets := $quClass($data);

  /* We start enforcing with no data, thus the third argument is []. */
  $tar_data := $reduce($clasBsets, $enClassTable, []);

  /* Get bindings sets for the ObjectProperties and make similar tables. */
  $propBsets := $quProp($data);

  /* We pass in the target data created so far. */
  $reduce($propBsets, $enPropTable, $tar_data) /* The code block returns the target data. */
)")

(deftest maybe-spec-example
  (testing "Testing the above long example"
    (run-test-rew
     owl-full-enforce-extra
     '(bi/primary
       (let [$data (bi/$readFile "data/testing/owl-example.edn")
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
                                  (assoc "table" $tableType)
                                  (assoc
                                   "content"
                                   (-> {}
                                       (assoc "resourceIRI" (bi/get-from-b-set b-set :?class-iri))
                                       (assoc "resourceNamespace" (bi/get-from-b-set b-set :?class-ns))
                                       (assoc "resourceLabel" (bi/get-from-b-set b-set :?class-name)))))})
             $quClass ($qtype "owl/Class")
             $quProp  ($qtype "owl/ObjectProperty")
             $enClassTable ($etype "ClassDefinition")
             $enPropTable ($etype "PropertyDefinition")
             $clasBsets ($quClass $data)
             $tar_data (bi/$reduce $clasBsets $enClassTable (with-meta [] #:bi{:json-array? true}))
             $propBsets ($quProp $data)]
         (bi/$reduce $propBsets $enPropTable $tar_data))))))

(deftest rearrange
  (testing "rearrange data as shown in  https://try.jsonata.org/sTPDRs--6."
    (testing "query for rearrange example."
      (let [result (->> (run "($data := $readFile('data/testing/jsonata/sTPDRs--6.json');
                                  $q := query(){ [?s ?system-name ?x]
                                                 [($match(?system-name, /system\\d/))]
                                                 [?x :owners ?y]
                                                 [?y ?owner-name ?z]
                                                 [($match(?owner-name, /owner\\d/))]
                                                 [?z ?device-name ?d]
                                                 [($match(?device-name, /device\\d/))]
                                                 [?d :id ?id]
                                                 [?d :status ?status] };
                                   $q($data) )")
                        (map #(dissoc % :?y :?s :?z :?d :?x))
                        set)]
        (is (= #{{:?device-name :device3, :?system-name :system1, :?id 300, :?status "Ok", :?owner-name :owner2}
                 {:?device-name :device8, :?system-name :system2, :?id 800, :?status "Ok", :?owner-name :owner2}
                 {:?device-name :device2, :?system-name :system1, :?id 200, :?status "Ok", :?owner-name :owner1}
                 {:?device-name :device4, :?system-name :system1, :?id 400, :?status "Ok", :?owner-name :owner2}
                 {:?device-name :device5, :?system-name :system2, :?id 500, :?status "Ok", :?owner-name :owner1}
                 {:?device-name :device7, :?system-name :system2, :?id 700, :?status "Ok", :?owner-name :owner2}
                 {:?device-name :device6, :?system-name :system2, :?id 600, :?status "Ok", :?owner-name :owner1}
                 {:?device-name :device1, :?system-name :system1, :?id 100, :?status "Ok", :?owner-name :owner1}}
               result))))
    (testing "full example"
      (run-test "($data := $readFile('data/testing/jsonata/sTPDRs--6.json');
                     $q := query(){ [?s ?systemName ?x]
                                    [($match(?systemName, /system\\d/))]
                                    [?x :owners ?y]
                                    [?y ?ownerName ?z]
                                    [($match(?ownerName, /owner\\d/))]
                                    [?z ?deviceName ?d]
                                    [($match(?deviceName, /device\\d/))]
                                    [?d :id ?id]
                                    [?d :status ?status] };

                  $bsets := $q($data);

                  $reduce($bsets,
                          enforce()
                                 {  {'owners':
                                        {?ownerName:
                                           {?systemName:
                                              {?deviceName : {'id'     : ?id,
                                                              'status' : ?status}}}}}
                                 }
                         )
                 )"
                {"owners"
                 {"owner1" {"system1" {"device1" {"id" 100, "status" "Ok"},
                                       "device2" {"id" 200, "status" "Ok"}},
                            "system2" {"device5" {"id" 500, "status" "Ok"},
                                       "device6" {"id" 600, "status" "Ok"}}},
                  "owner2" {"system1" {"device3" {"id" 300, "status" "Ok"},
                                       "device4" {"id" 400, "status" "Ok"}},
                            "system2" {"device7" {"id" 700, "status" "Ok"},
                                       "device8" {"id" 800, "status" "Ok"}}}}}))))
