(ns rad-mapper.query-test
  (:require
   [clojure.test :refer  [deftest is testing]]
   #?(:clj  [datahike.api         :as d]
      :cljs [datascript.core      :as d])
   #?(:clj  [datahike.pull-api    :as dp]
      :cljs [datascript.pull-api  :as dp])
;  #?(:clj [owl-db-tools.resolvers :refer [pull-resource]])
   [rad-mapper.builtins           :as bi]
   [rad-mapper.evaluate           :as ev]
   [rad-mapper.query              :as qu]
   [rad-mapper.util               :as util]
   [dev.dutil :refer [run examine run-test run-rew remove-meta] :refer-macros [run-test]]))

;;; ToDo: I think I intended to test whether some data (from express?) results in this?
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
#?(:clj
(def dolce-test-data
  "Get maps of everything in the DOLCE (dol) namespace. "
  (->> "data/testing/onto/dolce-1.edn"
       slurp
       util/read-str
       (mapv #(dissoc % :rdfs/subClassOf :owl/equivalentClass)))))

#?(:clj
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
)

#?(:clj
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
             (dp/pull-many conn '[*] (range 1 (-> conn :max-eid inc)))))))))

;;;================================ testing query ==================================
(deftest query-basics
  (testing "Testing query parsing, rewriting and execution."
    (testing "simple"
      (is (= (run-rew
              "query(){[?class :rdf/type     'owl/Class']
                       [?class :resource/iri  ?class-iri]}")
             '(bi/query '[] '[[?class :rdf/type "owl/Class"] [?class :resource/iri ?class-iri]])))))

  (testing "the attr of a triple (middle item) can be queried and results do not include db/schema content."
    (run-test
     "( $data := [{'person/fname' : 'Peter', 'person/lname' : 'Dee'}];
           $q := query(){[?ent ?attr ?val]};
           $q($data))"
     '[{?attr :person/lname, ?val "Dee"}
       {?attr :person/fname, ?val "Peter"}]))

  (testing "rewriting of an in-line query"
      (is (= (run-rew "query(){[?ent ?attr ?val]}([{'person/fname' : 'Peter', 'person/lname' : 'Dee'}])")
             '((bi/query '[] '[[?ent ?attr ?val]])
               [(-> {}
                    (assoc "person/fname" "Peter")
                    (assoc "person/lname" "Dee"))]))))

  (testing "rewriting of an in-line query with a parameter."
    (is (= (run-rew "($qBob := query($name){[?e :name $name]}('Bob');
                      $qBob([{'name' : 'Bob'}]))")
           '(bi/primary
            (let [$qBob ((bi/query '[$name] '[[?e :name $name]]) "Bob")]
              (bi/fncall {:args [[(-> {} (assoc "name" "Bob"))]], :func $qBob}))))))

  (testing "execution of an in-line query"
    (run-test
     "query(){[?ent ?attr ?val]}([{'person/fname' : 'Peter', 'person/lname' : 'Dee'}])"
     '[{?attr :person/lname, ?val "Dee"} {?attr :person/fname, ?val "Peter"}]))


    (testing "In-line parametric"
    (run-test
     "($qBob := query($name){[?e :name $name]}('Bob');
       $qBob([{'name' : 'Bob'}]))"
     [{}]))


  ;; ToDo: I think the following should work. I get a "Parse ended prematurely."
    #_(testing "Simple parameteric in-lined (double in-line)"
      (run-test
       "query($name){[?e :name $name]}('Bob') ([{'name' : 'Bob'}])"
       [{}]))


  (testing "rewriting a query using a mapping context with the default name 'source-data-1'"
    (is (= (run-rew "( $data := [{'person/fname' : 'Bob', 'person/lname' : 'Clark'}];
                       $q := query(){[?person :person/fname ?fname]
                                     [?person :person/lname ?lname]};
                       $q($data)
                     )")
           '(bi/primary
             (let [$data (with-meta [(-> {}
                                         (assoc "person/fname" "Bob")
                                         (assoc "person/lname" "Clark"))]
                           #:bi{:json-array? true})
                   $q (bi/query '[] '[[?person :person/fname ?fname] [?person :person/lname ?lname]])]
               (bi/fncall {:args [$data], :func $q}))))))

  (testing "execution of a query using a mapping context with the default name 'source-data-1'."
    (run-test
     "(  $data := [{'person/fname' : 'Bob', 'person/lname' : 'Clark'}];
         $q := query(){[?person :person/fname ?fname]
                       [?person :person/lname ?lname]};
         $q($data) )"
     '[{?fname "Bob", ?lname "Clark"}]))

  (testing "tests execution of a query using a mapping context with the default name 'source-data-1' (in spec)"
    (run-test
     "( $data := [{'Person/firstname' : 'Bob'  , 'Person/lastname' : 'Clark'},
                  {'Person/firstname' : 'Peter', 'Person/lastname' : 'Dee'}];
           $q := query(){[?person :Person/firstname ?fname]
                         [?person :Person/lastname  ?lname]};
        $q($data) )"
     '[{?fname "Peter", ?lname "Dee"}
       {?fname "Bob",   ?lname "Clark"}]))

  ;; This tests query on data (rather than directly on a DB).
  #_(:clj
  (is (= #{:dol/endurant :dol/spatio-temporal-region :dol/abstract-region :dol/physical-region :dol/non-physical-endurant
           :dol/region :dol/quality :dol/physical-quality :dol/quale :dol/particular :dol/physical-endurant :dol/perdurant
           :dol/feature :dol/time-interval}
         (->> ((bi/query [] '[[?class :rdf/type :owl/Class] [?class :resource/iri  ?class-iri]]) dolce-test-data)
              (map #(get % '?class-iri))
              (map keyword)
              set))))

  ;; This one is the same as db-for-tests-2 but mostly in RADmapper language.
  #_(is (= #{:dol/endurant :dol/spatio-temporal-region :dol/abstract-region :dol/physical-region :dol/non-physical-endurant
           :dol/region :dol/quality :dol/physical-quality :dol/quale :dol/particular :dol/physical-endurant :dol/perdurant
           :dol/feature :dol/time-interval}
         (->> (run-rew ; ToDo: This can use dolce-1.edn once heterogeneous data is handled.
               "( $read('data/testing/dolce-2.edn');
                  $q := query(){[?class :rdf/type     'owl/Class']
                                [?class :resource/iri  ?class-iri]};
                  $q($) )")
              (map #(get % '?class-iri))
              (map keyword)
              set))))

;;;================================ testing express ==================================
(deftest simple-immediate
  (testing "Complete simple express"
    (run-test "( $data := {'instance-of' : 'example',
                      'content'     : 'some-val'};

               $q :=  query(){ [?e :instance-of ?what]
                               [?e :content     ?val ] };

               $e := express() { {'inst' : ?what,
                                  'val'  : ?val} };
               $bsets := $q($data);

               $reduce($bsets, $e) )"
              {"inst" "example", "val" "some-val"})))

(deftest simple-express
  (testing "the essential steps of a simple express"
    (is (= (let [bbody (bi/sbind-body '{"instance-of" "FixedType" "content" ?class-iri})
                 bset '{?class-iri "MY-IRI"}
                 ai-maps (apply merge {} (bi/body&bset-ai-maps bbody bset))]
             (reduce-kv (fn [m k v] (assoc-in m k v)) {} ai-maps))
           {"instance-of" "FixedType", "content" "MY-IRI"}))))

(deftest simple-parameteric-express
  (testing "That error from the below (efn, pefn, etc.) aren't about my screwing up the args!" ; ...which they are currently!
    (run-test "( $data   := {'instance-of' : 'MyType', 'content' : 'someContent'};
                 $q      := query($type) { [?e :instance-of $type]  /* INTERESTING! I can't do both set and bind! */
                                           [?e :content     ?content] };
                 $qq     := $q('MyType');
                 $bsets  := $qq($data);
                 $reduce($bsets, express(){ { 'instance-of' : 'MyType',
                                              'content'     : ?content } })
               )"
              {"content" "someContent", "instance-of" "MyType"})))

;;; This creates a function. :params are empty, so this is the "immediate" type.
(def efn (bi/express {:params [],
                      :body '{"instance-of" "FixedType"
                              "content" ?content}}))

;;; This creates a higher-order function. This is the "immediate" type.
(def param-efn (bi/express '{:params [$type],
                             :body '{"instance-of" $type
                                     "content" ?content}}))

(def pefn (param-efn "MyType"))

(deftest express-basics
  (testing "Testing basic express"
    (testing "rewriting"

    (testing "The function has metadata."
      (is (= '#:bi{:params [b-set], :express? true, :options nil}
             (meta efn))))

    (testing "The function is executable with a binding set, creating content."
      (is (= {"instance-of" "FixedType", "content" "someContent"}
             (-> (efn '{?content "someContent"}) remove-meta))))

    (testing "e1 : rewrite for the parametric ('higher-order') is similar :params and closed over $type differ."
      (is (= (run-rew "express($type) {  {'instance-of'  : $type,
                                          'content'      : ?class-iri }
                                       }")
       '(bi/express {:params '($type), :options 'nil, :body '{"instance-of" $type, "content" ?class-iri}}))))

    (testing "the function returned is higher-order..."

      (testing "The function has the same metadata."
        (is (= '#:bi{:params [b-set], :express? true, :options nil}
               (meta pefn))))

      ;; ToDo: Next two need investigation.
      ;; Note that the full example of this above, simple-parameteric-express, works fine.
      #_(testing "Rather than 'FixedType', the instance-of is 'MyType'."
        (is (= {"instance-of" "MyType", "content" "someContent"}
               (pefn '{?content "someContent"}))))

      #_(testing "The function execution provides an ai-map for $reduce/assoc-in."
        (is (= #:bi{:ai-map {["instance-of"] "MyType" ["content"] "someContent"}}
               (-> (pefn '{?content "someContent"}) meta))))))))

;;;====================================================================
;; OWL example from the Draft OAGi Interoperable Mapping Specification
;;;====================================================================
;;; I'm using here the OWL DB that I also use for owl-db-tools.
;;; To make this DB see data/testing/make-data/make_data.clj.

#?(:clj
(def db-cfg
  {:store {:backend :file :path (str (System/getenv "HOME") "/Databases/datahike-owl-db")}
   :keep-history? false
   :schema-flexibility :write}))

#?(:clj
(def conn (-> db-cfg d/connect deref)))

#_(:clj
(def owl-test-data "the simplified objects used in the Draft OAGi Interoperable Mapping Specification example"
  (reduce (fn [res obj]
            (conj res (as-> (pull-resource obj conn) ?o
                        (cond-> ?o
                          (contains? ?o :rdfs/comment)    (update :rdfs/comment (fn [c] (mapv #(str (subs % 0 40) "...") c)))
                          (contains? ?o :rdfs/domain)     (update :rdfs/domain first)
                          (contains? ?o :rdfs/range)      (update :rdfs/range first)
                          (contains? ?o :rdfs/subClassOf) (update :rdfs/subClassOf first)))))
          []
          [:dol/endurant :dol/participant :dol/participant-in])))

#_(defn write-pretty-file
  "Transform/filter and sort objects; write them to fname."
  [fname objs & {:keys [transform] :or {transform identity}}]
  (spit fname
        (with-out-str
          (println "[")
          (doseq [obj (->> objs transform (sort-by :resource/iri))]
            (println "\n")
            (pprint obj))
          (println "]"))))


(comment (write-pretty-file "data/testing/owl-example.edn" owl-test-data))

(def owl-full-immediate "The whole thing looks like this:"
"
  (   $read('data/testing/owl-example.edn');

      $qClass := query()
                   { [?class :rdf/type            'owl/Class']
                     [?class :resource/iri        ?class-iri]
                     [?class :resource/namespace  ?class-ns]
                     [?class :resource/name       ?class-name]
                   };  /* Defines a higher-order function */

      $bsets := $qClass($getSource($, 'owl-source'));
      $reduce($bsets,
              express()
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
  (   $read('data/testing/owl-example.edn');

      $qtype  := query($type)
                   { [?class :rdf/type            $type]
                     [?class :resource/iri        ?class-iri]
                     [?class :resource/namespace  ?class-ns]
                     [?class :resource/name       ?class-name]
                   };  /* Defines a higher-order function */
      $qClass := $qtype('owl/Class');
      $qProp  := $qtype('owl/ObjectProperty');
      $bsets := $qClass($);                             /* Run query; return a collection of binding sets. */
                                                        /* Could use ~> here; instead, I'm passing $bsets. */
      $reduce($bsets,
              express()
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
  "express()
     {  {'instance-of'  : 'insert-row',
         'table'        : 'ObjectDefinition',
         'content'      : {'resourceIRI'       : ?class-iri,
                           'resourceNamespace' : ?class-ns,
                           'resourceLabel'     : ?class-name}}
      }")

;;; ToDo: I think all the owl example stuff are screwed up because the output (and maybe DB) is messed up.
;;;       See the function write-pretty-file above.
#_(deftest owl-example-rewrite
  (testing "OWL example in the spec"
    (testing "rewriting a query expression. Note quotes; bi/query isn't a macro"
      (is (= (run-rew "query($type){[?class :rdf/type $type]
                                    [?class :resource/iri ?class-iri]}")
             '(bi/query '[$type] '[[?class :rdf/type $type] [?class :resource/iri ?class-iri]]))))

    (testing "rewriting an express"
      (run-rew  owl-immediate-e1 :nyi))))

(def owl-query-immediate
"
  (   $read('data/testing/owl-example.edn');

      $qClass := query()
                   { [?class :rdf/type            'owl/Class']
                     [?class :resource/iri        ?class-iri]
                     [?class :resource/namespace  ?class-ns]
                     [?class :resource/name       ?class-name]
                   };  /* Defines a higher-order function */

      $qClass($)  /* Run query; return a collection of binding sets. */
  )")

(def owl-query-parametric
"
  (   $read('data/testing/owl-example.edn');

      $qtype  := query($type)
                   { [?class :rdf/type            'owl/Class']
                     [?class :resource/iri        ?class-iri]
                     [?class :resource/namespace  ?class-ns]
                     [?class :resource/name       ?class-name]
                   };  /* Defines a function that returns a higher-order function. */

      $qClass := $qtype('owl/Class'); /* Make a query function by specifying parameter values. */

      $qClass($)  /* Run query; return a collection of binding sets. */
  )")

;;; ToDo: I think all the owl example stuff are screwed up because the output (and maybe DB) is messed up.
;;;       See the function write-pretty-file above.
#_(deftest owl-example-executes ; ToDo: Investigate
  (testing "Testing execution of query and express."

    ;; bi/query is a higher-order function that returns either a query function, or a higher-order function
    ;; that takes a parameter to customize a 'parametric' query function.
    (let [data (-> (bi/$newContext) (bi/$addSource [{:name "Bob"}]))]
      (is (= [{'?e 2}] ((bi/query  []        '[[?e :name "Bob"]]) (bi/$getSource data "source-data-1"))))
      (is (= [{'?e 2}] (((bi/query '[$name]  '[[?e :name $name]]) "Bob") (bi/$getSource data "source-data-1"))))
      (is (= []        (((bi/query '[$name]  '[[?e :name $name]]) "xxx") (bi/$getSource data "source-data-1")))))

    ;; This tests making a new data source and use of a special.
    ;; I just check the type because the actual DB can't be tested for equality.
    #?(:clj
    (is (= datahike.db.DB
           (do (ev/run-rew
                "($ := $newContext() ~> $addSource($read('data/testing/owl-example.edn'), 'owl-data');))"
               (-> @bi/$ :sources (get "owl-data") type))))))

    (testing "executing owl-query-immediate"
      (run-test
       owl-query-immediate
       [{'?class 12, '?class-iri "dol/endurant", '?class-ns "dol", '?class-name "endurant"}]))

    (testing "executing owl-query-parametric"
      (run-test owl-query-parametric
                [{'?class 12, '?class-iri "dol/endurant", '?class-ns "dol", '?class-name "endurant"}]))

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
#_(:clj
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
             util/read-str
             (update :rdfs/subClassOf set)))))))

(def owl-full-express-extra
  "In this one, which is what I think should be in the spec, I'm not bothering with MCs."
"
( $data := $read('data/testing/owl-example.edn');

  $qtype  := query($rdfType, $extraTrips)
               { [?class :rdf/type            $rdfType]
                 [?class :resource/iri        ?class-iri]
                 [?class :resource/namespace  ?class-ns]
                 [?class :resource/name       ?class-name]
                 /* ToDo: $extraTrips */
               };  /* Defines a higher-order function, a template of sorts. */

  $etype  := express($tableType)
              {  {'instance-of'  : 'insert-row',
                  'table'        : $tableType,
                  'content'      : {'resourceIRI'       : ?class-iri,
                                    'resourceNamespace' : ?class-ns,
                                    'resourceLabel'     : ?class-name}}
                           }; /* Likewise, for an express template. */
                              /* The target tables for objects and relations a very similar. */

  $quClass := $qtype('owl/Class');     /* Use the template, here and the next three assignments. */

  /* This one doesn't just specify a value for $rdfType, but for $extraTrips. */
  $quProp       := $qtype('owl/ObjectProperty'); /* ToDo: ,queryTriples{[?class :rdfs/domain ?domain] [?class :rdfs/range ?range]}); */
  $enClassTable := $etype('ClassDefinition');
  $enPropTable  := $etype('PropertyDefinition');

  /* Run the class query; return a collection of binding sets about classes. */
  $clasBsets := $quClass($data);

  /* We start expressing with no data, thus the third argument is []. */
  $tar_data := $reduce($clasBsets, $enClassTable, []);

  /* Get bindings sets for the ObjectProperties and make similar tables. */
  $propBsets := $quProp($data);

  /* We pass in the target data created so far. */
  $reduce($propBsets, $enPropTable, $tar_data) /* The code block returns the target data. */
)")

;;; ToDo: I think all the owl example stuff are screwed up because the output (and maybe DB) is messed up.
;;;       See the function write-pretty-file above.
#_(deftest maybe-spec-example
  (testing "Testing the above long example"
    (run-test-rew
     owl-full-express-extra
     '(bi/primary
       (let [$data (bi/$read "data/testing/owl-example.edn")
             $qtype (bi/query '[$rdfType $extraTrips]
                              '[[?class :rdf/type $rdfType]
                                [?class :resource/iri ?class-iri]
                                [?class :resource/namespace ?class-ns]
                                [?class :resource/name ?class-name]])
             $etype (bi/express {:params [$tableType],
                                 :body
                                 (->
                                  {}
                                  (assoc "instance-of" "insert-row")
                                  (assoc "table" $tableType)
                                  (assoc
                                   "content"
                                   (-> {}
                                       (assoc "resourceIRI" (bi/get-from-b-set b-set '?class-iri))
                                       (assoc "resourceNamespace" (bi/get-from-b-set b-set '?class-ns))
                                       (assoc "resourceLabel" (bi/get-from-b-set b-set '?class-name)))))})
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
      (let [result (->> (run "($data := $read('data/testing/jsonata/sTPDRs--6.json');
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
                        (map #(dissoc % '?y '?s '?z '?d '?x))
                        set)]
        (is (= #{{'?device-name :device3, '?system-name :system1, '?id 300, '?status "Ok", '?owner-name :owner2}
                 {'?device-name :device8, '?system-name :system2, '?id 800, '?status "Ok", '?owner-name :owner2}
                 {'?device-name :device2, '?system-name :system1, '?id 200, '?status "Ok", '?owner-name :owner1}
                 {'?device-name :device4, '?system-name :system1, '?id 400, '?status "Ok", '?owner-name :owner2}
                 {'?device-name :device5, '?system-name :system2, '?id 500, '?status "Ok", '?owner-name :owner1}
                 {'?device-name :device7, '?system-name :system2, '?id 700, '?status "Ok", '?owner-name :owner2}
                 {'?device-name :device6, '?system-name :system2, '?id 600, '?status "Ok", '?owner-name :owner1}
                 {'?device-name :device1, '?system-name :system1, '?id 100, '?status "Ok", '?owner-name :owner1}}
               result))))
    (testing "full sTPDRs--6 rearrange example"
      (run-test "($data := $read('data/testing/jsonata/sTPDRs--6.json');
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
                          express()
                                 {  {'owners':
                                        {?ownerName:
                                          {'systems':
                                             {?systemName:
                                                {?deviceName : {'id'     : ?id,
                                                                'status' : ?status}}}}}}
                                 }
                         )
                 )"
                {"owners"
                 {"owner1" {"systems" {"system1" {"device1" {"id" 100, "status" "Ok"},
                                                  "device2" {"id" 200, "status" "Ok"}},
                                       "system2" {"device5" {"id" 500, "status" "Ok"},
                                                  "device6" {"id" 600, "status" "Ok"}}}},
                  "owner2" {"systems" {"system1" {"device3" {"id" 300, "status" "Ok"},
                                                  "device4" {"id" 400, "status" "Ok"}},
                                       "system2" {"device7" {"id" 700, "status" "Ok"},
                                                  "device8" {"id" 800, "status" "Ok"}}}}}}))))


;;; ToDo: Also demonstrate query with parameter for $id.
(deftest two-source
  (testing "Testing a simple 2-source problem."
    (testing "Show me the data...Oops!"
      (is (every? fn?
                  (run "( $DBa := [{'id' : 123, 'aAttr' : 'A-value'}];
                          $DBb := [{'id' : 123, 'bAttr' : 'B-value'}];

                          $aRes := query(){[?e :aAttr ?aData]};
                          $bRes := query(){[?e :bAttr ?bData]};

                          [$aRes, $bRes] )"))))


    (testing "Really show me the data."
      (run-test "( $DBa := [{'id' : 123, 'aAttr' : 'A-value'}];
                   $DBb := [{'id' : 123, 'bAttr' : 'B-value'}];

                   $aFn := query(){[?e :aAttr ?aData]};
                   $bFn := query(){[?e :bAttr ?bData]};

                   $aRes := $aFn($DBa);
                   $bRes := $bFn($DBb);

                   [$aRes, $bRes] )"
      [[{'?aData "A-value"}] [{'?bData "B-value"}]]))

    (testing "Really show me the data, and BTW, use a parametric query." ; FIX THIS??? Maybe skip this one in presentation.
      (run-test
        "( $DBa := [{'id' : 123, 'aAttr' : 'A-data'}];
           $DBb := [{'id' : 123, 'bAttr' : 'B-data'}];

           $pFnT := query($vAttr){[?e $vAttr ?data]     /* Send a qvar here??? */
                                  [?e :id    ?id]};

           $qaFn := $pFnT(:aAttr);
           $qbFn := $pFnT(:bAttr);

           $aRes := $qaFn($DBa);
           $bRes := $qbFn($DBb);

           [$aRes, $bRes] )"
        [[{'?data "A-data", '?id 123}] [{'?data "B-data", '?id 123}]]))

    (testing "The above isn't what you want! You want to query both DBs TOGETHER."
      (run-test "( $DBa := [{'id' : 123, 'aAttr' : 'A-data'}];
                   $DBb := [{'id' : 123, 'bAttr' : 'B-data'}];

                   $qFn :=  query(){[$DBa ?e1 :id    ?id]
                                    [$DBb ?e2 :id    ?id]       /* Match on ID. */
                                    [$DBa ?e1 :aAttr ?aData]
                                    [$DBb ?e2 :bAttr ?bData]};

                   $qFn($DBa, $DBb) )"
                [{'?id 123, '?aData "A-data", '?bData "B-data"}]))

    ;; ToDo: Use one "$Dba" to test argument counting!
    (testing "Similar to the above, but parametric."
      (run-test "( $DBa := [{'id' : 123, 'aAttr' : 'Bob-A-data',   'name' : 'Bob'},
                            {'id' : 234, 'aAttr' : 'Alice-A-data', 'name' : 'Alice'}];

                   $DBb := [{'id' : 123, 'bAttr' : 'Bob-B-data'},
                            {'id' : 234, 'bAttr' : 'Allic-B-data'}];

                   $qFnT :=  query($name){[$DBa ?e1 :id    ?id]
                                          [$DBb ?e2 :id    ?id]       /* Match on ID. */
                                          [$DBa ?e1 :name  $name]     /* Parametric on name */
                                          [$DBa ?e1 :name  ?name]     /* So that we capture name */
                                          [$DBa ?e1 :aAttr ?aData]
                                          [$DBb ?e2 :bAttr ?bData]};

                    $qFn := $qFnT('Bob');

                    $qFn($DBa, $DBb) )"
                [{'?id 123, '?aData "Bob-A-data", '?bData "Bob-B-data" '?name "Bob"}]))

    ;; Can just swap to $map in REPL for this one.
    (testing "Now we express the result by processing the b-sets."
      (run-test "( $DBa := [{'id' : 123, 'aAttr' : 'Bob-A-data',   'name' : 'Bob'},
                            {'id' : 234, 'aAttr' : 'Alice-A-data', 'name' : 'Alice'}];

                   $DBb := [{'id' : 123, 'bAttr' : 'Bob-B-data'},
                            {'id' : 234, 'bAttr' : 'Alice-B-data'}];

                   $qFn :=  query(){[$DBa ?e1 :id    ?id]
                                    [$DBb ?e2 :id    ?id]
                                    [$DBa ?e1 :name  ?name]
                                    [$DBa ?e1 :aAttr ?aData]
                                    [$DBb ?e2 :bAttr ?bData]};

                   $bSets := $qFn($DBa, $DBb);

                   $eFn := express(){{'name' : ?name, 'aData' : ?aData, 'bData' : ?bData, 'id' : ?id}};

                   $reduce($bSets, $eFn) )"
                {"aData" "Alice-A-data", "bData" "Alice-B-data", "id" 234, "name" "Alice"}))

    (testing "More like what you are looking for"
      (run-test "( $DBa := [{'id' : 123, 'aAttr' : 'Bob-A-data',   'name' : 'Bob'},
                            {'id' : 234, 'aAttr' : 'Alice-A-data', 'name' : 'Alice'}];

                   $DBb := [{'id' : 123, 'bAttr' : 'Bob-B-data'},
                            {'id' : 234, 'bAttr' : 'Alice-B-data'}];

                   $qFn :=  query(){[$DBa ?e1 :id    ?id]
                                    [$DBb ?e2 :id    ?id]
                                    [$DBa ?e1 :name  ?name]
                                    [$DBa ?e1 :aAttr ?aData]
                                    [$DBb ?e2 :bAttr ?bData]};

                   $bSets := $qFn($DBa, $DBb);

                   $eFn := express(){{?id : {'name' : ?name, 'aData' : ?aData, 'bData' : ?bData}}};

                   $reduce($bSets, $eFn) )"

                {123 {"aData" "Bob-A-data", "bData" "Bob-B-data", "name" "Bob"},
                 234 {"aData" "Alice-A-data", "bData" "Alice-B-data", "name" "Alice"}}))))

(deftest qif
  (testing "QIF"
    (testing "Express with hand-coded binding set"
      (run-test
         "( $bset := {?idKey    : 'KeyVal',
                      ?idKeyref : 'KeyrefVal',
                      ?instruct : 'some instruction',
                      ?method   : 'some method'};

            $eFn := express(){ {'QIFPlan/WorkInstructions' :
                                  {'QIFPlan.WorkInstructions/IdKey': ?idKey,
                                   'QIFPlan.WorkInstructions/IdKeyref': {'RefKey/id' : ?idKeyref},
                                   'QIFPlan.WorkInstructions/Instruction' :
                                      {'QIFPlan.WorkInstructions.Instruction/DocumentFileInstruction' : {'Instruction' : ?instruct}}
                                   },
                                'QIFPlan/ActionMethods' : {'QIFPlan/ActionMethods/ActionMethod' : {'Method' : ?method}}}};

          $eFn($bset)
        )"

         {"QIFPlan/WorkInstructions"
          {"QIFPlan.WorkInstructions/IdKey" "KeyVal",
           "QIFPlan.WorkInstructions/IdKeyref" {"RefKey/id" "KeyrefVal"},
           "QIFPlan.WorkInstructions/Instruction"
             {"QIFPlan.WorkInstructions.Instruction/DocumentFileInstruction" {"Instruction" "some instruction"}}},
          "QIFPlan/ActionMethods" {"QIFPlan/ActionMethods/ActionMethod" {"Method" "some method"}}}))))
