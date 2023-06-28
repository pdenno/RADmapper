(ns rad-mapper.query-test
  (:require
   [clojure.pprint               :refer [cl-format]]
   [clojure.test :refer  [deftest is testing]]
   #?(:clj  [datahike.api         :as d]
      :cljs [datascript.core      :as d])
   #?(:clj  [datahike.pull-api    :as dp]
      :cljs [datascript.pull-api  :as dp])
   ;;  #?(:clj [owl-db-tools.resolvers :refer [pull-resource]])
   #?(:cljs [cljs.reader])
   [rad-mapper.builtin            :as bi]
   [rad-mapper.query              :as qu]
   [rad-mapper.util               :as util]
   [develop.dutil :refer [run-rew]]
   [develop.dutil-util :refer [run remove-meta]]
   #?(:clj [develop.dutil-macros :as dutilm :refer [run-test #_unquote-body]]))
  #?(:cljs (:require-macros [develop.dutil-macros :as dutilm :refer [run-test #_unquote-body]])))

;;; Shadow can't see the one in dutilm. I think generally speaking, it can see the macros in those files, but nothing else.
(defn unquote-body
  "Walk through the body replacing (quote <qvar>) with <qvar>.
   Rationale: In most situations we want qvars to be rewritten as quoted symbols.
   An exception is their use in the :where of a datalog query. There may be more usages."
  [form]
  (letfn [(unq [obj]
            (cond (map? obj)                   (reduce-kv (fn [m k v] (assoc m (unq k) (unq v))) {} obj),
                  (vector? obj)                (mapv unq obj),
                  (and (seq? obj)
                       (= 'quote (first obj))) (if (== 2 (count obj)) (second obj) (rest obj)),
                  :else obj))]
    (unq form)))

(defn read-str [s]
  #?(:clj  (read-string s)
     :cljs (cljs.reader/read-string s)))

;;; ToDo: Dissoc is temporary; boxing.
;;; dolce-1.edn is both :owl/Class and :owl/ObjectProperty.
#?(:clj
(def dolce-test-data
  "Get maps of everything in the DOLCE (dol) namespace. "
  (->> "data/testing/onto/dolce-1.edn"
       slurp
       read-str
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
             '(bi/query
               {:params '[],
                :options nil,
                :pred-args '[],
                :body '[[?class :rdf/type "owl/Class"] [?class :resource/iri ?class-iri]],
                :in '[$]})))))

  (testing "the attr of a triple (middle item) can be queried and results do not include db/schema content."
    (is (= (-> (run
                 "( $data := [{'person/fname' : 'Peter', 'person/lname' : 'Dee'}];
                    $q := query(){[?ent ?attr ?val]};
                    $q($data))")
               set)
           #{{'?attr :person/lname, '?val "Dee"}
             {'?attr :person/fname, '?val "Peter"}})))

  (testing "rewriting of an in-line query"
    (is (= (run-rew "query(){[?ent ?attr ?val]}([{'person/fname' : 'Peter', 'person/lname' : 'Dee'}])")
           '((bi/query {:params '[],
                        :options nil,
                        :pred-args '[],
                        :body '[[?ent ?attr ?val]],
                        :in '[$]})
             [{"person/fname" "Peter"
               "person/lname" "Dee"}]))))


  (testing "rewriting of an in-line query with a parameter."
    (is (= (run-rew "($qBob := query($name){[?e :name $name]}('Bob');
                      $qBob([{'name' : 'Bob'}]))")
           '(bi/primary (let [$qBob ((bi/query {:params '[$name],
                                                :options nil,
                                                :pred-args '[],
                                                :body '[[?e :name $name]],
                                                :in '[$]}) "Bob")]
                          (bi/fncall {:args [[{"name" "Bob"}]], :func $qBob}))))))

  (testing "execution of an in-line query"
    (is (= (-> (run "query(){[?ent ?attr ?val]}([{'person/fname' : 'Peter', 'person/lname' : 'Dee'}])") set)
           #{'{?attr :person/lname, ?val "Dee"} '{?attr :person/fname, ?val "Peter"}})))

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
             (let [$data (with-meta [{"person/fname" "Bob"
                                      "person/lname" "Clark"}]
                           #:bi{:json-array? true})
                   $q (bi/query {:params '[],
                                 :options nil,
                                 :pred-args '[],
                                 :body '[[?person :person/fname ?fname] [?person :person/lname ?lname]],
                                 :in '[$]})]
               (bi/fncall {:args [$data], :func $q}))))))

  (testing "execution of a query using a mapping context with the default name 'source-data-1'."
    (run-test
     "(  $data := [{'person/fname' : 'Bob', 'person/lname' : 'Clark'}];
         $q := query(){[?person :person/fname ?fname]
                       [?person :person/lname ?lname]};
         $q($data) )"
     '[{?fname "Bob", ?lname "Clark"}]))

  (testing "tests execution of a query using a mapping context with the default name 'source-data-1' (in spec)"
    (is (= (-> (run "( $data := [{'Person/firstname' : 'Bob'  , 'Person/lastname' : 'Clark'},
                                 {'Person/firstname' : 'Peter', 'Person/lastname' : 'Dee'}];
                       $q := query(){[?person :Person/firstname ?fname]
                                     [?person :Person/lastname  ?lname]};
                       $q($data) )")
               set)
           #{{'?fname "Peter", '?lname "Dee"} {'?fname "Bob",  '?lname "Clark"}})))

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

(deftest query-$match
  (testing "Testing that query can do $match (especially in SCI/DS)."
    (run "( $data := {'instance-of' : 'example',
                      'content'     : 'some-val'};

            $q := query{ [?e :instance-of ?str]
                         [($match(?str, /example/))] };

            $q($data) )")))

;;;================================ testing express ==================================
(deftest simple-immediate
  (testing "Complete simple express"
    (run-test "$reduce([{?what : 'some-instance', ?val : 'some-value'}],
                       express() {{'inst' : ?what, 'val' : ?val}})"
              {"inst" "some-instance", "val" "some-value"})))

(deftest simple-parameteric-express ; A misnomer! It is the query that is parametric.
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
                      :base-body '{"instance-of" "FixedType"
                                   "content" ?content}}))

;;; This creates a higher-order function. This is the "immediate" type.
(def param-efn (bi/express '{:params [$type],
                             :body '{"instance-of" $type
                                     "content" ?content}}))

(def pefn (param-efn "MyType"))

(deftest express-basics
  (testing "Testing basic express."
    (testing "Testing rewriting."

    (testing "Testing that the function has metadata."
      (is (= '#:bi{:params [b-set],
                   :express? true,
                   :options nil,
                   :base-body {"instance-of" "FixedType", "content" ?content},
                   :reduce-body
                   #:redex{:more
                           [#:redex{:user-key "instance-of", :instance-of--instance-of [:redex/express-key "instance-of"], :val "FixedType"}
                            #:redex{:user-key "content", :content--content [:redex/express-key "content"], :val ?content}]},
                   :key-order nil}
             (-> efn meta (dissoc :bi/schema)))))

    (testing "Testing that the function is executable with a binding set, creating content."
      (is (= {"instance-of" "FixedType", "content" "someContent"}
             (-> (efn '{?content "someContent"}) remove-meta))))

    (testing "Testing e1 : rewrite for the parametric ('higher-order') is similar :params and closed over $type differ."
      ;; Some issue with quotes here. Tedious!
      (is (= '{:params '($type), :key-order ["instance-of" "content"], :options 'nil, :base-body {"instance-of" $type, "content" '?class-iri}}
             (-> (run-rew "express($type) {  {'instance-of'  : $type,
                                              'content'      : ?class-iri }
                                          }")
                 second
                 (dissoc :schema)))))

    (testing "Testing the function returned is higher-order..."

      (testing "Testing that the function has the same metadata."
        (is (= '#:bi{:params [b-set], :express? true, :options nil, :base-body nil, :reduce-body nil, :key-order nil}
               (-> pefn meta (dissoc :bi/schema)))))

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
;;; ----- This stuff should be done in the exerciser once it is talking to the server. -----

;;;#?(:clj
;;;(def db-cfg
;;;  {:store {:backend :file :path (str (System/getenv "HOME") "/Databases/datahike-owl-db")}
;;;   :keep-history? false
;;;   :schema-flexibility :write}))
;;;
;;;#?(:clj (def conn (-> db-cfg d/connect deref)))

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


#_(write-pretty-file "data/testing/owl-example.edn" owl-test-data)

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
           (do (bi/run-rew
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
             read-str
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

;;; ToDo: Also demonstrate query with parameter for $id.
(deftest two-source
  (testing "Testing a simple 2-source problem."
    (testing "Testing query on both DBs."
      (run-test "( $DBa := [{'id' : 123, 'aAttr' : 'A-data'}];
                   $DBb := [{'id' : 123, 'bAttr' : 'B-data'}];

                   $qFn :=  query(){[$DBa ?e1 :id    ?id]
                                    [$DBb ?e2 :id    ?id]       /* Match on ID. */
                                    [$DBa ?e1 :aAttr ?aData]
                                    [$DBb ?e2 :bAttr ?bData]};

                   $qFn($DBa, $DBb) )"
                [{'?id 123, '?aData "A-data", '?bData "B-data"}]))

    ;; ToDo: Use one "$Dba" to test argument counting.
    (testing "Testing query parameteric $name."
      (run-test "( $DBa := [{'id' : 123, 'aAttr' : 'Bob-A-data',   'name' : 'Bob'},
                            {'id' : 234, 'aAttr' : 'Alice-A-data', 'name' : 'Alice'}];

                   $DBb := [{'id' : 123, 'bAttr' : 'Bob-B-data'},
                            {'id' : 234, 'bAttr' : 'Alice-B-data'}];

                   $qFnT :=  query($name){[$DBa ?e1 :id    ?id]
                                          [$DBb ?e2 :id    ?id]       /* Match on ID. */
                                          [$DBa ?e1 :name  $name]     /* Parametric on name */
                                          [$DBa ?e1 :name  ?name]     /* So that we capture name */
                                          [$DBa ?e1 :aAttr ?aData]
                                          [$DBb ?e2 :bAttr ?bData]};

                    $qFn := $qFnT('Bob');

                    $qFn($DBa, $DBb) )"
                '[{?id 123, ?name "Bob", ?aData "Bob-A-data", ?bData "Bob-B-data"}]))

    (testing "Small test through to $reduce."
      (run-test "( $bSets := [{?id : 123, ?name : 'Bob',   ?aData : 'Bob-A-data',   ?bData : 'Bob-B-data'},
                              {?id : 234, ?name : 'Alice', ?aData : 'Alice-A-data', ?bData : 'Alice-B-data'}];

                  $eFn := express(){{?name : {'bData' : ?bData}}};

                  $reduce($bSets, $eFn) )"

                {"Alice" {"bData" "Alice-B-data"},
                 "Bob"   {"bData" "Bob-B-data"}}))


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

                   $eFn := express{{?name : {'aData' : ?aData, 'bData' : ?bData, 'id' : ?id}}};

                   $reduce($bSets, $eFn) )"

                {"Alice" {"aData" "Alice-A-data", "bData" "Alice-B-data", "id" 234},
                 "Bob"   {"aData" "Bob-A-data",   "bData" "Bob-B-data",   "id" 123}}))

    (testing "Same thing with the bSets hand-coded."
      (run-test "( $bSets := [{?id : 123, ?name : 'Bob',   ?aData : 'Bob-A-data',   ?bData : 'Bob-B-data'},
                              {?id : 234, ?name : 'Alice', ?aData : 'Alice-A-data', ?bData : 'Alice-B-data'}];

                  $eFn := express(){{?name : {'aData' : ?aData, 'bData' : ?bData, 'id' : ?id}}};

                  $reduce($bSets, $eFn) )"

                {"Alice" {"aData" "Alice-A-data", "bData" "Alice-B-data", "id" 234},
                 "Bob"   {"aData" "Bob-A-data",   "bData" "Bob-B-data",   "id" 123}})))

    (testing "More like what you are looking for."
      (testing "Testing (additionally!) that non-string user-keys (the 'id' here) are returned to their original type."
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

(deftest express-body-map
  (testing "Testing mapping over an express body"
    (is (=
         [{"owners" [{"t/type" "OWNER", "owner/id" "owner1",
                      "owner/systems"
                      [{"t/type" "SYSTEM", "system/id" "system1",
                        "system/devices" [{"t/type" "DEVICE", "device/id" 100, "device/name" "device1", "device/status" "Ok"}]}]}]}
          {"owners" [{"t/type" "OWNER", "owner/id" "owner1",
                      "owner/systems"
                      [{"t/type" "SYSTEM", "system/id" "system1",
                        "system/devices" [{"t/type" "DEVICE", "device/id" 200, "device/name" "device2", "device/status" "Ok"}]}]}]}
          {"owners" [{"t/type" "OWNER", "owner/id" "owner2",
                      "owner/systems"
                      [{"t/type" "SYSTEM", "system/id" "system2",
                        "system/devices" [{"t/type" "DEVICE", "device/id" 800, "device/name" "device8", "device/status" "Ok"}]}]}]}]
         (run
           "($bsets := [{?systemName : 'system1', ?deviceName : 'device1', ?id : 100, ?status  : 'Ok', ?ownerName : 'owner1'},
                        {?systemName : 'system1', ?deviceName : 'device2', ?id : 200, ?status  : 'Ok', ?ownerName : 'owner1'},
                        {?systemName : 'system2', ?deviceName : 'device8', ?id : 800, ?status  : 'Ok', ?ownerName : 'owner2'}];

                  $map($bsets,
                       express{{'owners': [{'t/type'       : 'OWNER',
                                            'owner/id'     : key(?ownerName),
                                            'owner/systems': [{'t/type'         : 'SYSTEM',
                                                               'system/id'      : key(?systemName),
                                                               'system/devices' : [{'t/type'         : 'DEVICE',
                                                                                    'device/id'      : ?id,
                                                                                    'device/name'    : key(?deviceName),
                                                                                    'device/status'  : ?status}]}]}]}
                              }  )
                  )")))))

(deftest express-body-map-small
  (testing "Test small express body $map with various body topology (not that it matters much!)."
    (testing "Testing Type 1 (express keys) -- vector."
      (run-test "$map([{?deviceName : 'device1', ?id : 100},
                       {?deviceName : 'device2', ?id : 200}],
                      express{{'device/id' : key(?id)}})"

                [{"device/id" 100} {"device/id" 200}]))

    (testing "Testing Type 2 (qvar-in-key-pos) -- vector."
       (run-test "$map([{?deviceName : 'device1', ?id : 100},
                        {?deviceName : 'device2', ?id : 200}],
                      express{{?deviceName : {'id' : ?id}}})"

                 [{"device1" {"id" 100}}, {"device2" {"id" 200}}]))))

;;;=========================== express body $reduce / $map tests ========================
;;; Currently, express body must be a map structure. I think iteration can be
;;; addressed either through use of $map and $merge or a vector under the key
;;; as in the tests below (e.g. express-body-reduce-small).
;;; Also, the reduce on the body could be part of a larger expression.
(deftest express-body-reduce-small
  (testing "Test small express body $reduce with various body topology."
    (testing "Testing Type 1 (express keys) -- map."
      (run-test "$reduce([{?deviceName : 'device1', ?id : 100},
                          {?deviceName : 'device2', ?id : 200}],
                         express{{'devices' : [{'device/id' : key(?id)}]}})"

                {"devices" [{"device/id" 100} {"device/id" 200}]}))

    (testing "Testing Type 2 (qvar-in-key-pos) -- map."
      (run-test "$reduce([{?deviceName : 'device1', ?id : 100},
                          {?deviceName : 'device2', ?id : 200}],
                         express{{'devices' : {?deviceName : {'id' : ?id}}}})"

                {"devices" {"device1" {"id" 100}, "device2" {"id" 200}}}))))

(deftest express-body-reduce-medium-type1
  (testing "Test medium-sized express body $reduce / $maps with various body topology."
    (testing "Testing Type 1 (express keys)"
      (run-test "$reduce([{?systemName : 'system1', ?deviceName : 'device1', ?id : 100, ?status : 'Ok', ?ownerName : 'owner1'},
                          {?systemName : 'system2', ?deviceName : 'device8', ?id : 800, ?status : 'Ok', ?ownerName : 'owner2'}],

                         express(){{'owners': [{'owner/id' : key(?ownerName),
                                                'systems'  : [{'system/id'  : key(?systemName),
                                                               'devices'    : [{'device/id'     : key(?id),
                                                                                'device/name'   : ?deviceName,
                                                                                'device/status' : ?status}]}]}]}})"
                {"owners"
                 #{{"owner/id" "owner1",
                   "systems" #{{"system/id" "system1",
                               "devices" #{{"device/id" 100, "device/name" "device1", "device/status" "Ok"}}}}}
                  {"owner/id" "owner2",
                   "systems" #{{"system/id" "system2",
                               "devices" #{{"device/id" 800, "device/name" "device8", "device/status" "Ok"}}}}}}}
                :sets? true))))

(deftest express-body-reduce-medium-type2
  (testing "Test medium-sized express body $reduce / $maps with various body topology."
    (testing "Testing Type 2 (qvar-in-key-pos)"
      (run-test "$reduce([{?systemName : 'system1', ?deviceName : 'device1', ?id : 100, ?status : 'Ok', ?ownerName : 'owner1'},
                          {?systemName : 'system2', ?deviceName : 'device8', ?id : 800, ?status : 'Ok', ?ownerName : 'owner2'}],

                         express{ {'owners':
                                    {?ownerName:
                                       {'systems':
                                          {?systemName:
                                             {?deviceName : {'id'     : ?id,
                                                           'status' : ?status}}}}}}})"

             {"owners" {"owner1" {"systems" {"system1" {"device1" {"id" 100, "status" "Ok"}}}}
                        "owner2" {"systems" {"system2" {"device8" {"id" 800, "status" "Ok"}}}}}}))))

(deftest express-body-reduce-medium-mixed
  (testing "Test mediuum-sized express body with mixed keys/qvars."
    (run-test "$reduce([{?systemName : 'system1', ?id : 100, ?ownerName : 'owner1'},
                        {?systemName : 'system2', ?id : 800, ?ownerName : 'owner2'}],

                       express{ {?ownerName : {'systems'  : [{'system/id'  : key(?systemName),
                                                              'devices'    : [{'device/id'     : key(?id)}]}]}} })"

              {"owner1" {"systems" [{"system/id" "system1", "devices" [{"device/id" 100}]}]}
               "owner2" {"systems" [{"system/id" "system2", "devices" [{"device/id" 800}]}]}})))

(deftest express-body-reduce-type1
  (testing "Testing express body type 1 reduction (use of express keys)"
    (run-test "($bsets := [{?systemName : 'system1', ?deviceName : 'device3', ?id : 300, ?status : 'Ok', ?ownerName : 'owner2'},
                           {?systemName : 'system2', ?deviceName : 'device8', ?id : 800, ?status : 'Ok', ?ownerName : 'owner2'},
                           {?systemName : 'system1', ?deviceName : 'device4', ?id : 400, ?status : 'Ok', ?ownerName : 'owner2'},
                           {?systemName : 'system2', ?deviceName : 'device5', ?id : 500, ?status : 'Ok', ?ownerName : 'owner1'},
                           {?systemName : 'system2', ?deviceName : 'device7', ?id : 700, ?status : 'Ok', ?ownerName : 'owner2'},
                           {?systemName : 'system2', ?deviceName : 'device6', ?id : 600, ?status : 'Ok', ?ownerName : 'owner1'},
                           {?systemName : 'system1', ?deviceName : 'device1', ?id : 100, ?status : 'Ok', ?ownerName : 'owner1'},
                           {?systemName : 'system1', ?deviceName : 'device2', ?id : 200, ?status : 'Ok', ?ownerName : 'owner1'}];

                $reduce($bsets,
                        express(){{'owners': [{'owner/id' : key(?ownerName),
                                               'systems'  : [{'system/id'  : key(?systemName),
                                                              'devices'    : [{'device/id'     : key(?id),
                                                                               'device/name'   : ?deviceName,
                                                                               'device/status' : ?status}]}]}]}}))"

             {"owners" [{"owner/id" "owner1",
                         "systems" [{"system/id" "system1",
                                     "devices" [{"device/id" 100, "device/name" "device1", "device/status" "Ok"}
                                                {"device/id" 200, "device/name" "device2", "device/status" "Ok"}]}
                                    {"system/id" "system2",
                                     "devices" [{"device/id" 500, "device/name" "device5", "device/status" "Ok"}
                                                {"device/id" 600, "device/name" "device6", "device/status" "Ok"}]}]}
                        {"owner/id" "owner2",
                         "systems" [{"system/id" "system1",
                                     "devices" [{"device/id" 300, "device/name" "device3", "device/status" "Ok"}
                                                {"device/id" 400, "device/name" "device4", "device/status" "Ok"}]}
                                    {"system/id" "system2",
                                     "devices" [{"device/id" 700, "device/name" "device7", "device/status" "Ok"}
                                                {"device/id" 800, "device/name" "device8", "device/status" "Ok"}]}]}]})))

(deftest express-body-reduce-type2
  (testing "Testing express body type 1 reduction (use of express keys)"
    (run-test "($bsets := [{?systemName : 'system1', ?deviceName : 'device3', ?id : 300, ?status : 'Ok', ?ownerName : 'owner2'},
                           {?systemName : 'system2', ?deviceName : 'device8', ?id : 800, ?status : 'Ok', ?ownerName : 'owner2'},
                           {?systemName : 'system1', ?deviceName : 'device4', ?id : 400, ?status : 'Ok', ?ownerName : 'owner2'},
                           {?systemName : 'system2', ?deviceName : 'device5', ?id : 500, ?status : 'Ok', ?ownerName : 'owner1'},
                           {?systemName : 'system2', ?deviceName : 'device7', ?id : 700, ?status : 'Ok', ?ownerName : 'owner2'},
                           {?systemName : 'system2', ?deviceName : 'device6', ?id : 600, ?status : 'Ok', ?ownerName : 'owner1'},
                           {?systemName : 'system1', ?deviceName : 'device1', ?id : 100, ?status : 'Ok', ?ownerName : 'owner1'},
                           {?systemName : 'system1', ?deviceName : 'device2', ?id : 200, ?status : 'Ok', ?ownerName : 'owner1'}];

                $reduce($bsets,
                        express()
                               {  {'owners':
                                     {?ownerName:
                                        {'systems':
                                           {?systemName:
                                              {?deviceName : {'id'     : ?id,
                                                              'status' : ?status}}}}}}}))"

             {"owners" {"owner1" {"systems" {"system1" {"device1" {"id" 100, "status" "Ok"},
                                                        "device2" {"id" 200, "status" "Ok"}},
                                             "system2" {"device5" {"id" 500, "status" "Ok"},
                                                        "device6" {"id" 600, "status" "Ok"}}}},
                        "owner2" {"systems" {"system1" {"device3" {"id" 300, "status" "Ok"},
                                                        "device4" {"id" 400, "status" "Ok"}},
                                             "system2" {"device7" {"id" 700, "status" "Ok"},
                                                        "device8" {"id" 800, "status" "Ok"}}}}}})))


;;; For clojure; Kaocha does this better!
(defn check-all-express []
  {:express-body-map-small             (express-body-map-small)
   :express-body-reduce-small          (express-body-reduce-small)
   :express-body-reduce-medium-type1   (express-body-reduce-medium-type1)
   :express-body-reduce-medium-type2   (express-body-reduce-medium-type2)
   :express-body-reduce-medium-mixed   (express-body-reduce-medium-mixed)
   :express-body-reduce-type1          (express-body-reduce-type1)
   :express-body-reduce-type2          (express-body-reduce-type2)})

(defn parse-test-not-here []
  (bi/processRM :ptag/query-patterns
  "[$DBb ?e2 :id    ?id]       /* Match on ID. */
   [$DBa ?e1 :name  $name]     /* Parametric on name */
   [$DBa ?e1 :name  ?name]     /* So that we capture name */"
  {:rewrite? true}))

(deftest from-screen
  (testing "Testing bi/redex-keys-values, mostly"
    (run-test
     "($DBa := [{'email' : 'bob@example.com', 'aAttr' : 'Bob-A-data',   'name' : 'Bob'},
                {'email' : 'alice@alice.org', 'aAttr' : 'Alice-A-data', 'name' : 'Alice'}];

       $DBb := [{'id' : 'bob@example.com', 'bAttr' : 'Bob-B-data'},
                {'id' : 'alice@alice.org', 'bAttr' : 'Alice-B-data'}];

       $qFn :=  query(){[$DBa ?e1 :email ?id]
                        [$DBb ?e2 :id    ?id]
                        [$DBa ?e1 :name  ?name]
                        [$DBa ?e1 :aAttr ?aData]
                        [$DBb ?e2 :bAttr ?bData]};

       $bSet := $qFn($DBa, $DBb);

       $eFn := express(){{'name'  : key(?name),
                          'aData' : ?aData,
                          'bData' : ?bData}};

      $reduce($bSet, $eFn) )"
     [{"name" "Alice", "aData" "Alice-A-data", "bData" "Alice-B-data"}
      {"name" "Bob", "aData" "Bob-A-data", "bData" "Bob-B-data"}]))

    (testing "Testing qvar-in-key-pos, but WITH key(),  which should not matter."
      (run-test
       "($DBa := [{'email' : 'bob@example.com', 'aAttr' : 'Bob-A-data',   'name' : 'Bob'},
                  {'email' : 'alice@alice.org', 'aAttr' : 'Alice-A-data', 'name' : 'Alice'}];

         $DBb := [{'id' : 'bob@example.com', 'bAttr' : 'Bob-B-data'},
                  {'id' : 'alice@alice.org', 'bAttr' : 'Alice-B-data'}];

         $qFn :=  query(){[$DBa ?e1 :email ?id]
                          [$DBb ?e2 :id    ?id]
                          [$DBa ?e1 :name  ?name]
                          [$DBa ?e1 :aAttr ?aData]
                          [$DBb ?e2 :bAttr ?bData]};

         $bSet := $qFn($DBa, $DBb);

         $eFn := express(){{?id : {'name'  : key(?name),
                                   'aData' : ?aData,
                                   'bData' : ?bData}}};

        $reduce($bSet, $eFn) )"
       {"alice@alice.org" {"name" "Alice", "aData" "Alice-A-data", "bData" "Alice-B-data"},
        "bob@example.com" {"name" "Bob", "aData" "Bob-A-data", "bData" "Bob-B-data"}}))


      (testing "Testing qvar-in-key-pos, no key()."
      (run-test
       "($DBa := [{'email' : 'bob@example.com', 'aAttr' : 'Bob-A-data',   'name' : 'Bob'},
                  {'email' : 'alice@alice.org', 'aAttr' : 'Alice-A-data', 'name' : 'Alice'}];

         $DBb := [{'id' : 'bob@example.com', 'bAttr' : 'Bob-B-data'},
                  {'id' : 'alice@alice.org', 'bAttr' : 'Alice-B-data'}];

         $qFn :=  query(){[$DBa ?e1 :email ?id]
                          [$DBb ?e2 :id    ?id]
                          [$DBa ?e1 :name  ?name]
                          [$DBa ?e1 :aAttr ?aData]
                          [$DBb ?e2 :bAttr ?bData]};

         $bSet := $qFn($DBa, $DBb);

         $eFn := express(){{?id : {'name'  : key(?name),
                                   'aData' : ?aData,
                                   'bData' : ?bData}}};

        $reduce($bSet, $eFn) )"
       {"alice@alice.org" {"name" "Alice", "aData" "Alice-A-data", "bData" "Alice-B-data"},
        "bob@example.com" {"name" "Bob", "aData" "Bob-A-data", "bData" "Bob-B-data"}})))

;;; ToDo: placement of quotes is still a PITA, right?
(deftest more-and-obj
  (testing "Testing the generation of the reduce-express where nesting occurs vs. does not occur."
    (testing "Generating :redex/more; used where the attributes belong in 'this' object."
      (is (= '{:name [:redex/express-key ?name],
               :redex/user-key "name",
               :redex/ek-val ?name,
               :redex/more [#:redex{:bData--?name|bData [:redex/express-key ?name "bData"], :user-key "bData", :val ?bData}]}
             (-> (bi/processRM :ptag/exp "express(){{'name': key(?name), 'bData': ?bData}}" {:rewrite? true})
                 second ; first is call to bi/express.
                 :base-body
                 unquote-body
                 bi/reduce-body-and-schema
                 :reduce-body))))

    (testing "Generating :redex/obj for a fresh object at the key."
      (is (= '#:redex{:user-key ?name,
                      :?name--?name [:redex/express-key ?name],
                      :obj [#:redex{:user-key "bData", :bData--?name|bData [:redex/express-key ?name "bData"], :val ?bData}]}
             (-> (bi/processRM :ptag/exp "express{{?name : {'bData' : ?bData}}}" {:rewrite? true})
                 second
                 :base-body
                 unquote-body
                 bi/reduce-body-and-schema
                 :reduce-body))))

    (testing "When there are no keys, none of this matters." ; This is Case 2 in bi/redex-toplevel-merge
      (is (= '#:redex{:more
                      [#:redex{:user-key "name", :name--name [:redex/express-key "name"], :val ?name}
                       #:redex{:user-key "bData", :bData--bData [:redex/express-key "bData"], :val ?bData}]}
             (-> (bi/processRM :ptag/exp "express{{'name' : ?name, 'bData' : ?bData}}" {:rewrite? true})
                 second
                 :base-body
                 unquote-body
                 bi/reduce-body-and-schema
                 :reduce-body))))))

  (testing "Testing complete executions in these three cases."
    (testing "express from the first one above (a :redex/more).  This one is a bit bogus as a reduce, produces a vector."
      (run-test
       "($DBa := [{'email' : 'bob@example.com', 'name' : 'Bob'},
                  {'email' : 'alice@alice.org', 'name' : 'Alice'}];
         $DBb := [{'id' : 'bob@example.com', 'bAttr' : 'Bob-B-data'},
                  {'id' : 'alice@alice.org', 'bAttr' : 'Alice-B-data'}];
         $qFn :=  query(){[$DBa ?e1 :email ?id] [$DBb ?e2 :id ?id] [$DBa ?e1 :name ?name] [$DBb ?e2 :bAttr ?bData]};
         $bSet := $qFn($DBa, $DBb);
         $eFn := express(){{'name': key(?name), 'bData': ?bData}};
         $reduce($bSet, $eFn) )"
       #{{"name" "Alice", "bData" "Alice-B-data"},
         {"name" "Bob",   "bData" "Bob-B-data"}}
       :sets? true))

    (testing "express from the second one above (a :redex/obj)."
      (run-test
       "($DBa   := [{'email' : 'bob@example.com', 'name' : 'Bob'},
                    {'email' : 'alice@alice.org', 'name' : 'Alice'}];
         $DBb   := [{'id' : 'bob@example.com', 'bAttr' : 'Bob-B-data'},
                    {'id' : 'alice@alice.org', 'bAttr' : 'Alice-B-data'}];
         $qFn   :=  query(){[$DBa ?e1 :email ?id] [$DBb ?e2 :id ?id] [$DBa ?e1 :name ?name] [$DBb ?e2 :bAttr ?bData]};
         $bSets := $qFn($DBa, $DBb);
         $eFn   := express{{?name : {'bData' : ?bData}}};
         $reduce($bSets, $eFn) )"
       {"Alice" {"bData" "Alice-B-data"},
        "Bob"   {"bData" "Bob-B-data"}}))


    (testing "like above but with deeper data."
      (run-test
       "($DBa   := [{'email' : 'bob@example.com', 'name' : 'Bob'},
                    {'email' : 'alice@alice.org', 'name' : 'Alice'}];
         $DBb   := [{'id' : 'bob@example.com', 'bAttr' : {'b1' : 'Bob-B1-data',   'b2' : 'Bob-B2-data'  }},
                    {'id' : 'alice@alice.org', 'bAttr' : {'b1' : 'Alice-B1-data', 'b2' : 'Alice-B2-data'}}];
         $qFn   :=  query{[$DBa ?e1 :email ?id] [$DBb ?e2 :id ?id] [$DBa ?e1 :name ?name] [$DBb ?e2 :bAttr ?e3] [$DBb ?e3 :b1 ?b1] [$DBb ?e3 :b2 ?b2]};
         $bSets := $qFn($DBa, $DBb);
         $eFn   := express{{?name : {'bData' : {'b1': ?b1, 'b2': ?b2}}}};
         $reduce($bSets, $eFn) )"
       [{"Bob"   {"bData" {"b1" "Bob-B1-data",   "b2" "Bob-B2-data"}}}
        {"Alice" {"bData" {"b1" "Alice-B1-data", "b2" "Alice-B2-data"}}}])))

(defn ident-code
  "Returns text for execution of a $qIdent/$eIdent problem when provided with a data (which will be pprinted)."
  [data]
  (cl-format nil
             " ( $data := ~A;
   $qFn   := query{$qIdent($data)};
   $bSets := $qFn($data);
   $eFn   := express{$eIdent($data)};
   $reduce($bSets, $eFn) )"
             (bi/pprint-obj data)))

(deftest query-identity
  (testing "Testing $qIdent"
    (testing "Testing generation of the query forms"
    (is (= '[[?e1 :id ?v1] [?e1 :aAttr ?e2] [?e2 :aval ?v2] [?e2 :cval ?e3] [?e3 :cc-val ?v3] [?e1 :bAttr ?e4] [?e4 :bval ?v4]]
           (bi/processRM :ptag/exp
                         "$qIdent({'id' : [123, 456],
                                   'aAttr' : {'aval'  : 'A-value',
                                              'cval' : {'cc-val': 'C-value'}},
                                   'bAttr' : {'bval' : 'B-value'}})"
                         {:execute? true}))))

    (testing "qIdent used in a query"
      (run-test
       "( $data  := {'id' : 123, 'aAttr' : {'val' : 'A-value'}};
          $qFn   := query{$qIdent($data)};
         $qFn($data) )"
       '[{?v1 123, ?v2 "A-value"}]))

    (testing "qIdent used in a query and express"
      (run-test
       (ident-code {"id" 123 "aAttr" {"val" "A-value"}})
       {"id" 123, "aAttr" {"val" "A-value"}}))))

(deftest reduce-bodies
  (testing "Testing reduce bodies."
    (testing "simple object; it is just a vector of it kv pairs."
      (is (= '#:redex{:more [#:redex{:user-key ?v1, :?v1--?v1 [:redex/express-key ?v1], :val "val1"}
                             #:redex{:user-key ?v2, :?v2--?v2 [:redex/express-key ?v2], :val "val2"}]}
             (-> (qu/schematic-express-body '{?v1 "val1" ?v2 "val2"}) :reduce-body))))

    (testing "simple nested obj"
      (is (= '#:redex{:user-key ?v1,
                      :?v1--?v1 [:redex/express-key ?v1],
                      :obj [#:redex{:user-key "value", :value--?v1|value [:redex/express-key ?v1 "value"], :val ?v2}]}
             (-> (qu/schematic-express-body '{?v1 {"value" ?v2}}) :reduce-body))))

    (testing "keyed obj, for example, {'owner/id' : key(?owner), 'owner/name' : ?name}; base-body has a e-k in it."
      (is (= '{:owner/id [:redex/express-key ?owner],
               :redex/user-key "owner/id",
               :redex/ek-val ?owner,
               :redex/more [#:redex{:owner*name--?owner|owner*name [:redex/express-key ?owner "owner/name"], :user-key "owner/name", :val ?name}]}
             (-> (qu/schematic-express-body '{"owner/id" [:redex/express-key ?owner], "owner/name" ?name}) :reduce-body))))

    (testing "nested obj"
      (is (= '#:redex{:user-key ?systemName,
                      :?systemName--?systemName [:redex/express-key ?systemName],
                      :obj
                      [#:redex{:user-key ?deviceName,
                               :?deviceName--?systemName|?deviceName [:redex/express-key ?systemName ?deviceName],
                               :obj
                               [#:redex{:user-key "id", :id--?systemName|?deviceName|id [:redex/express-key ?systemName ?deviceName "id"], :val ?id}]}]}
             (-> (qu/schematic-express-body '{?systemName {?deviceName {"id" ?id}}}) :reduce-body))))))

(deftest redex-idents
  (testing "Testing ability to recover data with reduce/express on identity query and express."
    (testing "Testing simple redex identity"
      (run-test (ident-code {"abc" 123})               {"abc" 123})
      (run-test (ident-code {"abc" 123, "xyz" 456})    {"abc" 123, "xyz" 456})
      #_(run-test (ident-code {"abc" [1 2 3]})           {"abc" [1 2 3]})
      #_(run "( $data := {'abc': [1, 2, 3]};
                  $qFn   := query{[?e1 :abc ?v1]};
                  $bSets := $qFn($data);
                  $eFn   := express{{'abc': ?v1}};
                  $reduce($bSets, $eFn) )")
      #_(run "( $data := {'abc': [1, 2, 3]};
                  $qFn   := query{[?e1 :abc ?v1]};
                  $bSets := $qFn($data);
                  $eFn   := express{{'abc': [?v1]}};
                  $map($bSets, $eFn) )")))) ; <======================== Start here!


;;; datalog allows you to package up sets of :where clauses into named rules.
;;; These rules make query logic reusable, and also composable, meaning that you can bind portions of a query's logic at query time.

;;; A rule is a named group of clauses that can be plugged into the :where section of your query.
;;; For example, here is a rule from the Seattle example dataset that tests whether a community is a twitter feed
(defn tryme []
  (bi/processRM :ptag/exp
                "rule{(twitter? ?c)
                      [?c :community/type :community.type/twitter]}"))
