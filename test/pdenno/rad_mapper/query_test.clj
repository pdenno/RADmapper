(ns pdenno.rad-mapper.query-test
  (:require
   [clojure.test :refer  [deftest is testing]]
   [pdenno.rad-mapper.query       :as gq]
   [pdenno.owl-db-tools.core      :as owl]
   [pdenno.owl-db-tools.resolvers :as res]
   [datahike.api                  :as d]
   [datahike.pull-api             :as dp]))

(def big-cfg {:store {:backend :file :path "/tmp/datahike-owl-db"}
              :keep-history? false
              :schema-flexibility :write})

(def big-sources
  {"cause"  {:uri "http://www.ontologydesignpatterns.org/ont/dlp/Causality.owl"  :ref-only? true},
   "coll"   {:uri "http://www.ontologydesignpatterns.org/ont/dlp/Collections.owl"},
   "colv"   {:uri "http://www.ontologydesignpatterns.org/ont/dlp/Collectives.owl"},
   "common" {:uri "http://www.ontologydesignpatterns.org/ont/dlp/CommonSenseMapping.owl"},
   "dlp"    {:uri "http://www.ontologydesignpatterns.org/ont/dlp/DLP_397.owl"},
   "dol"    {:uri "http://www.ontologydesignpatterns.org/ont/dlp/DOLCE-Lite.owl"},
   "edns"   {:uri "http://www.ontologydesignpatterns.org/ont/dlp/ExtendedDnS.owl"},
   "fpar"   {:uri "http://www.ontologydesignpatterns.org/ont/dlp/FunctionalParticipation.owl"},
   "info"   {:uri "http://www.ontologydesignpatterns.org/ont/dlp/InformationObjects.owl"},
   "mod"    {:uri "http://www.ontologydesignpatterns.org/ont/dlp/ModalDescriptions.owl"},
   "pla"    {:uri "http://www.ontologydesignpatterns.org/ont/dlp/Plans.owl"},
   "sem"    {:uri "http://www.ontologydesignpatterns.org/ont/dlp/SemioticCommunicationTheory.owl" :ref-only? true},
   "space"  {:uri "http://www.ontologydesignpatterns.org/ont/dlp/SpatialRelations.owl"},
   "soc"    {:uri "http://www.ontologydesignpatterns.org/ont/dlp/SocialUnits.owl"},
   "sys"    {:uri "http://www.ontologydesignpatterns.org/ont/dlp/Systems.owl"},
   "time"   {:uri "http://www.ontologydesignpatterns.org/ont/dlp/TemporalRelations.owl"},

   "model"  {:uri "http://modelmeth.nist.gov/modeling",   :access "data/modeling.ttl",   :format :turtle},
   "ops"    {:uri "http://modelmeth.nist.gov/operations", :access "data/operations.ttl", :format :turtle}})

(def big-atm (d/connect big-cfg))

(defn make-big-db [cfg]
  (when (d/database-exists? cfg) (d/delete-database cfg))
  (alter-var-root (var big-atm)
                  (fn [_]
                    (owl/create-db!
                     cfg
                     big-sources
                     :rebuild? true
                     :check-sites ["http://ontologydesignpatterns.org/wiki/Main_Page"]))))

(def conn @big-atm)
(alter-var-root (var owl/*conn*) (fn [_] conn))

;;;====================================================================================== NYI
(def foo (res/pull-resource :dol/spatio-temporal-particular @big-atm))

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

(def owl-db
  "Arrange to pull data from the database at big-atm"
  (res/register-resolvers! owl/*conn*))

(def diag (atom nil))

;;; We are assuming that no database exists and we have this data to map.
(def test-data
  "Get maps of everything in the DOLCE (dol) namespace"
  (let [all-objs (->> (mapv #(res/pull-resource % owl/*conn*)
                            (:ontology/context
                             (owl-db '[(:ontology/context {:filter-by {:attr :resource/namespace :val "dol"}})])))
                      ;; Clean them up: domain and range are just singletons here. :rdfs/subClassOf is heterogeneous
                      (mapv #(if (:rdfs/domain %) (update % :rdfs/domain first) %))
                      (mapv #(if (:rdfs/range  %) (update % :rdfs/range  first) %))
                      (mapv #(dissoc % :rdfs/subClassOf :owl/equivalentClass)))
        used-class? (->> all-objs (filter :rdfs/domain) (map :rdfs/domain) set)] ; Things used as domain are also used as range.
    (into (filterv #(used-class? (:resource/iri %)) all-objs)
          (filterv #(= :owl/ObjectProperty (:rdf/type %)) all-objs))))

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
(defn tryme [conn]
  (d/q '[:find ?class ?class-iri ?class-ns ?class-name ?rel ?rel-name ?rel-range
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
       conn))
