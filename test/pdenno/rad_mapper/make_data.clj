(ns pdenno.rad-mapper.make-data
  "This is some stuff useful for making data with owl-db-tools"
  (:require
   [clojure.pprint :refer [pprint]]
   [pdenno.owl-db-tools.core      :as owlc]
   [pdenno.owl-db-tools.resolvers :as owlr]
   [datahike.api                  :as d]))

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
                    (owlc/create-db!
                     cfg
                     big-sources
                     :rebuild? true
                     :check-sites ["http://ontologydesignpatterns.org/wiki/Main_Page"]))))

(def conn @big-atm)
(alter-var-root (var owlc/*conn*) (fn [_] conn))

(def owl-db
  "Arrange to pull data from the database at big-atm"
  (owlr/register-resolvers! owlc/*conn*))

(defn write-test-data
  "Add a DOLCE ontology test to ./tests."
  []
  (let [all-objs (->> (mapv #(owlr/pull-resource % owlc/*conn*)
                            (:ontology/context
                             (owl-db '[(:ontology/context {:filter-by {:attr :resource/namespace :val "dol"}})])))
                      ;; Clean them up: domain and range are just singletons here. :rdfs/subClassOf is heterogeneous
                      (mapv #(if (:rdfs/domain %) (update % :rdfs/domain first) %))
                      (mapv #(if (:rdfs/range  %) (update % :rdfs/range  first) %)))
        used-class? (->> all-objs (filter :rdfs/domain) (map :rdfs/domain) set)] ; Things used as domain are also used as range.
    (spit "data/testing/dolce-1.edn"
          (with-out-str
            (println "[")
            (doseq [obj (into (filterv #(used-class? (:resource/iri %)) all-objs)
                            (filterv #(= :owl/ObjectProperty (:rdf/type %)) all-objs))]
              (println "\n")
              (pprint obj))
            (println "]")))))
