 (ns pdenno.rad-mapper.graph-test
   (:require
    ;[com.wsscode.pathom3.cache :as p.cache]
    ;[com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
    ;[com.wsscode.pathom3.connect.built-in.plugins :as pbip]
    ;[com.wsscode.pathom3.connect.foreign :as pcf]
    ;[com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.connect.operation :as pco]
    ;[com.wsscode.pathom3.connect.operation.transit :as pcot]
    ;[com.wsscode.pathom3.connect.planner :as pcp]
    ;[com.wsscode.pathom3.connect.runner :as pcr]
    ;[com.wsscode.pathom3.error :as p.error]
    ;[com.wsscode.pathom3.format.eql :as pf.eql]
    ;[com.wsscode.pathom3.interface.async.eql :as p.a.eql]
    ;[com.wsscode.pathom3.interface.eql :as p.eql]
    ;[com.wsscode.pathom3.interface.smart-map :as psm]
    ;[com.wsscode.pathom3.path :as p.path]
    ;[com.wsscode.pathom3.plugin :as p.plugin]
    [pdenno.rad-mapper.graph   :as gr]
    [pdenno.owl-db-tools.core  :as owl]
    [datahike.api              :as d]
    [datahike.pull-api         :as dp]))

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

(def big-cfg {:store {:backend :file :path "/tmp/datahike-owl-db"}
              :keep-history? false
              :schema-flexibility :write})
(def big-atm nil)

(defn make-big-db [cfg]
  (when (d/database-exists? cfg) (d/delete-database cfg))
  (alter-var-root (var big-atm)
                  (fn [_]
                    (owl/create-db!
                     cfg
                     big-sources
                     :rebuild? true
                     :check-sites ["http://ontologydesignpatterns.org/wiki/Main_Page"]))))


(def foo (owl/pull-resource :dol/spatio-temporal-particular @big-atm))

(defn class-order
  "Return the map as a sorted map with nice ordering of keys for humans."
  [m]
  (into (sorted-map-by
         (fn [k1 k2]
           (cond (= k1 :resource/id) -1
                 (= k2 :resource/id) +1
                 (= k1 :rdf/type) -1
                 (= k2 :rdf/type) +1
                 (= k1 :rdfs/subClassOf) -1
                 (= k2 :rdfs/subClassOf) +1
                 (= k1 :owl/comment) +1
                 (= k2 :owl/comment) +1)))
         m))
         
