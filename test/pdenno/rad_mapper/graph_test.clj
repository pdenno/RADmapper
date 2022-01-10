 (ns pdenno.rad-mapper.graph-test
   (:require
    ;[com.wsscode.pathom3.cache :as p.cache]
    ;[com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
    ;[com.wsscode.pathom3.connect.built-in.plugins :as pbip]
    ;[com.wsscode.pathom3.connect.foreign :as pcf]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.connect.operation :as pco]
    ;[com.wsscode.pathom3.connect.operation.transit :as pcot]
    ;[com.wsscode.pathom3.connect.planner :as pcp]
    ;[com.wsscode.pathom3.connect.runner :as pcr]
    ;[com.wsscode.pathom3.error :as p.error]
    ;[com.wsscode.pathom3.format.eql :as pf.eql]
    ;[com.wsscode.pathom3.interface.async.eql :as p.a.eql]
    [com.wsscode.pathom3.interface.eql :as p.eql]
    [com.wsscode.pathom3.interface.smart-map :as psm]
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

;;;============================== Experimentation with Pathom3 and Learning =============================
(alter-var-root (var big-atm) (fn [_] (d/connect big-cfg)))

(pco/defresolver resource-subclass-of [{:resource/keys [id]}]
  {:rdfs/subClassOf (get (owl/pull-resource id @big-atm) :rdfs/subClassOf)})

;;; For the real implementation, this is cheating. Instead:
;;;   (1) Pull in the code in pull-resource that does this, and
;;;   (2) See if you can generate that code by learning from the data.  <====================
(pco/defresolver resource-type [{:resource/keys [id]}]
  {:rdf/type (get (owl/pull-resource id @big-atm) :rdf/type)})

(def indexes
  (pci/register [resource-subclass-of resource-type]))

(def smart-map (psm/smart-map indexes {:resource/id :dol/perdurant}))

(:resource/id smart-map)       ; ==> :dol/perdurant
(:rdfs/subClassOf smart-map)   ; ==> ....Good!
(:rdf/type smart-map)          ; ==> :owl/Class

;;; Another way to do it: p.eql/process
;;; The body could here could be much more, as it is with the cold? example
(pco/defresolver subclass-of? [{:rdfs/keys [subClassOf]}]
  {:subclass-of? subClassOf})

(def indexes2
  (pci/register [resource-subclass-of subclass-of?]))

(p.eql/process
  indexes2
  {:resource/id :dol/spatio-temporal-particular #_:dol/perdurant}
  [:subclass-of?])  ; ==> ....Good! (both of them).

;;; (db-keys @big-atm) ==> [:source/long-name :source/short-name :resource/id]
(defn db-keys [conn]
  (d/q '[:find [?id ...] :where
         [?e :db/unique :db.unique/identity]
         [?e :db/ident ?id]]
       conn))

;;; (pull-rand :resource/id 4 @big-atm)
(defn pull-rand
  "dp/pull randomly at most CNT objects of where :db/ident is DB-IDENT."
  [db-attr cnt conn]
  (let [ents (-> (d/q `[:find [(~'rand ~cnt ?e)] :where [?e ~db-attr ?id]] conn))]
    (dp/pull-many conn '[*] (flatten ents))))

;;; (tryme :rdf/type @big-atm)
(defn tryme [focus-attr conn]
  )
  

;;;====================================================================================== NYI
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
         
