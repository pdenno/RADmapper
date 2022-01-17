(ns pdenno.rad-mapper.graph-test
   (:require
    ;[com.wsscode.pathom3.cache :as p.cache]
    [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
    [com.wsscode.pathom3.connect.built-in.plugins :as pbip]
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
    [edn-query-language.core :as eql] ; Nice for experimentation
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
;;;============================== Experimentation with Pathom3 and Learning =============================
;;; (resource-by-db-id {:db/id 629}) ; ==> The whole :info/mapped-to entity. Not currently used.
(pco/defresolver resource-by-db-id [{:db/keys [id]}]
  {:app/resource (dp/pull conn '[*] id)})

;;; (uri-to-db-id {:resource/rdf-uri :info/mapped-to}) ; ==> #:db{:id 629} Not currently used.
(pco/defresolver uri-to-db-id [{:resource/keys [rdf-uri]}]
  {::pco/output [:db/id]}
  (dp/pull conn [:db/id] [:resource/id rdf-uri]))

;;; (resource-by-uri {:resource/rdf-uri :info/mapped-to}) ; ==> The whole :info/mapped-to entity.
(pco/defresolver resource-by-uri [{:resource/keys [rdf-uri]}]
  {:resource/body (dp/pull conn '[*] [:resource/id rdf-uri])}) ; lookup (ident style) of pull.

;;; I conclude that I can't do what I want without PARAMETERS!
;;; (p.eql/process index [{[:resource/rdf-uri :info/mapped-to] ['(:resource/attr {:attr/name :rdfs/domain})]}])
;;; (p.eql/process index [{[:resource/rdf-uri :info/mapped-to] ['(:resource/attr {:attr/name :rdfs/comment})]}])
(pco/defresolver resource-attr  [env {:resource/keys [body]}]
  {::pco/output [{:resource/attr [:attr/body :attr/name]}]} ; attr/name will be provided as a PARAMETER.
  {:resource/attr
   (letfn [(subobj [x]
             (cond (and (map? x) (contains? x :resource/id)) (:resource/id x),          ; It is a whole resource, return ref.
                   (and (map? x) (contains? x :db/id) (== (count x) 1))                 ; It is an object ref...
                   (or (and (map? x)
                            (contains? x :db/id)
                            (d/q `[:find ?id . :where [~(:db/id x) :resource/id ?id]] conn)) ; ...return keyword if it is a resource...
                       (subobj (dp/pull conn '[*] (:db/id x)))),                             ; ...otherwise it is some other structure.
                   (map? x) (reduce-kv (fn [m k v] (assoc m k (subobj v))) {} x),
                   (vector? x) (mapv subobj x),
                   :else x))]
     (when-let [attr (get (pco/params env) :attr/name)] ; Retrieve parameter.
       {:attr/body (subobj (get body attr))}))})

(def index (pci/register [resource-by-uri resource-attr]))

#_(pco/defresolver object-property-domain [{:resource/keys [id]}]
  {:rdfs/domain
   (when-let [domain-obj (-> (dp/pull conn [:rdfs/domain] id) :rdfs/domain first :db/id)] ; LEARN!
      (println "domain-ob" domain-obj)
      (:resource/id (dp/pull conn [:resource/id] domain-obj)))})

#_(def indexes ; sometimes called env.
  (pci/register [object-property-domain
                 resource-db-id
                 resource-subclass-of
                 resource-type ]))

#_(def smart-map (psm/smart-map indexes {:resource/id :dol/perdurant}))

#_(:resource/id smart-map)       ; ==> :dol/perdurant
#_(:rdfs/subClassOf smart-map)   ; ==> ....Good!
#_(:rdf/type smart-map)          ; ==> :owl/Class

;;; Another way to do it: p.eql/process
;;; The body could here could be much more, as it is with the cold? example
#_(pco/defresolver subclass-of? [{:rdfs/keys [subClassOf]}]
  {:subclass-of? subClassOf})

;;;=================================================================================================
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
