(ns rad-mapper.resolvers
  "Resolvers for use on the schema db (created with a different system)."
  (:require
   [clojure.java.io :as io]
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   [datahike.api        :as d]
   [datahike.pull-api   :as dp]
   [mount.core :as mount :refer [defstate]]
   [rad-mapper.util     :as util :refer [resolve-db-id]]
   [taoensso.timbre     :as log]))

;;; I think generally speaking this has to be recompiled and (user/restart) to catch resolver updates.

(def db-cfg-atm "Configuration map used for connecting to the db. It is set in core."  (atom nil))

(defn connect-atm
  "Set the var rad-mapper.db-util/conn by doing a d/connect.
   Return a connection atom."
  []
  (when-let [db-cfg @db-cfg-atm]
    (if (d/database-exists? db-cfg)
      (d/connect db-cfg)
      (log/warn "There is no DB to connect to."))))

;;;=========================== Schema Operations (useful by themselves and in resolvers) ==================
;;; (list-schemas :sdo :oagi)
(defn list-schemas
  "Return a list of schema, by default they are sorted.
   Attrs should be a list of attributes to return"
  [& {:keys [sdo sort? _attrs] :or {sort? true sdo '_}}] ; ToDo: attrs
  (let [result (d/q '[:find ?n ?sdo
                      :keys schema/name schema/sdo
                      :in $ ?sdo
                      :where
                      [?s :schema/name ?n]
                      [?s :schema/sdo ?sdo]]
                    @(connect-atm) sdo)]
    (cond->> result
      true (map :schema/name)
      sort? sort
      true vec)))

;;; (get-schema "urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_1")
(defn get-schema
  "Return the map stored in the database for the given schema-urn. Useful in development.
    :filter-set - DB attribute to leave out (e.g. #{:db/id} or #{:db/doc-string}) "
  [schema-name & {:keys [resolve? filter-set] :or {resolve? true filter-set #{:db/id}}}]
  (when-let [ent  (d/q '[:find ?ent .
                         :in $ ?schema-name
                         :where [?ent :schema/name ?schema-name]]
                       @(connect-atm) schema-name)]
    (cond-> (dp/pull @(connect-atm) '[*] ent)
      resolve? (resolve-db-id (connect-atm) filter-set))))

;;;============================ Resolvers  ==================================
;;; (pathom-resolve {:list/id :ccts/message-schema} [:schema/name])
(pco/defresolver list-id->schema-names
  "This resolver returns schema/names (which are db/unique db.unique/identity)
   that match the given :schema/type. They might be"
  [_env {:list/keys [id]}]
  {::pco/output [:schema/name]}
  (when-let [schema-maps (->>
                          (d/q '[:find ?name ?topic
                                 :in $ ?schema-type
                                 :keys schema/name schema/topic
                                 :where
                                   [?ent :schema/name  ?name]
                                   [?ent :schema/topic ?topic]
                                   [?ent :schema/type  ?schema-type]]
                                 @(connect-atm) id) ; id is ?schema-type
                            (sort-by :schema/topic)
                            (mapv #(dissoc % :schema/topic))
                            not-empty)]
      {:list/id id
       :list/schemas schema-maps}))

;;; (pathom-resolve {:schema/name "urn:oagis-10.8.4:Nouns:Quote"} [:db/id])
(pco/defresolver schema-name->schema-object
  "Return the db/id for a whole schema-object.
   Subsequent queries (e.g. schema-objec->schema-props) can pull from this whatever else is needed."
  [_env {:schema/keys [name]}]
  {::pco/output [:db/id]}
  (log/info "schema-name->schema-object")
  {:db/id (d/q '[:find ?ent .
                 :in $ ?schema-name
                 :where [?ent :schema/name ?schema-name]]
               @(connect-atm) name)})

;;; This one doesn't work. It can't see the output map because of the let, but I thought that
;;; just having :pco/output as I do would be sufficient. Nope.
;;; (pathom-resolve {:schema/name "urn:oagis-10.8.4:Nouns:Quote"} [:schema/content])
#_(pco/defresolver schema-object->schema-props
 "Returns various things given a schema db/id."
 [_env {:db/keys [id]}]
  {:pco/output [:schema/content #_:schema/spec]}
  (log/info "schema-object->schema-props")
  (let [sobj (resolve-db-id {:db/id id} (connect-atm) #{:db/id})]
    {:schema/content (:schema/content sobj)
     #_#_:schema/spec    (:schema/spec sobj)}))

;;; This one works because it can see the output map.
(pco/defresolver schema-object->schema-props
 "Returns various things given a schema db/id."
 [_env {:db/keys [id]}]
  {:pco/output [:schema/content #_:schema/spec]}
  (log/info "schema-object->schema-props")
  {:schema/content
   (-> {:db/id id}
       (resolve-db-id (connect-atm) #{:db/id})
       :schema/content)})

;;; (pathom-resolve {:db/name :schema/db"} [:db/connection])
(pco/defresolver db-connection
  "Return a keyword designating the schema database for use by clients."
  [_env {:db/keys [name]}]
  {:pco/out [:db/connection]}
  {:db/connection :_rm/schema-db})

;;; Nice thing about pathom (relative to GraphQL) is that you don't have to start at the root.
;;; This has nothing to do with ::pco/input; you can add it to a query anywhere.
;;; (pathom-resolve {} [:server/time])
(pco/defresolver current-system-time [_ _]
  {::pco/output [:server/time]}
  {:server/time (java.util.Date.)})

(def indexes (atom nil))

;;; (pathom-resolve {:list/id :ccts/message-schema} [:schema/name])
(defn pathom-resolve
  "Uses the indexes to respond to a query.
   ident-map: a single-entry map, the key of which names an identity condition,
              such as a db/ident, the value of which is a value for the identity condition.
   outputs: a vector of properties (the :pco/outputs of resolvers) that are sought."
  [ident-map outputs]
  (try (log/info "Pathom resolve: db = " @(connect-atm) " ident-map = " ident-map " outputs= " outputs)
       (catch Exception _e
         (throw (ex-info "schema database has not been initialized." {}))))
  (p.eql/process @indexes ident-map outputs))

;;; ===== Starting and stopping =================================
(def base-dir
  (or (-> (System/getenv) (get "RM_MESSAGING"))
      (throw (ex-info (str "Set the environment variable RM_DATABASES to the directory in which you want databases written."
                           "\nCreate a directory 'schema' under it.") {}))))

(def db-dir
    (if (-> base-dir (str "/databases/schema") io/file .isDirectory)
      (str base-dir "/databases/schema")
      (throw (ex-info "Directory not found:" {:dir (str base-dir "/databases/schema")}))))

(defn init-db
  "Reset and return the atom used to connect to the db."
  []
  (reset! db-cfg-atm {:store {:backend :file :path db-dir}
                      :rebuild-db? false
                      :schema-flexibility :write})
  (connect-atm))

(defn init-resolvers
  []
  (reset! indexes (pci/register [list-id->schema-names
                                 schema-name->schema-object
                                 schema-object->schema-props
                                 db-connection
                                 current-system-time]))
  (log/info "Pathom3 resolvers: " (pathom-resolve {} [:server/time])))

(defstate schema-db-atm
  :start (init-db))

(defstate resolvers
  :start (init-resolvers))
