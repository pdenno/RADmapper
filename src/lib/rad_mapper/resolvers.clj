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
   [rad-mapper.codelib  :refer [codelib-atm]]
   [taoensso.timbre     :as log]))

;;; I think generally speaking this has to be recompiled and (user/restart) to catch resolver updates.
;;; It is okay if the output doesn't contain all of the :pco/output (try adding :fn/DNE)
;;; It is okay if the query contains less than what is in :pco/output
;;; It is NOT okay that the query contains something that is not returned; See codelib resolvers for a way around this.

;;; ToDo: Get rid of this and 'connect-atm'. resolvers.clj is for any DB (so far schema and codelib).
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

;;; ====================== Schema-db resolvers ========================================================
;;; https://pathom3.wsscode.com/docs/resolvers/ :
;;; "So resolvers establish edges in the Pathom graph, allowing Pathom to traverse from some source data to some target data."
;;; [I don't think that the requires that the Pathom graph and the database have similar connectivity. You can do whatever
;;;  you need to get the data. See my comment in the function pathom-resolve.]
;;;
;;; Resolvers are functions and can be called with ONE ARG, the IDENT:
;;; For example (list-id->list-content {:list/id :ccts/message-schema})

;;; Currently this only returns lists of schema, but (changing the :pco/output???) I think it could return lists of anything.
;;; (pathom-resolve {:list/id :ccts/message-schema} [:list/content])
;;; (list-id->list-content {:list/id :ccts/message-schema})
(pco/defresolver list-id->list-content
  "This resolver returns lists."
  [{:keys [list/id]}]
  {::pco/output [:list/content]}
  (let [schema-type? (set (d/q '[:find [?type ...] :where [_ :schema/type  ?type]] @(connect-atm)))
        id (keyword id)
        res (cond (schema-type? id)
              (->>
               (d/q '[:find ?name
                      :in $ ?schema-type
                      :keys schema/name
                      :where
                      [?ent :schema/name  ?name]
                      [?ent :schema/type  ?schema-type]]
                    @(connect-atm) (keyword id))
               (map :schema/name)
               sort
               vec
               not-empty))]
    {:list/content res}))

;;; (pathom-resolve {:schema/name "urn:oagis-10.8.4:Nouns:Quote"} [:db/id])
(pco/defresolver schema-name->schema-object
  "Return the db/id for a whole schema-object.
   Subsequent queries (e.g. schema-objec->schema-props) can pull from this whatever else is needed."
  [{:schema/keys [name]}]
  {::pco/output [:db/id]}
  {:db/id (d/q '[:find ?ent .
                 :in $ ?schema-name
                 :where [?ent :schema/name ?schema-name]]
               @(connect-atm) name)})

;;; This one doesn't work. It can't see the output map because of the let, but I thought that
;;; just having :pco/output as I do would be sufficient. Nope.
;;; (pathom-resolve {:schema/name "urn:oagis-10.8.4:Nouns:Quote"} [:schema/content])
#_(pco/defresolver schema-object->schema-props
 "Returns various things given a schema db/id."
 [{:db/keys [id]}]
  {:pco/output [:schema/content #_:schema/spec]}
  (log/info "schema-object->schema-props")
  (let [sobj (resolve-db-id {:db/id id} (connect-atm) #{:db/id})]
    {:schema/content (:schema/content sobj)
     #_#_:schema/spec    (:schema/spec sobj)}))

;;; This one works because it can see the output map.
(pco/defresolver schema-object->schema-props
 "Returns various things given a schema db/id."
 [{:db/keys [id]}]
  {:pco/output [:schema/content #_:schema/spec]}
  {:schema/content
   (-> {:db/id id}
       (resolve-db-id (connect-atm) #{:db/id})
       :schema/content)})

;;; ====================== Codelib resolvers  ========================================================
(pco/defresolver codelib-fn-name->codelib-id
  "Return the db/id for a codelib object. Subsequent queries can pull the properties."
  [{:library/keys [fn]}]
  {::pco/output [:db/id]}
  {:db/id (d/q '[:find ?ent .
                 :in $ ?fn-name
                 :where [?ent :fn/name ?fn-name]]
               @codelib-atm fn)})

;;; (pathom-resolve {:library/fn 'schemaParentChild'} [:fn/name :fn/doc :fn/src])
;;; It is okay if the output doesn't contain all of the :pco/output (try adding :fn/DNE)
;;; It is okay if the query contains less than what is in :pco/output
;;; It is NOT okay that the query contains something that is not returned.
(pco/defresolver codelib-id->fn-props
  "Return properties for a codelib object"
  [{:db/keys [id]}]
  {::pco/output [:fn/name :fn/src :fn/doc]}
  (-> {:db/id id}
      (resolve-db-id codelib-atm #{:db/id})))

(pco/defresolver codelib-id->extra
  "Return a placeholder for the :fn/exe property."
  [{:db/keys [id]}]
  {::pco/output [:fn/exe]}
  {:fn/exe :resolvers/replace-me})

;;; ====================== Miscellaneous resolvers ========================================================
;;; (pathom-resolve {:db/name :schema/db"} [:db/connection])
(pco/defresolver db-connection
  "Return a keyword designating the schema database for use by clients."
  [{:db/keys [name]}]
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
   ident-map: a single-key map:
                - the key names an identity condition attribute, (e.g. schema/name)
                - the value uniquely identifies an individual using that identity condition attribute.

   IMPORTANT: The notion of uniqueness used in this function is one established for the pathom graph;
   it need not be manifest in a database as unique. For example, {:list/id :ccts-message-schema}
   is a unique list, but there is no notion of list in the database. In contrast,
   {:schema/name 'urn:oagis-10.8.4:Nouns:Quote'} does refer to a :db.unique/identity DB attribute,
   but that is not necessary.

   outputs: a vector of properties (the :pco/outputs of resolvers) that are sought."
  [ident-map outputs]
  (try (log/info "Pathom3 resolve: ident-map = " ident-map " outputs= " outputs)
       (catch Exception _e
         (throw (ex-info "schema database has not been initialized." {}))))
  (p.eql/process @indexes ident-map outputs))

;;; ===== Starting and stopping =================================
(def base-dir "The base directory of the databases. Can't be set at compile time in Docker." nil)
(def db-dir "The directory containing schema DBs. Can't be set at compile time in Docker." nil)

(defn init-db
  "Set directory vars from environment variables.
   Reset and return the atom used to connect to the db."
  []
  (alter-var-root
   (var base-dir)
   (fn [_]
     (or (-> (System/getenv) (get "RM_MESSAGING"))
         (throw (ex-info (str "Set the environment variable RM_MESSAGING to the directory containing RADmapper databases."
                              "\nCreate a directory 'schema' under it.") {})))))
  (alter-var-root
   (var db-dir)
   (fn [_]
     (if (-> base-dir (str "/databases/schema") io/file .isDirectory)
       (str base-dir "/databases/schema")
       (throw (ex-info "Directory not found:" {:dir (str base-dir "/databases/schema")})))))
  (reset! db-cfg-atm {:store {:backend :file :path db-dir}
                      :rebuild-db? false
                      :schema-flexibility :write})
  (connect-atm))

(defn init-resolvers
  []
  (reset! indexes (pci/register [list-id->list-content
                                 schema-name->schema-object
                                 schema-object->schema-props
                                 codelib-fn-name->codelib-id
                                 codelib-id->fn-props
                                 codelib-id->extra
                                 db-connection
                                 current-system-time]))
  (pathom-resolve {} [:server/time])
  @indexes)

(defstate schema-atm
  :start (init-db))

(defstate resolvers
  :start (init-resolvers))
