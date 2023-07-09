(ns rm-server.resolvers
  "Resolvers for use on the schema db (created with a different system)."
  (:require
   [clojure.java.io :as io]
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   [datahike.api        :as d]
   [datahike.pull-api   :as dp]
   [mount.core          :as mount :refer [defstate]]
   [rad-mapper.util     :as util :refer  [resolve-db-id]]
   [rm-server.sutil     :as sutil :refer [register-db connect-atm]]
   [taoensso.timbre     :as log]))

;;; I think, generally speaking, this has to be recompiled and (user/restart) to catch resolver updates.
;;; It is okay if the output doesn't contain all of the :pco/output (try adding :fn/DNE)
;;; It is okay if the query contains less than what is in :pco/output
;;; It is NOT okay that the query contains something that is not returned; See codelib resolvers for a way around this.
;;; A new introduced in Pathom3 is the abiltiy to call the resolvers as functions.
;;;   For example, (schema-name->schema-object {:schema/name "urn:oagis-10.8.4:Nouns:Quote"}) ==> #:db{:id 36505}
;;;                (codelib-fn-name->codelib-id {:library/fn "addOne"}) ==> #:db{:id 4}

;;; ToDo: Get rid of this and 'connect-atm'. resolvers.clj is for any DB (so far schema and codelib).
(def db-cfg-atm "Configuration map used for connecting to the db. It is set in core."  (atom nil))

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
                    @(connect-atm :schema) sdo)]
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
                       @(connect-atm :schema) schema-name)]
    (cond-> (dp/pull @(connect-atm :schema) '[*] ent)
      resolve? (resolve-db-id (connect-atm :schema) filter-set))))

;;; ====================== Schema-db resolvers ========================================================
;;; https://pathom3.wsscode.com/docs/resolvers/ :
;;; "So resolvers establish edges in the Pathom graph, allowing Pathom to traverse from some source data to some target data."
;;; [I don't think that the requires that the Pathom graph and the database have similar connectivity. You can do whatever
;;;  you need to get the data. See my comment in the function pathom-resolve.]
;;;
;;; Resolvers are functions and can be called with ONE ARG, the IDENT:
;;; For example (list-id->list-content {:list_id :ccts_messageSchema})


;;; Note that only if the ident-val has a underscore in it does this convert to keyword.
;;; That is to allow idents like [:library/fn "addOne"].
;;; (res/list-id->list-content {:list_id :cct_messageSchema})
(pco/defresolver list-id->list-content
  "This resolver returns lists."
  [{:keys [list_id]}]
  {::pco/output [:list_content]}
  (log/info "id = " list_id)
  (let [schema-types (d/q '[:find [?type ...] :where [_ :schema_type  ?type]] @(connect-atm :schema))]
    (cond  (= "lists" list_id)      {:list_content (into ["library_fn"] schema-types)}
           (= "library_fn" list_id) {:list_content (d/q '[:find [?name ...] :where [?ent :fn_name ?name]] @(connect-atm :codelib))}
           :else (let [schema-type? (set schema-types)
                       res (cond (schema-type? list_id)
                                 (->>
                                  (d/q '[:find ?name
                                         :in $ ?schema-type
                                         :keys schema_name
                                         :where
                                         [?ent :schema_name  ?name]
                                         [?ent :schema_type  ?schema-type]]
                                       @(connect-atm :schema) (keyword list_id))
                                  (map :schema_name)
                                  sort
                                  vec
                                  not-empty))]
                   {:list_content res}))))

;;; (pathom-resolve {:schema_name "urn:oagis-10.8.4:Nouns:Quote"} [:db/id])
;;; (schema-name->schema-object {:schema_name "urn:oagis-10.8.4:Nouns:Quote"})
(pco/defresolver schema-name->schema-object
  "Return the db/id for a whole schema-object.
   Subsequent queries (e.g. schema-objec->schema-props) can pull from this whatever else is needed."
  [{:keys [schema_name]}]
  {::pco/output [:db/id]}
  (let [res (d/q '[:find ?ent .
                   :in $ ?schema-name
                   :where [?ent :schema_name ?schema-name]]
                 @(connect-atm :schema) schema_name)]
    (if res
      {:db/id res}
      (throw (ex-info "No such schema: " {:schema_name schema_name})))))

;;; (pathom-resolve {:schema_name "urn:oagis-10.8.4:Nouns:Quote"} [:schema_content])
;;; (schema-object->schema-props #:db{:id 36517})
(pco/defresolver schema-object->schema-props
 "Returns various things given a schema db/id."
 [{:db/keys [id]}]
  {:pco/output [:schema_content]}
  {:schema_content
   (-> {:db/id id}
       (resolve-db-id (connect-atm :schema) #{:db/id})
       :schema_content)})

;;; ====================== Codelib resolvers  ========================================================
;;; (pathom-resolve {:library_fn "addOne"} [:db/id])
(pco/defresolver codelib-fn-name->codelib-id
  "Return the db/id for a codelib object. Subsequent queries can pull the properties."
  [{:keys [library_fn]}]
  {::pco/output [:db/id]}
  {:db/id (d/q '[:find ?ent .
                 :in $ ?fn-name
                 :where [?ent :fn_name ?fn-name]]
               @(connect-atm :codelib) library_fn)})

;;; (pathom-resolve {:library_fn "addOne"} [:fn_name :fn_doc :fn_src])
;;; It is okay if the output doesn't contain all of the :pco/output (try adding :fn/DNE)
;;; It is okay if the query contains less than what is in :pco/output
;;; It is NOT okay that the query contains something that is not returned.
(pco/defresolver codelib-id->fn-props
  "Return properties for a codelib object"
  [{:db/keys [id]}]
  {::pco/output [:fn_name :fn_src :fn_doc]}
  (-> {:db/id id}
      (resolve-db-id (connect-atm :codelib) #{:db/id})))

(pco/defresolver codelib-id->extra
  "Return a placeholder for the :fn/exe property."
  [{:db/keys [id]}]
  {::pco/output [:fn_exe]}
  {:fn_exe :resolvers/replace-me})

;;; ====================== Miscellaneous resolvers ========================================================
;;; (pathom-resolve {:db_name "schema_db"} [:db_connection])
(pco/defresolver db-connection
  "Return a keyword designating the schema database for use by clients."
  [{:db/keys [name]}]
  {:pco/out [:db/connection]}
  {:db/connection :_rm/schema-db}) ; Not :db_connection, I think. They'll never see it.

;;; Nice thing about pathom (relative to GraphQL) is that you don't have to start at the root.
;;; This has nothing to do with ::pco/input; you can add it to a query anywhere.
;;; (pathom-resolve {} [:server/time])
(pco/defresolver current-system-time [_ _]
  {::pco/output [:server/time]}
  {:server/time (java.util.Date.)})

(def indexes (atom nil))

(def type-typ?
  "These are DB types for which the ident-value is a keyword."
 #{"cct_bie"
   "cct_componentSchema"
   "cct_messageSchema"
   "generic_codeListSchema"
   "generic_librarySchema"
   "generic_messageSchema"
   "generic_qualifiedDtypeSchema"
   "generic_unqualifiedDtypeSchema"
   "generic_xsdFile"
   "niem_codeListSchema"
   "niem_domainSchema"})

(defn adjust-ident-map
  "$get sends pathom-resolve parameters untouched, but we've tried to maintain a discipline where
   a value which identifies a type is represented as a keyword, whereas a value that identifies
   an individual is represented as a string. Since $get, gets both with strings, the strings
   representing types need to be converted to a keyword."
  [ident-map]
  (if (= :list_id (-> ident-map keys first))
    (cond (empty? ident-map)                  ident-map
          (-> ident-map vals first type-typ?) (update-vals ident-map keyword)
          :else                               ident-map)
    ident-map))

;;; (pathom-resolve {:list/id :ccts_messageSchema} [:schema_name])
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
  (log/info "Pathom3 resolve: ident-map = " ident-map " outputs= " outputs)
  (try (let [ident-map (adjust-ident-map ident-map)
             res (p.eql/process @indexes ident-map outputs)
             #_#_debug-str (str res) ; ToDo this is wasteful and should go away.
             #_#_len (count debug-str)
             #_#_es-str (subs debug-str 0 (min 40 len))]
         #_(log/info "Pathom3 returns:" res-str)
         res)
       (catch Exception e
         (throw (ex-info "pathom-resolve: " {:msg (.getMessage e)})))))

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
                      :keep-history? false ; Not clear that I'd have to respecify this, right?
                      :schema-flexibility :write})
  (register-db :schema @db-cfg-atm)
  @db-cfg-atm)

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

(defstate schema-cfg
  :start (init-db))

(defstate resolvers
  :start (init-resolvers))
