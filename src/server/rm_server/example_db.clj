(ns rm-server.example-db
  (:require
   [datahike.api        :as d]
   [datahike.pull-api   :as dp]
   [taoensso.timbre     :as log]))

(def db-cfg {:store {:backend :file :path "resources/databases/examples"}
             :rebuild-db? true
             :schema-flexibility :write})

(def example-entries
  [[:db/add -1 :examp/id (java.util.UUID/fromString "7b4f5d46-e09f-4cd9-b3f4-ef5803da2ae4")]
   {:examp/id    #uuid "7b4f5d46-e09f-4cd9-b3f4-ef5803da2ae4"
    :examp/date  #inst "2023-03-06T19:11:34.799-00:00"
    :examp/code  "[[1,2,3], 4].$[0][0] /* Take first of each element (twice) */"}])

(def db-schema
  "Defines the datahike schema for this database.
     :db/db.cardinality=many means value is a vector of values of some :db.type."
  [#:db{:cardinality :db.cardinality/one,  :valueType :db.type/uuid,    :ident :examp/id    :unique :db.unique/identity}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/instant, :ident :examp/date}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :examp/code}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :examp/data}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :examp/title
        :doc "Used in examples provided by the exerciser"}])

(defn get-db-atm
  "Do a d/connect to the database, returning a connection atom."
  []
  (if (d/database-exists? db-cfg)
    (d/connect db-cfg)
    (log/warn "There is no example DB to connect to.")))

(defn create-db!
  "Create the database if :rebuild? is true, otherwise just set the connection atom, conn."
  []
  (when (:rebuild-db? db-cfg)
    (when (d/database-exists? db-cfg) (d/delete-database db-cfg))
    (d/create-database db-cfg)
    (let [conn (d/connect db-cfg)]
      (d/transact conn db-schema)
      (d/transact conn example-entries)
      (log/info "Created schema DB " conn)
      conn)))

(defn store-example
  "Store the argument example and return a UUID."
  [{:keys [code data]}]
  (let [uuid (java.util.UUID/randomUUID)
        obj (cond-> {:examp/id uuid
                     :examp/date (new java.util.Date)
                     :examp/code code}
              data (assoc :examp/data data))]
    (d/transact (get-db-atm) [[:db/add -1 :examp/id uuid]])
    (d/transact (get-db-atm) [obj])
    (log/info "Stored example " uuid)
    uuid))

(defn get-example
  "Retrieve an example from the examples DB by its id"
  [{:keys [id]}]
  (dp/pull @(get-db-atm) '[*] [:examp/id (java.util.UUID/fromString id)]))
