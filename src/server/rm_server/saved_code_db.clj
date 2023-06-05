(ns rm-server.saved-code-db
  (:require
   [clojure.java.io     :as io]
   [datahike.api        :as d]
   [datahike.pull-api   :as dp]
   [mount.core          :as mount :refer [defstate]]
   [taoensso.timbre     :as log]))

(def db-cfg-atm "Configuration map used for connecting to the db. It is set in core."  (atom nil))

(defn connect-atm
  "Set the var rad-mapper.db-util/conn by doing a d/connect.
   Return a connection atom."
  []
  (when-let [db-cfg @db-cfg-atm]
    (if (d/database-exists? db-cfg)
      (d/connect db-cfg)
      (log/warn "There is no DB to connect to."))))

(def saved-code-entries
  [[:db/add -1 :code/id (java.util.UUID/fromString "7b4f5d46-e09f-4cd9-b3f4-ef5803da2ae4")]
   {:code/id    #uuid "7b4f5d46-e09f-4cd9-b3f4-ef5803da2ae4"
    :code/date  #inst "2023-03-06T19:11:34.799-00:00"
    :code/code  "[[1,2,3], 4].$[0][0] /* Take first of each element (twice) */"}])

(def db-schema
  "Defines the datahike schema for this database.
     :db/db.cardinality=many means value is a vector of values of some :db.type."
  [#:db{:cardinality :db.cardinality/one,  :valueType :db.type/uuid,    :ident :code/id    :unique :db.unique/identity}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/instant, :ident :code/date}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :code/code}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :code/data}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :code/title
        :doc "Used in saved-code provided by the exerciser"}])

(defn get-db-atm
  "Do a d/connect to the database, returning a connection atom."
  []
  (if (d/database-exists? @db-cfg-atm)
    (d/connect @db-cfg-atm)
    (log/warn "There is no saved-code DB to connect to.")))

(defn create-db!
  "Create the database if :rebuild? is true, otherwise just set the connection atom, conn."
  []
  (when (:rebuild-db? @db-cfg-atm)
    (when (d/database-exists? @db-cfg-atm) (d/delete-database @db-cfg-atm))
    (d/create-database @db-cfg-atm)
    (let [conn (d/connect @db-cfg-atm)]
      (d/transact conn db-schema)
      (d/transact conn saved-code-entries)
      (log/info "Created schema DB " conn)
      conn)))

(defn store-code
  "Store the argument saved-code and return a UUID."
  [{:keys [code data]}]
  (let [uuid (java.util.UUID/randomUUID)
        obj (cond-> {:code/id uuid
                     :code/date (new java.util.Date)
                     :code/code code}
              data (assoc :code/data data))]
    (d/transact (get-db-atm) [[:db/add -1 :code/id uuid]])
    (d/transact (get-db-atm) [obj])
    (log/info "Stored saved-code " uuid)
    uuid))

(defn get-code
  "Retrieve an saved-code from the saved-code DB by its id"
  [{:keys [id]}]
  (dp/pull @(get-db-atm) '[*] [:code/id (java.util.UUID/fromString id)]))


(def base-dir
  (or (-> (System/getenv) (get "RM_MESSAGING"))
      (throw (ex-info (str "Set the environment variable RM_DATABASES to the directory in which you want databases written."
                           "\nCreate a directory 'schema' under it.") {}))))

(def db-dir
    (if (-> base-dir (str "/databases/saved-code") io/file .isDirectory)
      (str base-dir "/databases/saved-code")
      (throw (ex-info "Directory not found:" {:dir (str base-dir "/databases/saved-code")}))))

(defn init-db
  "Reset and return the atom used to connect to the db."
  []
  (reset! db-cfg-atm {:store {:backend :file :path db-dir}
                      :rebuild-db? false ; <=======================
                      :schema-flexibility :write})
  (connect-atm))

(defstate save-code-db-atm
  :start (init-db))
