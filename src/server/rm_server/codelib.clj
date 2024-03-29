(ns rm-server.codelib
  "Manage a library of RADmapper code."
  (:require
   [clojure.java.io     :as io]
   [datahike.api        :as d]
   [mount.core          :as mount :refer [defstate]]
   [rm-server.sutil     :refer [register-db]]
   [rm-server.libcode   :refer [library-code]]
   [taoensso.timbre     :as log]))

(def db-cfg-atm "Configuration map used for connecting to the db. It is set in core."  (atom nil))

(def db-schema
  "Defines the datahike schema for this database.
     :db/db.cardinality=many means value is a vector of values of some :db.type."
  [#:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :ident :fn_name :unique :db.unique/identity}
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :ident :fn_src}
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :ident :fn_doc}])

;;; ToDo: Don't really need these. See stbd.db.
(def base-dir "The base directory of the databases. Can't be set at compile time in Docker." nil)
(def db-dir "The directory containing schema DBs. Can't be set at compile time in Docker." nil)
(def rebuild-db? "Don't keep this on the db-cfg map." true)

(defn create-db!
  "Create the database if :rebuild? is true, otherwise return nil."
  []
  (when rebuild-db?
    (when (d/database-exists? @db-cfg-atm) (d/delete-database @db-cfg-atm))
    (d/create-database @db-cfg-atm)
    (let [conn (d/connect @db-cfg-atm)]
      (d/transact conn db-schema)
      (d/transact conn library-code)
      (log/info "Created codelib DB " conn)
      conn)))

(defn init-db
  "Reset and return the atom used to connect to the db."
  []
  (alter-var-root
   (var base-dir)
   (fn [_]
     (or (-> (System/getenv) (get "RM_MESSAGING"))
         (throw (ex-info (str "Set the environment variable RM_MESSAGING to the directory containing RADmapper databases."
                              "\nCreate a directory 'saved-code' under it.") {})))))
  (alter-var-root
   (var db-dir)
   (fn [_]
     (if (-> base-dir (str "/databases/code-lib") io/file .isDirectory)
       (str base-dir "/databases/code-lib")
       (throw (ex-info "Directory not found:" {:dir (str base-dir "/databases/code-lib")})))))
  (reset! db-cfg-atm {:store {:backend :file :path db-dir}
                      :keep-history? false ; <=================== Debatable.
                      :schema-flexibility :write})
  (create-db!)
  (register-db :codelib @db-cfg-atm)
  @db-cfg-atm)

(defstate codelib-cfg
  :start (init-db))
