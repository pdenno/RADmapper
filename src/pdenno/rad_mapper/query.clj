(ns pdenno.rad-mapper.query
  "query, query!, enforce and things related"
  (:require
   [pdenno.owl-db-tools.core      :as owl] ; ToDo: Reference to "plug-ins" is probably temporary.
   [pdenno.owl-db-tools.resolvers :as res]
   [datahike.api                  :as d]
   [datahike.pull-api             :as dp]
   [taoensso.timbre               :as log]))

;;; ToDo: Get somem more types in here.
(defn dh-type-of
  "Return a Datahike schema :db/valueType object for the argument"
  [obj]
  (cond (string? obj)  :db.type/string
        (number? obj)  :db.type/number
        (keyword? obj) :db.type/keyword
        (boolean? obj) :db.type/boolean))

;;; ToDo: If this runs too long, assume you've learned everything and stop it.
(defn learn-schema-walking
  "Return DH schema objects for the data provided."
  [data]
  (let [learned (atom [])]
    (letfn [(update-learned [attr typ card] ; ToDo: Warn on heterogeneous.
            (lsw-aux [obj]
              (cond (map? obj)
                    (or (vector? obj) (set? obj))
                    ,,,)))])))

(defn db-for!
  "Create a database for the argument data and return a connection to it."
  [data db-name]
  (let [db-cfg {:store {:backend :mem :id db-name} :keep-history? false :schema-flexibility :write}]
    (when (d/database-exists? db-cfg) (d/delete-database db-cfg))
    (let [conn-atm (d/connect db-cfg)]
      (d/transact @conn-atm (learn-schema-walking data))
      (d/transact @conn-atm data)
      @conn-atm)))
