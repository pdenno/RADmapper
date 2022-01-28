(ns pdenno.rad-mapper.query
  "query, query!, enforce and things related"
  (:require
   [pdenno.owl-db-tools.core      :as owl] ; ToDo: Reference to "plug-ins" is probably temporary.
   [pdenno.owl-db-tools.resolvers :as res]
   [datahike.api                  :as d]
   [datahike.pull-api             :as dp]
   [taoensso.timbre               :as log]))

;;; ToDo: Get some more types in here, and in implementation generally.
(defn dh-type-of
  "Return a Datahike schema :db/valueType object for the argument"
  [obj]
  (cond (string? obj)  :db.type/string
        (number? obj)  :db.type/number
        (keyword? obj) :db.type/keyword
        (map? obj)     :db.type/ref
        (boolean? obj) :db.type/boolean))

(defn sample-vec
  [vec k & {:keys [sample-threshold sample-size]
             :or {sample-threshold 200 sample-size 100}}]
  (let [len (count vec)
        vec (if (< len sample-threshold)
               vec ; ToDo: repeatedly solution less than ideal.
               (repeatedly sample-size #(nth vec (rand-int len))))
        result (-> (map dh-type-of vec) set)]
    (if (> (count result) 1)
      (throw (ex-info  "Heterogeneous types:" {:types result :attribute k :vector vec}))
      (first result))))

(defn learn-schema-walking
  "Return DH schema objects for the data provided."
  [data]
  (let [learned (atom {})]
    (letfn [(update-learned! [k v]
              (let [typ  (-> @learned k :db/valueType)
                    card (-> @learned k :db/cardinality)
                    vec? (vector? v)
                    this-typ  (if vec? (sample-vec v k) (dh-type-of v))
                    this-card (if (or vec? (= card :db.cardinality/many)) ; permissive to many
                                :db.cardinality/many
                                :db.cardinality/one)]
                (if (and typ (not= typ this-typ))
                  (log/warn "Different types:" k)
                  (swap! learned #(-> %
                                      (assoc-in [k :db/cardinality] this-card)
                                      (assoc-in [k :db/valueType] this-typ))))))
            (lsw-aux [obj]
              (cond (map? obj) (doall (map (fn [[k v]]
                                             (update-learned! k v)
                                             (when (coll? v) (lsw-aux v)))
                                           obj))
                    (coll? obj) (doall (map lsw-aux obj))))]
      (lsw-aux data)
      (reduce-kv (fn [res k v] (conj res (assoc v :db/ident k))) [] @learned))))

(defn db-for!
  "Create a database for the argument data and return a connection to it."
  [data & {:keys [db-name] :or {db-name "temp"}}]
  (let [db-cfg {:store {:backend :mem :id db-name} :keep-history? false :schema-flexibility :write}]
    (when (d/database-exists? db-cfg) (d/delete-database db-cfg))
    (d/create-database db-cfg)
    (let [conn-atm (d/connect db-cfg)]
      (d/transact conn-atm (learn-schema-walking data))
      (d/transact conn-atm data)
      @conn-atm)))
