(ns rad-mapper.query
  "supporting code for query and express"
  (:require
   [clojure.walk :refer [keywordize-keys]]
   #?(:clj  [datahike.api         :as d]
      :cljs [datascript.core      :as d])
   [failjure.core                 :as fj]
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
  "Run dh-type-of on just some of the data in vec."
  [vec k & {:keys [sample-threshold sample-size]
             :or {sample-threshold 200 sample-size 100}}]
  (let [len (count vec)
        vec (if (< len sample-threshold)
               vec ; ToDo: repeatedly solution less than ideal.
               (repeatedly sample-size #(nth vec (rand-int len))))
        result (-> (map dh-type-of vec) set)]
    (if (> (count result) 1)
      (fj/fail "Heterogeneous types: %s" {:types result :attribute k :vector vec})
      (first result))))

;;; ToDo: It should be possible to learn schema from binding sets and query combined.
;;;       That would be an entirely different function, though.
(defn learn-schema
  "Return DH/DS schema objects for the data provided.

   Limitation: It can't learn from binding sets; the attributes of those are not the
   data's attributes, and everything will appear as multiplicity 1."
  [data & {:keys [known-schema datahike?] :or {known-schema {} datahike? true}}]
  (let [learned (atom known-schema)]
    (letfn [(update-learned! [k v]
              (let [typ  (-> @learned k :db/valueType)
                    card (-> @learned k :db/cardinality)
                    vec? (vector? v)
                    this-typ  (if vec? (sample-vec v k) (dh-type-of v))
                    this-card (if (or vec? (= card :db.cardinality/many)) ; permissive to many
                                :db.cardinality/many
                                :db.cardinality/one)]
                (if (and typ (not= typ this-typ))
                  (log/warn "Different types:" k "first:" typ "second:" this-typ)
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
      (cond->> @learned
        datahike? (reduce-kv (fn [res k v] (conj res (assoc v :db/ident k))) [])))))

(defn json-like
  "Return the object with its map keys replaced with strings.
  :ab ==> 'ab'; :ns/ab ==> 'ns/ab'."
  [obj]
  (cond (map? obj) (reduce-kv (fn [m k v]
                                (cond (keyword? v) (assoc m k (subs (str v) 1))
                                      (vector? v) (assoc m k (mapv json-like v))
                                      (map? v)   (assoc m k (reduce-kv (fn [m k v] (assoc m k (json-like v))) {} v))
                                      :else     (assoc m k v)))
                              {}
                              obj),
        (vector? obj) (mapv json-like obj),
        :else obj))

;;; BTW, I can make a trivial DB on my laptop using this in 6 milliseconds.
#?(:clj
(defn db-for!
  "Datahike version : Create a database for the argument data and return a connection to it.
   Called by builtins for query and express, for example.
   The argument known-schema takes a map indexed by db/ident (not a vector).
   NOTE: The db attributes (map keys) have to be keyword; you can't use strings etc."
  [data & {:keys [known-schema db-name] :or {known-schema {} db-name "temp"}}]
  (let [db-cfg {:store {:backend :mem :id db-name} :keep-history? false :schema-flexibility :write}
        data (-> (if (vector? data) data (vector data)) keywordize-keys)]
    #?(:clj (when (d/database-exists? db-cfg) (d/delete-database db-cfg))) ; ToDo: FIX!
    (d/create-database db-cfg)
    (let [conn-atm (d/connect db-cfg)]
      (d/transact conn-atm (learn-schema data :known-schema known-schema))
      (d/transact conn-atm data)
      conn-atm))))

#?(:cljs
(defn db-for!
  "Datascript version: Create a database for the argument data and return a connection to it.
   Called by builtins for query and express, for example.
   The argument known-schema takes a map indexed by db/ident (not a vector).
   NOTE: The db attributes (map keys) have to be keyword; you can't use strings etc."
  [data & {:keys [known-schema] :or {known-schema {}}}]
  (let [data (-> (if (vector? data) data (vector data)) keywordize-keys)
        schema   (learn-schema data :known-schema known-schema :datahike? false)
        conn-atm (d/create-conn schema)]
    (d/transact conn-atm data)
    conn-atm)))
