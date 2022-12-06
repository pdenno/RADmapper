(ns rad-mapper.query
  "supporting code for query and express"
  (:require
   [clojure.string :refer [starts-with?]]
   [clojure.walk   :refer [keywordize-keys]]
   #?(:clj  [datahike.api         :as d]
      :cljs [datascript.core      :as d])
   [taoensso.timbre               :as log]))

;;; ToDo: Get some more types in here, and in implementation generally.
(defn db-type-of
  "Return a Datahike schema :db/valueType object for the argument"
  [obj]
  (cond (string? obj)  :db.type/string
        (number? obj)  :db.type/number
        (keyword? obj) :db.type/keyword
        (map? obj)     :db.type/ref
        (boolean? obj) :db.type/boolean))

(defn sample-vec
  "Run db-type-of on just some of the data in vec."
  [vec k & {:keys [sample-threshold sample-size]
             :or {sample-threshold 200 sample-size 100}}]
  (let [len (count vec)
        vec (if (< len sample-threshold)
               vec ; ToDo: repeatedly solution less than ideal.
               (repeatedly sample-size #(nth vec (rand-int len))))
        result (-> (map db-type-of vec) set)]
    (if (> (count result) 1)
      (throw (ex-info "Heterogeneous types:"
                      {:types result :attribute k :vector vec}))
      (first result))))

(def diag (atom nil))

(defn schema-from-canonical
  "Return a Datahike or Datascript conforming schema from the canonical schema,
   which is a map with keys naming the attribute (like :db/ident) and values being content."
  [smap type]
  (case type
    :datahike ;; DH uses a vec and attr :db/ident.
    (reduce-kv (fn [res k v] (conj res (assoc v :db/ident k))) [] smap)
    :datascript ;; DS uses a map indexed by what would be :db/ident (like the input smap)
    (reduce-kv (fn [schemas attr schema]
                 (assoc schemas
                        attr ; DS doesn't use :db/valueType except to distinguish refs.
                        (reduce-kv (fn [m k v]
                                     (if (and (= k :db/valueType) (not (= v :db.type/ref)))
                                       m
                                       (assoc m k v)))
                                   {}
                                   schema)))
               {}
               smap)))

;;; ToDo: I think this can override known-schema. I should do a final merge of known-schema at each entry.
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
                    this-typ  (if vec? (sample-vec v k) (db-type-of v))
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
      (schema-from-canonical @learned (if datahike? :datahike :datascript)))))

(def example
  '{"owners"
    {"t/type" "OWNER",
     "owner/id" (bi/express-key ?ownerName),
     "owner/systems" [{"t/type" "SYSTEM",
                       "system/id" (bi/express-key ?systemName),
                       "system/devices" [{"t/type" "DEVICE",
                                          "device/id" (bi/express-key ?deviceName),
                                          "device/status" ?status}]}]}})

(defn qvar? [obj] (and (symbol? obj) (starts-with? (name obj) "?")))
(defn key-exp? [obj]
  (and (seq? obj)
       (let [[exp-key qvar] obj]
         (and (= exp-key 'bi/express-key) (qvar? qvar)))))

;;; ToDo: Check that there is at most one key at a map level. (filter instead of some).
(defn rewrite-keys
  "Rewrite an express body's bi/express-key forms to concatenate parent keys."
  [body]
  (let [ekeys (atom [])]
    (letfn [(rew-keys [obj]
              (cond (map? obj)
                    (do (when-let [this-key (some #(when (key-exp? %) (second %)) (vals obj))]
                          (swap! ekeys conj this-key))
                        (let [res (doall (reduce-kv (fn [m k v]
                                                      (if (key-exp? v)
                                                        (assoc m k `(bi/express-key ~@(deref ekeys)))
                                                        (assoc m k (rew-keys v))))
                                                    {} obj))]
                          (swap! ekeys #(-> % rest vec))
                          res))
                    (vector? obj) (doall (mapv rew-keys obj))
                    :else obj))]
      (rew-keys body))))

(defn learn-schema-from-express
  [body]
  (let [schema (atom {})
        key-stack (atom '[])]
    (letfn [(lsfe-aux [obj]
              (cond (map? obj)
                    (do
                      (swap! key-stack conj '())
                      (doseq [[k v] obj]
                        (let [db-ident (keyword k)
                              typ (db-type-of v)]
                          (cond typ         (do (swap! schema #(assoc-in % [db-ident :db/valueType] typ))
                                                (swap! schema #(assoc-in % [db-ident :db/cardinality] :db.cardinality/one))
                                                (when (= :db.type/ref typ) (lsfe-aux v))),
                                (vector? v) (let [typ (db-type-of (first v))]
                                              (when-not typ (throw (ex-info "Invalid express structure" {:obj v})))
                                              (swap! schema #(assoc-in % [db-ident :db/valueType] typ))
                                              (swap! schema #(assoc-in % [db-ident :db/cardinality] :db.cardinality/many))
                                              (doall (map lsfe-aux v)))
                                (seq? v)    (let [[exp-key qvar] v]
                                              (when (and (= exp-key 'bi/express-key) (qvar? qvar))
                                                (swap! schema #(assoc-in % [db-ident :_rm/cat-key] (conj (-> @key-stack first) qvar)))
                                                (let [ix (-> @key-stack count dec)]
                                                  (swap! key-stack #(assoc % ix (conj (nth % ix) qvar)))))))))
                      #_(swap! key-stack #(-> % rest vec)))
                    (vector? obj) (doall (map lsfe-aux obj))))]
      (lsfe-aux body))
    {:result @schema
     :key-stack @key-stack}))

;;; BTW, I can make a trivial DB on my laptop using this in 6 milliseconds.
#?(:clj
(defn db-for!
  "Datahike version : Create a database for the argument data and return a connection to it.
   Called by builtins for query and express, for example.
   The argument known-schema is DS-style, a map indexed by db/ident (not a vector).
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
   The argument known-schema is DS-style, a map indexed by db/ident (not a vector).
   NOTE: The db attributes (map keys) have to be keyword; you can't use strings etc."
  [data & {:keys [known-schema] :or {known-schema {}}}]
  (let [data (-> (if (vector? data) data (vector data)) keywordize-keys)
        schema (learn-schema data :known-schema known-schema :datahike? false)
        conn-atm (d/create-conn schema)]
    (d/transact conn-atm data)
    conn-atm)))
