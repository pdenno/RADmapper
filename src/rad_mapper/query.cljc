(ns rad-mapper.query
  "supporting code for query and express"
  (:require
   [clojure.string :refer [starts-with? replace]]
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

;;; ToDo: Pull out all the :_rm/ stuff.
(defn schema-from-canonical
  "Return a Datahike or Datascript conforming schema from the canonical schema,
   which is a map with keys naming the attribute (like :db/ident) and values being content.
   The argument schema can have content that isn't fit for any DB; of course the schema
   returned will filter all that out."
  [smap type]
  (as-> smap ?schema ; Remove schema entries whose keys are not :db
    (reduce-kv (fn [m k v]
                 (let [new-v (reduce-kv (fn [m1 k1 v1] (if (= "db" (namespace k1)) (assoc m1 k1 v1) m1))
                                        {}
                                        v)]
                   (assoc m k new-v)))
               {}
               ?schema)
    (case type
      :datahike ;; DH uses a vec and attr :db/ident.
      (reduce-kv (fn [res k v] (conj res (assoc v :db/ident k))) [] ?schema)
      :datascript ;; DS uses a map indexed by what would be :db/ident (like the input ?schema)
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
                 ?schema))))

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

(defn qvar? [obj] (and (symbol? obj) (starts-with? (name obj) "?")))
(defn key-exp?
  "Return true when the argument looks like (bi/express-key ?some-qvar)."
  [obj]
  (and (seq? obj)
       (let [[exp-key qvar] obj]
         (and (= exp-key :rm/express-key) (qvar? qvar)))))

;;; ToDo: Check that there is at most one key at each map level. (filter instead of some).
(defn rewrite-express-keys
  "Rewrite an express body's bi/express-key forms to concatenate parent keys required for reducing over it."
  [body]
  (let [ekeys (atom [])]
    (letfn [(rew-keys [obj]
              (cond (map? obj)
                    (do (when-let [this-key (some #(when (key-exp? %) (second %)) (vals obj))]
                          (swap! ekeys conj this-key))
                        (let [res (doall (reduce-kv (fn [m k v]
                                                      (if (key-exp? v)
                                                        (assoc m k `(:rm/express-key ~@(deref ekeys)))
                                                        (assoc m k (rew-keys v))))
                                                    {} obj))]
                          (swap! ekeys #(-> % rest vec)) ; Pop the key when you've finished the map.
                          res))
                    (vector? obj) (doall (mapv rew-keys obj))
                    :else obj))]
      (rew-keys body))))

(defn rewrite-express-catkeys
  "Rewrite an express body by inserting :_rm/whatever attributes required for reducing over it."
  [body schema]
  (let [new-keys (reduce-kv (fn [m k v] (if (contains? v :_rm/ref-key) (assoc m k v) m)) {} schema)]
    (letfn [(rew4ckeys [obj]
              (cond (map? obj)     (reduce-kv (fn [m k v]
                                                (if-let [info (some (fn [[_ iv]] (when (= k (:_rm/ref-key iv)) iv))
                                                                    (seq new-keys))]
                                                  (-> m
                                                      (assoc (:_rm/self info) v)
                                                      (assoc (:_rm/ref-key info) (last v)))
                                                  (assoc m k (rew4ckeys v))))
                                              {}
                                              obj)
                    (vector? obj)  (mapv rew4ckeys obj)
                    :else          obj))]
      (rew4ckeys body))))

(defn express-key-schema
  "Return schema information for an express-key."
  [k v]
  (let [db-ident (keyword "_rm"
                          (apply str
                                 (replace k "/" "*")
                                 "--"
                                 (interpose "|" (map #(replace (name %) "?" "") (rest v)))))]
    (-> {}
        (assoc-in [db-ident :db/unique] :db.unique/identity)
        (assoc-in [db-ident :db/valueType] :db.type/string)
        (assoc-in [db-ident :db/cardinality] :db.cardinality/one)
        (assoc-in [db-ident :_rm/self] db-ident)
        (assoc-in [db-ident :_rm/ref-key] k))))

;;; ToDo: Here when you encounter a [:rm/express-key ?qvar...] you need to
;;;    1) note that the db-ident is :db.cardinality/one
;;;    2) create another db-ident (maybe :_rm/orig-db-ident--?qvar-...) that is :db.unique/ident and :db.type/string
(defn learn-schema-from-express
  [body]
  (let [schema (atom {:_rm/ROOT {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}})]
    (letfn [(lsfe-aux [obj]
              (cond (map? obj)
                      (doseq [[k v] obj]
                        (let [db-ident (keyword k)
                              typ (db-type-of v)]
                          (when (qvar? v)
                            (swap! schema #(assoc-in % [db-ident :_rm/qvar] v)))
                          (cond typ         (do (swap! schema #(assoc-in % [db-ident :db/valueType] typ))
                                                (swap! schema #(assoc-in % [db-ident :db/cardinality] :db.cardinality/one))
                                                (when (= :db.type/ref typ) (lsfe-aux v))),
                                (vector? v) (let [typ (db-type-of (first v))]
                                              (when-not typ (throw (ex-info "Invalid express structure" {:obj v})))
                                              (swap! schema #(assoc-in % [db-ident :db/valueType] typ))
                                              (swap! schema #(assoc-in % [db-ident :db/cardinality] :db.cardinality/many))
                                              (doall (map lsfe-aux v)))
                                (seq? v)    (when (key-exp? v) ; ToDo: otherwise learn-schema-from-bset.
                                              (swap! schema #(merge % (express-key-schema k v)))
                                              (swap! schema #(assoc-in % [db-ident :_rm/qvar] (last v)))
                                              (swap! schema #(assoc-in % [db-ident :db/cardinality] :db.cardinality/one)))))
                        (lsfe-aux v)),
                    (vector? obj) (doall (map lsfe-aux obj))))]
      (lsfe-aux body)
      @schema)))

;;; ToDo: Do I care that I'm only looking at one bset?
(defn learn-schema-from-bset
  "Update the schema with :db/valueType from an example bset."
  [known-schema bset]
  (let [schema (atom known-schema)]
    (doseq [[qvar bval] (seq bset)]
      ;; Not all the bs keys need to be used in the express.
      (when-let [skey (some #(let [[skey smap] %] (when (= qvar (:_rm/qvar smap)) skey)) @schema)]
        (when-let [typ (db-type-of bval)]
          (swap! schema #(assoc-in % [skey :db/valueType] typ))
          (swap! schema #(assoc-in % [skey :db/cardinality] :db.cardinality/one)))))
    @schema))

;;; BTW, I can make a trivial DB on my laptop using this in 6 milliseconds.
#?(:clj
(defn db-for!
  "Datahike version : Create a database for the argument data and return a connection to it.
   Called by builtins for query and express, for example.
   The argument known-schema is DS-style, a map indexed by db/ident (not a vector).
   NOTE: The db attributes (map keys) have to be keyword; you can't use strings etc."
  [data & {:keys [known-schema db-name :learn?] :or {known-schema {} db-name "temp" learn? true}}]
  (let [db-cfg {:store {:backend :mem :id db-name} :keep-history? false :schema-flexibility :write}
        data (-> (if (vector? data) data (vector data)) keywordize-keys)]
    #?(:clj (when (d/database-exists? db-cfg) (d/delete-database db-cfg))) ; ToDo: FIX!
    (d/create-database db-cfg)
    (let [conn-atm (d/connect db-cfg)]
      (d/transact conn-atm (if learn? (learn-schema data :known-schema known-schema) known-schema))
      (d/transact conn-atm data)
      conn-atm))))

#?(:cljs
(defn db-for!
  "Datascript version: Create a database for the argument data and return a connection to it.
   Called by builtins for query and express, for example.
   The argument known-schema is DS-style, a map indexed by db/ident (not a vector).
   NOTE: The db attributes (map keys) have to be keyword; you can't use strings etc."
  [data & {:keys [known-schema learn?] :or {known-schema {} learn? true}}]
  (let [data (-> (if (vector? data) data (vector data)) keywordize-keys)
        schema (if learn? (learn-schema data :known-schema known-schema :datahike? false) known-schema)
        conn-atm (d/create-conn schema)]
    (d/transact conn-atm data)
    conn-atm)))
