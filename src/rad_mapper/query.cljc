(ns rad-mapper.query
  "supporting code for query and express"
  (:require
   [clojure.string :refer [starts-with?] :as string]
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
  "Return true when the argument looks like (:rm/express-key ?some-qvar)."
  [obj]
  (and (seq? obj)
       (let [[exp-key & args] obj]
         (and (= :rm/express-key exp-key)
              (every? #(or (qvar? %) (string? %)) args)))))

(defn schema-ident
  "Return a schema key for the argument stack of key representing
   the nesting of a value in the express body."
  [user-key all-keys]
  (keyword "_rm"
           (apply str
                  (string/replace user-key "/" "*")
                  (if (empty? all-keys) "" "--")
                  (interpose "|" (map #(-> % str (string/replace "/" "*")) all-keys)))))

;(def ^:dynamic *key?* true)

(defn key-schema
  "Define schema information for a user key (constant string or qvar doesn't matter)."
  [ident k all-keys]
   {:db/cardinality :db.cardinality/one,
    :db/valueType :db.type/string,
    :db/unique :db.unique/identity
    :_rm/cat-key all-keys,
    :_rm/self ident
    :_rm/user-key (str k)})

(defn exp-key-schema
  "Define schema information for the key that has as a value an express key.
   For example owner/id in 'owner/id' : key(?ownerName)"
  [ident k]
  {:db/cardinality :db.cardinality/one,
   :db/valueType :db.type/string, ; Might be overwritten by b-set knowledge.
   :_rm/self ident
   :_rm/user-key k})

(defn user-key
  "Return a keyword for a user-defined key. If the argument is a string, it could have a / in it.
   In which case, it is treated as namespaced."
  [s]
  (if-let [[_ nspace nam] (re-matches #"(.*)/(.*)" (str s))]
    (keyword nspace (string/replace (str nam) "/" "*"))
    (keyword s)))

;;; This one, where express-keys are implied by qvar is simpler to process.
;;;              {'owners':
;;;                 {?ownerName:
;;;                    {'systems':
;;;                       {?systemName:
;;;                          {?deviceName : {'id'     : ?id,
;;;                                          'status' : ?status}}}}

;;; Here you have to essentially make it look like the above plus keep extras like 'owner/id' and 'device/id'.
;;;              {'owners': {'owner/id'     : key(?ownerName),
;;;                          'systems'      : [{'system/id'  : key(?systemName),
;;;                                             'devices'    : [{'device/id' : key(?deviceName),
;;;                                                              'status'    : ?status}]}]}}
(defn schematic-express-body
  "Return a map containing a schema describing the argument express body and
   the body rewritten to use that schema. The schema will have to be augmented
   by :db/valueType information and some b-set data will have to be boxed because
   the values of keys is always a :db.value/ref.
   Express keys can only appear in value position of a map.
   Keys (qvar or constant) are always treated things with :_rm/Kkey-val."
  [body]
  (let [key-stack (atom []) ; ToDo: currently not popping stack.
        schema (atom {})]
    (letfn [(rb [obj] ; Here there is an express-key in a map value.
              (if-let [{:keys [key-key key-val]} (and (map? obj)
                                                      (some  (fn [[k v]] (when (key-exp? v)
                                                                           {:key-key k :key-val (second v)}))
                                                             (seq obj)))]
                (let [ident (schema-ident key-val @key-stack)]
                  (swap! key-stack conj key-val)
                  (swap! schema #(assoc % ident (key-schema ident key-key @key-stack)))
                  (swap! schema #(assoc % (user-key key-key) (exp-key-schema (user-key key-key) key-key)))
                  (-> {}
                      (assoc (keyword key-key) key-val)
                      (assoc ident `(:rm/express-key ~@(deref key-stack)))
                      (assoc :_rm/val (rb (dissoc obj key-key))))) ; Rest of map is under the :_rm/val
                (cond (map? obj)            (let [result
                                                  (reduce-kv (fn [r k v] ; Each key is treated
                                                               (swap! key-stack conj k)
                                                               (let [ident (schema-ident k @key-stack)
                                                                     res (conj r
                                                                               (-> {}
                                                                                   (assoc ident `(:rm/express-key ~@(deref key-stack)))
                                                                                   (assoc :_rm/user-key k)
                                                                                   (assoc :_rm/val (rb v))))]
                                                                 (swap! schema #(assoc % ident (key-schema ident k @key-stack)))
                                                                 (swap! key-stack #(-> % butlast vec)) ; Since iterating on slots, pop stack.
                                                                 res))
                                                             []
                                                             obj)]
                                              ;; ToDo: Either this or fix it at :_rm/ROOT later.
                                              (if (== 1 (count result)) (first result) result))
                     (vector? obj)          (mapv rb obj)
                     :else                  obj)))]
      {:body (rb body)
       :schema @schema})))


;;; ToDo: Check that there is at most one key at each map level. (filter instead of some).
(defn rewrite-express-keys
  "Called in rewrite.
   Rewrite an express body's bi/express-key forms to concatenate parent keys required for reducing over it."
  [body]
  (let [ekeys (atom [])]
    (letfn [(rew-keys [obj]
              (cond (map? obj) ; I think the or below is justified; the map can only have one key.
                    (do (when-let [this-key (or (some #(when (key-exp? %) (second %)) (vals obj))
                                                (some #(when (key-exp? %) (second %)) (keys obj)))]
                          (swap! ekeys conj this-key))
                        (let [res (doall (reduce-kv (fn [m k v]
                                                      (if (key-exp? v)
                                                        (assoc m k `(:rm/express-key ~@(deref ekeys)))
                                                        (if (key-exp? k)
                                                          (assoc m `(:rm/express-key ~@(deref ekeys)) (rew-keys v))
                                                          (assoc m k (rew-keys v)))))
                                                    {} obj))]
                          (swap! ekeys #(-> % rest vec)) ; Pop the key when you've finished the map.
                          res))
                    (vector? obj) (doall (mapv rew-keys obj))
                    :else obj))]
      (rew-keys body))))

(def support-schema
  "These are added to the schema when reducing on express-body."
  {:_rm/ROOT        {:db/cardinality :db.cardinality/many :db/valueType :db.type/ref}
   :_rm/val         {:db/cardinality :db.cardinality/many :db/valueType :db.type/ref}
   :_rm/user-key    {:db/cardinality :db.cardinality/one  :db/valueType :db.type/string }
   :box/boolean-val {:db/cardinality :db.cardinality/one  :db/valueType :db.type/boolean}
   :box/keyword-val {:db/cardinality :db.cardinality/one  :db/valueType :db.type/keyword}
   :box/number-val  {:db/cardinality :db.cardinality/one  :db/valueType :db.type/number}
   :box/string-val  {:db/cardinality :db.cardinality/one  :db/valueType :db.type/string}})

;;; ToDo: Currently not used and might not ever be useful.
#_(defn schema-updates-from-data
  "Return a map of :db/valueTypes by studying the data and schema."
  [schema data]
  (let [new-info (atom {})]
    (letfn [(supdate [obj]
              (cond (map? obj)        (doseq [[k v] (seq obj)]
                                             (when (contains? schema k)
                                               (when-let [typ (db-type-of v)]
                                                 (swap! new-info #(assoc-in % [k :db/valueType] typ))))
                                             (supdate v))
                    (vector? obj)     (doall (map supdate obj))))]
      (supdate data)
      @new-info)))

;;; BTW, I can make a trivial DB on my laptop using this in 6 milliseconds.
;;; ToDo: Where I'm creating DBs for short-term use (e.g. express reduce) we need to d/delete-database when done!
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
    (let [db-atm (d/connect db-cfg)]
      (d/transact db-atm (if learn? (learn-schema data :known-schema known-schema) known-schema))
      (when (not-empty data) (d/transact db-atm data))
      db-atm))))

#?(:cljs
(defn db-for!
  "Datascript version: Create a database for the argument data and return a connection to it.
   Called by builtins for query and express, for example.
   The argument known-schema is DS-style, a map indexed by db/ident (not a vector).
   NOTE: The db attributes (map keys) have to be keyword; you can't use strings etc."
  [data & {:keys [known-schema learn?] :or {known-schema {} learn? true}}]
  (let [data (-> (if (vector? data) data (vector data)) keywordize-keys)
        schema (if learn? (learn-schema data :known-schema known-schema :datahike? false) known-schema)
        db-atm (d/create-conn schema)]
    (d/transact db-atm data)
    db-atm)))
