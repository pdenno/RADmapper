(ns rad-mapper.query
  "supporting code for query and express"
  (:require
   [clojure.string                :refer [starts-with?] :as string]
   [clojure.walk                  :refer [keywordize-keys]]
   [rad-mapper.data-util.db-util  :refer [db-type-of box]]
   #?(:clj  [datahike.api         :as d]
      :cljs [datascript.core      :as d])
   [taoensso.timbre :as log]))

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
(defn schema-for-db
  "Given a map indexed by DB idents with values (maps) containing some information about those
   idents in a form consistent with the type argument of database (either :datascript or :datahike)
   return a conforming schema for that database. To do this it just filters out the extraneous
   key/value pairs of each value, and in the case of :datahike, returns a vector of maps where the
   original keys are used to set :db/ident in each vector element (a map)."
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
      (schema-for-db @learned (if datahike? :datahike :datascript)))))

;;;------------------------------ Express reduce-body generation
(def support-schema
  "These are added to the schema when reducing on express-body. All of these can appear in data,
   of course, but the full-schema can have values that do not meet the :db/valueType here.
   For example, in the full-schema :_rm/user-key of attrs can be a qvar.
   (BTW that indicates 'qvar-in-key-pos'.)"
  {:_rm/ROOT          {:db/cardinality :db.cardinality/many :db/valueType :db.type/ref}
   :_rm/attrs         {:db/cardinality :db.cardinality/many :db/valueType :db.type/ref}
   :_rm/vals          {:db/cardinality :db.cardinality/many :db/valueType :db.type/ref}
   :_rm/val           {:db/cardinality :db.cardinality/one  :db/valueType :db.type/ref}
   :_rm/user-key      {:db/cardinality :db.cardinality/one  :db/valueType :db.type/ref}    ; It is boxed.
   :_rm/ek-val        {:db/cardinality :db.cardinality/one  :db/valueType :db.type/ref}    ; key component used w/ express keys and in output.
   :box/boolean-val   {:db/cardinality :db.cardinality/one  :db/valueType :db.type/boolean}
   :box/keyword-val   {:db/cardinality :db.cardinality/one  :db/valueType :db.type/keyword}
   :box/number-val    {:db/cardinality :db.cardinality/one  :db/valueType :db.type/number}
   :box/string-val    {:db/cardinality :db.cardinality/one  :db/valueType :db.type/string}})

(defn child-node-type
  "Argument is the child node of a key (qvar, string, whatever) in the express body.
   returned is a attribute type from the support schema."
  [node]
  (cond (map? node)    :_rm/attrs
        (vector? node) :_rm/vals
        :else          :_rm/val))

(defn qvar? [obj] (and (symbol? obj) (starts-with? (name obj) "?")))

(defn exp-key?
  "Return true when the argument looks like [:rm/express-key ?some-qvar]."
  [obj]
  (and (vector? obj)
       (let [[exp-key & args] obj]
         (and (= :rm/express-key exp-key)
              (every? #(or (qvar? %) (string? %)) args)))))

(defn schema-ident
  "Return a schema key for the argument stack of key representing
   the nesting of a value in the express body."
  [user-key all-keys]
  (keyword "_rm"
           (apply str
                  (string/replace (str user-key) "/" "*")
                  (if (empty? all-keys) "" "--")
                  (interpose "|" (map #(-> % str (string/replace "/" "*")) all-keys)))))

(defn key-schema
  "Define schema information for a user key (constant string or qvar doesn't matter)."
  [ident k all-keys & {:keys [exp-key?]}]
  (cond-> {:db/unique :db.unique/identity
           :db/valueType :db.type/string
           :db/cardinality :db.cardinality/one
           :_rm/cat-key all-keys          ; ToDo: This is just for debugging, I think.
           :_rm/self ident
           :_rm/user-key (box k)}               ; Will be a qvar if exp-key? = true
    exp-key? (assoc :_rm/exp-key? true))) ; ToDo: This is just for debugging, I think. Data has :_rm/ek-val.

(defn db-key-ident
  "Return a keyword for a user-defined key. If the argument is a string, it could have a / in it.
   In which case, it is treated as namespaced. If it has more than one, th ones to the right are
   changed to '*'"
  [s]
  (if-let [[_ nspace nam] (re-matches #"(.*)/(.*)" (str s))]
    (keyword nspace (string/replace (str nam) "/" "*"))
    (keyword s)))

;;; Here :rm/express-key are implied by qvar; they aren't spelled out in the base-body.
;;;              {'owners':
;;;                 {?ownerName:
;;;                    {'systems':
;;;                       {?systemName:
;;;                          {?deviceName : {'id'     : ?id,
;;;                                          'status' : ?status}}}}

;;; Here you have to essentially make it look like the above plus keep extras like 'owner/id' and 'device/id'.
;;;             {"owners" [{"owner/id" [:rm/express-key ?ownerName],
;;;                         "systems" [{"system/id" [:rm/express-key ?systemName],
;;;                                     "devices" [{"device/name" [:rm/express-key ?deviceName],
;;;                                                 "device/id" ?id, "status" ?status}]}]}]}
(defn schematic-express-body
  "Return a map containing
      (1) :reduce-body : the express body rewritten rewritten for use in $reduce and,
      (2) :schema a schema describing all the attributes introduced in the reduce body.
   The schema produced is a little bit short on information. Examples:
      (a) Some values will be qvars and thus the type won't be known until a bset applied.
      (b) Values for :_rm/val and :_rm/vals, though defined db/valueType db.type/ref, could be
          primitive types. When bset values are substituted, primitives for :_rm/val(s) are boxed.
   - Note that express keys can only appear in value position of the user's map but on rewriting
     :rm/express 'catkey' are injected for non-express key attrs and REMOVED for express-key attrs.
       - The attrs that are user-define express keys have the form:
         {:slot-name ?qvar :_rm/user-key 'slot-name' :_rm/{val,vals,attrs}}
   - The values in key-position in the base-body can be qvars strings or whatever.
   - There is no special processing needed for qvars in key-position."
  [base-body]
  (let [key-stack (atom [])
        schema (atom support-schema)]
    (letfn [(rb [obj] ; Here there is an express-key in a map value.
              (if-let [{:keys [key-key key-val]} (and (map? obj)
                                                      (some  (fn [[k v]] (when (exp-key? v)
                                                                           {:key-key k :key-val (second v)}))
                                                             (seq obj)))]
                ;; a key-exp
                (let [ident (db-key-ident key-key)]   ; user-defined key slots don't need fancy names, but they concatenate.
                  (swap! key-stack conj key-val)
                  (swap! schema #(assoc % ident (key-schema ident key-val @key-stack :exp-key? true)))
                   (-> {ident `[:rm/express-key  ~@(deref key-stack)]}
                       (assoc :_rm/user-key      (-> ident str (subs 1)))
                       (assoc :_rm/ek-val        key-val)
                       (assoc :_rm/attrs         (rb (dissoc obj key-key))))) ; Other attrs (_:rm/attrs) is a vector of maps because...
                                                                              ; ...each has its own data such as :_rm/user-key.
                ;; not a key-exp
                (cond (map? obj)      (reduce-kv (fn [r k v] ; Each key is treated, qvar, string, whatever.
                                                   (swap! key-stack conj k)
                                                   (let [ident (schema-ident k @key-stack)
                                                         ident-val `[:rm/express-key ~@(deref key-stack)]
                                                         attr (child-node-type v)
                                                         child (rb v)
                                                         res (conj r (-> {}
                                                                         (assoc ident ident-val)
                                                                         (assoc :_rm/user-key k)
                                                                         (assoc attr child)))]
                                                     (swap! schema #(assoc % ident (key-schema ident k @key-stack)))
                                                     (swap! key-stack #(-> % butlast vec)) ; Since iterating on slots, pop stack.
                                                     res))
                                                 []   ; Returning a vector of maps, which in the case of (dissoc obj key-key)...
                                                 obj) ; ...is the value of :_rm/attrs
                     (vector? obj)    (mapv rb obj)
                     :else            obj)))]
      {:reduce-body (rb base-body) ;
       :schema @schema})))

;;; ToDo: Check that there is at most one key at each map level. (filter instead of some).
(defn rewrite-express-keys
  "Called in rewrite.
   Rewrite an express body's bi/express-key forms to concatenate parent keys required for reducing over it."
  [body]
  (let [ekeys (atom [])]
    (letfn [(rew-keys [obj]
              (cond (map? obj) ; I think the 'or' below is justified; the map can only have one key.
                    (do (when-let [this-key (or (some #(when (exp-key? %) (second %)) (vals obj))
                                                (some #(when (exp-key? %) (second %)) (keys obj)))]
                          (swap! ekeys conj this-key))
                        (let [res (doall (reduce-kv (fn [m k v]
                                                      (if (exp-key? v)
                                                        (assoc m k `[:rm/express-key ~@(deref ekeys)])
                                                        (if (exp-key? k)
                                                          (assoc m `[:rm/express-key ~@(deref ekeys)] (rew-keys v))
                                                          (assoc m k (rew-keys v)))))
                                                    {} obj))]
                          (swap! ekeys #(-> % rest vec)) ; Pop the key when you've finished the map.
                          res))
                    (vector? obj) (doall (mapv rew-keys obj))
                    :else obj))]
      (rew-keys body))))

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
