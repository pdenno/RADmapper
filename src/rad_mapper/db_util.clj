(ns rad-mapper.db-util
  "Utilities for schema-db (which will likely become a library separate from rad-mapper"
  (:require
   [cemerick.url          :as url]
   [clojure.data.xml      :as x]
   [clojure.java.io       :as io]
   [clojure.walk          :as walk]
   #?(:clj  [datahike.pull-api     :as dp]
      :cljs [datascript.pull-api   :as dp])
   [taoensso.timbre       :as log]))

;;; This seems to cause problems in recursive resolution. (See resolve-db-id)"
(defn db-ref?
  "It looks to me that a datahike ref is a map with exactly one key: :db/id."
  [obj]
  (and (map? obj) (= [:db/id] (keys obj))))

;;; {:db/id 3779}
(defn resolve-db-id
  "Return the form resolved, possibly removing some properties."
  ([form conn-atm] (resolve-db-id form conn-atm #{}))
  ([form conn-atm filter-set]
   (letfn [(resolve-aux [obj]
             (cond
               (db-ref? obj) (let [res (dp/pull @conn-atm '[*] (:db/id obj))]
                               (if (= res obj) nil (resolve-aux res)))
               (map? obj) (reduce-kv (fn [m k v] (if (filter-set k) m (assoc m k (resolve-aux v))))
                                     {}
                                     obj)
               (vector? obj)      (mapv resolve-aux obj)
               (set? obj)    (set (mapv resolve-aux obj))
               (coll? obj)        (map  resolve-aux obj)
               :else  obj))]
     (resolve-aux form))))

(defn keywordize
  "Return the string as a keyword. If the string as a colon in it,
  the prefix is used as the keyword's namespace."
  ([str]
   (if (string? str)
     (if-let [[_ prefix word] (re-matches #"(\w+)\:(\w+)" str)]
       (keyword prefix word)
       (keyword str))
     str))
  ([str ns]
   (keyword ns str)))

(defn condition-form
  "Return the form with certain map values as keywords and certain map values zipped.
   Usually top-level call is a form representing a whole schema file. Walks schema."
  [form]
  (let [key-pred? ; Map keys corresponding to values that should be keywordized
        #{:xsd/extension :cct/PrimitiveType :cct/sc-use :cct/sc-type :cct/CategoryCode}
        needs-zip? ; Map keys to encode as a map for later decoding (See db-utils/resolve-db-id)
        #{:code-list/terms}]
    (letfn [(cf-aux [form]
              (cond (map? form) (reduce-kv (fn [m k v]
                                             (if (needs-zip? k)
                                               (-> m
                                                   (assoc :zip/keys (-> v keys vec))
                                                   (assoc :zip/vals (-> v vals vec)))
                                               (if (key-pred? k)
                                                 (assoc m k (keywordize v))
                                                 (assoc m k (cf-aux v)))))
                                           {}
                                           form)
                    (vector? form)   (mapv cf-aux form)
                    (set? form) (set (map  cf-aux form))
                    (coll? form)     (map  cf-aux form)
                    :else form))]
      (cf-aux form))))

;;; POD ToDo: Spec about this?
(defn storable?
  "Return true if the argument contains no nils.
   Such data cannot be stored in datahike."
  [obj]
  (let [ok? (atom true)]
    (letfn [(storable-aux [obj]
              (cond (not @ok?) false
                    (nil? obj) (reset! ok? false)
                    (map? obj) (reset! ok? (reduce-kv (fn [result _ v] (cond (not @ok?) false
                                                                             (not result) false
                                                                             (nil? v) false
                                                                             :else (storable-aux v)))
                                                      true
                                                      obj))
                    (coll? obj) (reset! ok? (every? storable-aux obj))
                    :else true))]
      (storable-aux obj))
    @ok?))

(defn find-nils
  "Return the form with all its :db/id resolved."
  [form]
  (letfn [(find-nils-aux [obj]
            (cond
              (map? obj) (reduce-kv (fn [_ _ v] (if (nil? v)
                                                  (log/info "nil:" v)
                                                  (find-nils-aux v)))
                                    {}
                                    obj)
              (coll? obj) (do (some #(when (nil? %) (log/info "nil:" obj)) obj)
                              (map find-nils-aux obj))))]
    (find-nils-aux form)))


(defn xpath-internal
  [content props path-in]
  (loop [result content
         path path-in]
    (cond (empty? path) result,
          (not (map? content)) (when (:warn? props) (log/warn "xpath failed at:" path "in" path-in)),
          :else
          (let [search (first path)]
            (recur
             (if (number? search)
               (nth (:xml/content result) search)
               (some #(when (= search (:xml/tag %)) %) (:xml/content result)))
             (rest path))))))

;;; POD Could enhance this to be REAL XPath.
(defn xpath
  "Content is a map with :xml/content. Follow the path, each step of
   which selects something from argument's :xml/content
   either an :xml/tag element, in which the first such is chosen, or an index,
   in which case that one is "
  [content & path-in]
  (xpath-internal content {:warn? true} path-in))

(defn xpath-
  "Like xpath but without warning on no content."
  [content & path-in]
  (xpath-internal content {} path-in))

;;; POD More sophisticated usage???
(defn xml-type?
  "Return true if the content has :xml/tag = the argument."
  [xml xtype]
  (= (:xml/tag xml) xtype))

(defn clean-whitespace
  "Remove whitespace in element :content."
  [xml]
  (walk/postwalk
   (fn [obj]
     (if (and (map? obj) (contains? obj :content))
       (if (= 1 (count (:content obj))) ;; POD Maybe don't remove it if is the only content???
         obj
         (update obj :content (fn [ct] (remove #(and (string? %) (re-matches #"^\s*$" %)) ct))))
       obj))
   xml))

(defn explicit-root-ns
  "Return argument x/element-nss map modified so that that the empty-string namespace is 'root' or whatever
   If the schema uses 'xs' for 'http://www.w3.org/2001/XMLSchema', change it to xsd"
  [nspaces & {:keys [root-name] :or {root-name "ROOT"}}]
  (when (-> nspaces :p->u (contains? root-name))
    (log/warn "XML uses explicit 'root' namespace alias.")) ; POD so pick something else. NYI.
  (as-> nspaces ?ns
    (assoc-in ?ns [:p->u root-name] (or (get (:p->u ?ns) "") :mm/nil))
    (update ?ns :p->u #(dissoc % ""))
    (update ?ns :u->ps
            (fn [uri2alias-map]
              (reduce-kv (fn [res uri aliases]
                           (assoc res uri (mapv #(if (= % "") root-name %) aliases)))
                         {}
                         uri2alias-map)))
    ;; Now change "xs" to "xsd" if it exists.
    (if (= "http://www.w3.org/2001/XMLSchema" (get (:p->u ?ns) "xs"))
      (as-> ?ns ?ns1
        (assoc-in ?ns1 [:p->u "xsd"] "http://www.w3.org/2001/XMLSchema")
        (update ?ns1 :p->u  #(dissoc % "xs"))
        (update ?ns1 :u->ps #(dissoc % "http://www.w3.org/2001/XMLSchema"))
        (assoc-in ?ns1 [:u->ps "http://www.w3.org/2001/XMLSchema"] ["xsd"]))
      ?ns)))

;;; POD Currently this isn't looking for redefined aliases. It calls x/element-nss just once!
;;; (-> sample-ubl-message io/reader x/parse alienate-xml)
(defn alienate-xml ; Silly, but I like it!
  "Replace namespaced xml map keywords with their aliases."
  [xml]
  (let [ns-info (-> xml x/element-nss explicit-root-ns)]
    (letfn [(equivalent-tag [tag]
              (let [[success? ns-name local-name] (->> tag str (re-matches #"^:xmlns\.(.*)/(.*)$"))]
                (if success?
                  (let [ns-name (url/url-decode ns-name)]
                    (if-let [alias-name (-> ns-info :u->ps (get ns-name) first)]
                      (keyword alias-name  local-name)
                      (keyword ns-name     local-name)))
                  tag)))]
      (walk/postwalk
       (fn [obj]
         (if (and (map? obj) (contains? obj :tag))
           (update obj :tag equivalent-tag)
           obj))
       xml))))

;;; (detagify '{:tag :cbc/InvoiceTypeCode, :attrs {:listID "UN/ECE 1001 Subset", :listAgencyID "6"}, :content ("380")})
(defn detagify
  "Argument in content from clojure.data.xml/parse. Return a map where
    (1) :tag is :schema/type,
    (2) :content, if present, is a simple value or recursively detagified.
    (3) :attrs, if present, are :xml/attrs.
   The result is that
     (a) returns a string or a map that if it has :xml/content, it is a string or a vector.
     (b) if a map, and the argument had attrs, has an :xml/attrs key."
  [obj]
  (cond (map? obj)
        (as-> obj ?m
          (assoc ?m :xml/tag (:tag ?m))
          (if (not-empty (:attrs   ?m)) (assoc ?m :xml/attrs (:attrs ?m)) ?m)
          (if (not-empty (:content ?m)) (assoc ?m :xml/content (detagify (:content ?m))) ?m)
          (dissoc ?m :tag :attrs :content))
        (seq? obj) (if (and (== (count obj) 1) (-> obj first string?))
                     (first obj)
                     (mapv detagify obj))
        (string? obj) obj ; It looks like nothing will be number? Need schema to fix things.
        :else (throw (ex-info "Unknown type in detagify" {:obj obj}))))

(defn read-xml
  "Return a map of the XML file read."
  [pathname]
  (let [xml (-> pathname io/reader x/parse)]
     {:xml/ns-info (explicit-root-ns (x/element-nss xml))
      :xml/content (-> xml alienate-xml clean-whitespace detagify vector)
      :schema/pathname pathname}))

(defn parse-xml-string
  "This is useful for debugging. Typical usage:
  (-> sss util/parse-xml-string (xpath :xsd/schema :xsd/complexType) rewrite-xsd)"
  [s]
  (let [pre "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
             <xsd:schema xmlns=\"urn:test-string\"
                         xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
                         targetNamespace=\"urn:test-string\"
                         elementFormDefault=\"qualified\"
                         attributeFormDefault=\"unqualified\"
                         version=\"2.3\">"
        post "</xsd:schema>"
        xml  (x/parse-str (str pre s post))]
    (-> {}
        (assoc :xml/ns-info (explicit-root-ns (x/element-nss xml)))
        (assoc :xml/content (-> xml alienate-xml clean-whitespace detagify vector)))))

(defn dir-up
  "file is java.io file. Return the path of the nth parent directory, applied recursively to the ."
  [file n]
  (if (> n 0)
    (recur (.getParentFile file) (dec n))
    file))
