(ns pdenno.rad-mapper.util
  (:require
   [cemerick.url                 :as url]
   [clojure.data.xml             :as x]
   [clojure.java.io              :as io]
   [clojure.walk                 :as walk]))

(defn nspaces
  "Return a string of n spaces."
  [n]
  (reduce (fn [s _] (str s " ")) "" (range n)))

(defn explicit-root-ns
  "Return argument x/element-nss map modified so that that the empty-string namespace is 'root' or whatever
   If the schema uses 'xs' for 'http://www.w3.org/2001/XMLSchema', change it to xsd"
  [nspaces & {:keys [root-name] :or {root-name "ROOT"}}]
  #_(when (-> nspaces :p->u (contains? root-name))
    (log/warn "XML uses explicit 'root' namespace alias.")) ; ToDo
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

(defn alienate-xml
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

(defn clean-whitespace
  "Remove whitespace in element :content."
  [xml]
  (walk/postwalk
   (fn [obj]
     (if (and (map? obj) (contains? obj :content))
       (if (= 1 (count (:content obj))) ;; ToDo Maybe don't remove it if is the only content?
         obj
         (update obj :content (fn [ct] (remove #(and (string? %) (re-matches #"^\s*$" %)) ct))))
       obj))
   xml))

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

(defn trans-tag [tag]
  (if-let [ns (namespace tag)]
    (keyword (str ns "_" (name tag)))
    tag))

(defn number-str?
  "This only handles integers and decimals."
  [s]
  (when-let [[_ _sign first-digit decimal?] (re-matches #"^([\+,\-])?(\d)?\d*(\.)?\d*$" s)]
    (or decimal? (not= first-digit "0"))))

(defn simplify-xml
  "Given a map of xml in the form produced by read-xml, change :xml/tag and :xml/content to a map."
  [obj]
  (cond
    (not (or (map? obj) (vector? obj)))
    (if (number-str? obj) (read-string obj) obj)
    (vector? obj) (mapv simplify-xml obj)
    (map? obj) (as-> {} ?r
                 (assoc ?r (trans-tag (:xml/tag obj)) (simplify-xml (:xml/content obj)))
                 (reduce-kv (fn [r key val] (assoc r (trans-tag key) (simplify-xml val)))
                            ?r
                            (:xml/attrs obj)))))

(defn string-permute
  "Return a lazy sequence of the name of columns"
  ([chars] (string-permute [""] chars))
  ([prev chars]
   (let [strs (mapcat (fn [c] (map (fn [s] (str c s)) prev)) chars)]
     (lazy-cat strs (string-permute strs chars)))))
