(ns rad-mapper.util
  (:require
   [cemerick.url                 :as url]
   #?(:clj  [clojure.data.xml    :as x]) ; ToDo: Investigate. Not cljs version?
   #?(:cljs [cljs.reader]) ; ToDo: Investigate. Not cljs version?
   [clojure.pprint               :refer [cl-format]]
   [clojure.string               :as str]
   [clojure.walk                 :as walk :refer [postwalk]]
   [taoensso.timbre              :as log])
  #?(:clj (:import [datahike.db DB])))

;;; ================== CLJ/CLJS/SCI Interop =========================
(defn regex? [o]
  #?(:clj  (instance? java.util.regex.Pattern o)
     :cljs (instance? js/RegExp o)))

(defn read-str [s]
  #?(:clj  (read-string s)
     :cljs (cljs.reader/read-string s)))

#?(:clj
(defn db-atm? [o]
  (and (instance? clojure.lang.Atom o)
       (instance? datahike.db.DB @o))))

#?(:cljs (defn db-atm? [_o] true)) ; ToDo: Implement this.

#_(def log-info ^:sci/macro
  (fn [_&form _&env body]))

;;; ToDo: Why is SCI able to use this? Util isn't a sci-used ns.
;;; https://stackoverflow.com/questions/53321244/clojurescript-equivalent-of-re-matcher-and-re-groups
#?(:cljs
(defn grouper
  "Uses js/RegExp to find matching groups. Note that the JS value
   returned by `:last-index` is the index of the first char in the
   input string *after* the current match."
  [re input-str]
  (let [re-src re.source] ; the source string from the regexp arg
    (loop [groups []
           regexp (js/RegExp. re-src "g")] ; 'g' => global search
      (let [res     (.exec regexp input-str)
            res-clj (js->clj res)]
        (if (nil? res)
          groups
          (recur
            (conj groups {:groups res-clj :match (get res-clj 0)
                          :index res.index :input res.input
                          :last-index regexp.lastIndex})
            regexp)))))))

;;; ================== Ordinary Utils =========================

;;; ToDo: The problem with output to log/debug might have to do with *err* not defined in cljs.
(defn custom-output-fn
  " - I don't want :hostname_ and :timestamp_ in the log output preface text..
    - I don't want any preface text in rad-mapper.parse output."
  ([data] (custom-output-fn nil data))
  ([opts data]
   (if (=  (:?ns-str data) "rad-mapper.parse")
     (apply str (:vargs data)) ; So it can do simple indented call tracing.
     (taoensso.timbre/default-output-fn opts (dissoc data :hostname_ :timestamp_)))))

(defn config-log
  "Configure Timbre: set reporting levels and specify a custom :output-fn."
  [min-level]
  (if (#{:trace :debug :info :warn :error :fatal :report} min-level)
    (log/set-config!
     (-> log/*config*
         (assoc :output-fn #'custom-output-fn)
         (assoc :min-level [[#{"datahike.*"} :error]
                            [#{"datascript.*"} :error]
                            [#{"*"} min-level]])))
    (log/error "Invalid timbre reporting level:" min-level)))

(defn default-min-log-level
  []
  (->> log/*config* :min-level (some #(when (= #{"*"} (first %)) (second %)))))

(defn cljs? [] (if (find-ns 'cljs.core.Namespace) true false))

;;; ToDo: Refactor: This stuff belongs in the "messaging plug-in".
(defn nspaces
  "Return a string of n spaces."
  [n]
  (reduce (fn [s _] (str s " ")) "" (range n)))

;;;(default-name "data-" ["data-1" "foo-2"]) => "data-2")
(defn default-name
  "Return a unique sequenced (-1, -2) name using the base."
  [base-str used-names]
  (let [regex (re-pattern (str base-str "\\d+"))]
    (str base-str (->> (map str used-names) (filter #(re-matches regex %)) count inc))))

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

#?(:clj
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
      (postwalk
       (fn [obj]
         (if (and (map? obj) (contains? obj :tag))
           (update obj :tag equivalent-tag)
           obj))
       xml)))))

(defn clean-whitespace
  "Remove whitespace in element :content."
  [xml]
  (postwalk
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
        :else (throw (ex-info "Unknown type in detagify:" {:obj obj}))))

#?(:clj
(defn read-xml
  "Return a map of the XML file read."
  [pathname]
  (let [xml (-> pathname clojure.java.io/reader x/parse)]
     {:xml/ns-info (explicit-root-ns (x/element-nss xml))
      :xml/content (-> xml alienate-xml clean-whitespace detagify vector)
      :schema/pathname pathname})))

(defn trans-tag [tag]
  (if-let [ns (namespace tag)]
    (keyword (str ns "_" (name tag))) ; ToDo: nname
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
    (if (number-str? obj) (read-str obj) obj)
    (vector? obj) (mapv simplify-xml obj)
    (map? obj) (as-> {} ?r
                 (assoc ?r (trans-tag (:xml/tag obj)) (simplify-xml (:xml/content obj)))
                 (reduce-kv (fn [r key val] (assoc r (trans-tag key) (simplify-xml val)))
                            ?r
                            (:xml/attrs obj)))))

(defn string-permute
  "Return a lazy sequence of A, B, C,...Z, AA, AB,..."
  ([]  (string-permute "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  ([chars] (string-permute [""] chars))
  ([prev chars]
   (let [strs (mapcat (fn [c] (map (fn [s] (str c s)) prev)) chars)]
     (lazy-cat strs (string-permute strs chars)))))

;;; See https://clojuredocs.org/clojure.core/split-with
;; The following split-by builds on top of split-with. Instead of
;; splitting only the first time pred returns false, it splits (lazily)
;; every time it turns from true to false.
(defn split-by [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (let [[xs ys] (split-with pred s)]
        (if (seq xs)
          (cons xs (split-by pred ys))
          (let [!pred (complement pred)
                skip (take-while !pred s)
                others (drop-while !pred s)
                [xs ys] (split-with pred others)]
            (cons (concat skip xs)
                  (split-by pred ys))))))))

(def dg-cnt (atom 0))
(defn reset-dgensym! [] (reset! dg-cnt 0))
(defn dgensym! [] (->> (swap! dg-cnt inc) (str "_x") symbol))

(defn class-name
  "Return a keyword representing the class of the object.
  For example (class-name 1) ==> :Long. Class name is not namespace-qualified."
  [obj]
  (->> obj type str (re-matches #"^.+\.(.*)$") second keyword))

(def ^:private num-map "A map indexed by strings with values being the number the string represents."
  (let [nvec (into (range 1 20) (map #(* 10 %) (range 2 10)))]
    (zipmap (map #(cl-format nil "~r" %) nvec) nvec)))

(def ^:private num-word (-> num-map keys set))

;;; ToDo: Add :creative? an do 'word concatenation' for e.g. 'twenty twenty two', triple eight, double.
;;; Synonyms of zero = aught, cipher, goose egg, naught (also nought), nil, nothing, o, oh, zilch, zip.
(defn parse-num-string
  "Return the number represented by the argument English language string."
  [string]
  (let [units {"thousand" 1000, "million" 1000000, "billion" 1000000000,
               "trillion" 1000000000000, "quadrillion" 1000000000000000}
        s-parts (-> string str/lower-case (str/replace  #",|-| and" " ") (str/split #"\s+"))]
    (letfn [(add [nums] (if (== 1 (count nums)) (first nums) (apply + nums)))
            (trans-nums [u] (if (empty? u) 0 (-> (map #(get num-map %) u) add)))
            (translate [x] ; 'hundred' makes things a little harder....I think!
              (if (every? #(get num-word %) x)
                (trans-nums x)
                (let [[hunvec others] (split-with (complement #(= % "hundred")) x)
                      multiplier (if ((-> units keys set) (last x)) (get units (last x)) 1)]
                  (* (+ (if (empty? others) (trans-nums (butlast hunvec)) (* 100 (trans-nums hunvec)))
                        (trans-nums (if (get units (last others)) (-> others rest butlast) (rest others))))
                     multiplier))))]
      (->> (split-by (-> units keys set) s-parts)
           (map translate)
           add))))

(defn ln-seq
  "Implement line-seq interoperable for JS-hosted use.
   Pass-through to clojure.core/line-seq for Java-hosted."
  [s]
  #?(:cljs (seq (clojure.string/split-lines s))
     :clj  (line-seq s)))
