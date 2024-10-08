(ns rad-mapper.util
  (:require
   [clojure.pprint            :refer [cl-format]]
   [clojure.string            :as str]
   [mount.core                :refer [defstate]]
   [promesa.core              :as p]
   [taoensso.timbre           :as log]
   #?@(:clj
       [[clojure.data.json    :as json]
        [clojure.edn          :as edn]
        [clojure.java.io]
        [datahike.db.utils    :refer [db?]]
        [datahike.pull-api    :as dp]])
   #?@(:cljs
       [[cljs.reader]                       ; ToDo: Investigate. Not cljs version?
        [datascript.core      :refer [db?]]
        [datascript.pull-api :as dp]])))

;;; ================== CLJ/CLJS/SCI Interop =========================
(defn regex? [o]
  #?(:clj  (instance? java.util.regex.Pattern o)
     :cljs (instance? js/RegExp o)))

(defn read-str [s]
  #?(:clj  (edn/read-string s)
     :cljs (cljs.reader/read-string s)))

(defn remove-preamble
  "The LLM might put text and markup around the answer, return the answer without this crap."
  [response]
  (let [response (str/replace response #"\s" " ")]
    (if (re-matches #".*```clojure.*" response)
      (let [pos (str/index-of response "```clojure")
            response (subs response (+ pos 10))
            pos (str/index-of response "```")]
        (subs response 0 pos))
      response)))

(defn read-str-llm
  "(1) remove-preamble (which is ```clojure...)
   (2) Hope beyond hope? LLM occassionally has too many or too few parentheses."
  [s]
  (let [s (remove-preamble s)]
    (or (try (read-str s)
             (catch #?(:clj Exception :cljs :default) _e nil))
        (try (let [res (read-str (str s "}"))]
               (log/info "llm returns unbalanced (under-closed).")
               res)
             (catch #?(:clj Exception :cljs :default) _e nil))
        (ex-info "String cannot be read:" {:s s}))))

;;; ToDo: Useless; use .indexOf
(defn has-index [v elem]
  (let [len (count v)]
    (loop [ix 0]
      (cond (>= ix len) -1
            (= elem (nth v ix)) ix
            :else (recur (inc ix))))))

(defn exception?
  [x]
  (or
   #?(:clj  (instance? clojure.lang.ExceptionInfo x)
      :cljs (instance? cljs.core.ExceptionInfo x))
   (and (p/promise? x) (p/rejected? x))))

(def ^:dynamic *await-finalize* 45000) ; Sufficient for a tough LLM call.

(defn await-promise
  [obj]
  #?(:clj (p/await obj *await-finalize*)
     :cljs (do #_(log/warn "Awaiting on CLJS.") obj)))

(defn db-atm? [o] (db? o))

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

(defn json-pprint
  "Return the object as a pretty-printed string.
   These ignore namespaces (e.g. of keyword keys) so if those are to
   be preserved, the keys should be handled beforehand. See clj-key->rm-id below"
  [obj]
  #?(:clj  (with-out-str (json/pprint obj))
     :cljs (js/JSON.stringify (clj->js obj) nil 2)))

;;; ================== Ordinary Utils =========================
;;; ToDo: This isn't working.
(defn qvar? [obj] (and (symbol? obj) (str/starts-with? (name obj) "?")))

(defn unquote-qvars
  "Walk through the body replacing (quote <qvar>) with <qvar>.
   Rationale: In most situations we want qvars to be rewritten as quoted symbols.
   An exception is their use in the :where of a datalog query. There may be more usages."
  [form]
  (letfn [(unq [obj]
            (cond (map? obj)                   (reduce-kv (fn [m k v] (assoc m (unq k) (unq v))) {} obj)
                  (vector? obj)                (mapv unq obj)
                  (and (seq? obj)
                       (== 2 (count obj))
                       (= 'quote (first obj))
                       (-> obj second qvar?))   (second obj)
                  :else obj))]
    (unq form)))

(defn quote-qvars
  "Walk through the body replacing <qvar> with (quote <qvar>).
   Rationale: In most situations we want qvars to be rewritten as quoted symbols.
   An exception is their use in the :where of a datalog query. There may be more usages."
  [form]
  (letfn [(qq [obj]
            (cond (map? obj)                   (reduce-kv (fn [m k v] (assoc m (qq k) (qq v))) {} obj)
                  (vector? obj)                (mapv qq obj)
                  (qvar? obj)                  `(quote ~obj)
                  :else obj))]
    (qq form)))

(defn clj-key->rm-id
  "Walk the object replacing its keys with strings. Where the original key
   is namespaced, the string used is <namespace>.<name>; roughly what JSON would be." ; ToDo: Really? "."
  [obj]
  (cond (map? obj)       (reduce-kv (fn [m k v] (assoc m (clj-key->rm-id k) (clj-key->rm-id v))) {} obj)
        (vector? obj)    (mapv clj-key->rm-id obj)
        (keyword? obj)   (if-let [ns (namespace obj)] (str ns "_" (name obj)) (name obj))
        :else            obj))

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
         (assoc :min-level [[#{"RM-DEFAULT" "rad-mapper.*" "rm-server.*" "exerciser-app.*" "user"} min-level]
                            [#{"datahike.*"} :error]
                            [#{"datascript.*"} :error]
                            [#{"*"} :error]])))
     (log/error "Invalid timbre reporting level:" min-level)))

(defn default-min-log-level
  "Get the value of 'RM-DEFAULT' in (:min-level log/*config*), it designates
   the logging level for namespaces of rad-mapper, including rad-mapper,
   exerciser-app, and user."
  []
  (->> log/*config* :min-level (some #(when (contains? (first %) "RM-DEFAULT") (second %)))))


;;;(defn cljs? [] (if (find-ns 'cljs.core.Namespace) true false)) ; Not good for prod code.
(defn cljs?
  "Return true if this is executed in CLJS."
  []
  (if (resolve '*clojurescript-version*) true false))

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

;;; ToDo: If
(defn stringify-role
  "Turn a RM role (a keyword) into a string; if the argument isn't a keyword just return it."
  [role]
  (cond (and (or (keyword? role) (symbol? role))
             (not-empty (namespace role)))           (str (namespace role) "/" (name role)),
        (keyword? role)                              (name role),
        :else                                        role))

;;;======================= Next 4 are also in schema-db/{util,db_util}. ============== ToDo:
(defn box
  "Wrap the argument (an atomic value) in a box.
   Note that unlike unbox, this only accepts atomic values."
  [obj]
  (cond (string?  obj) {:box/string-val  obj},
        (number?  obj) {:box/number-val  obj},
        (keyword? obj) {:box/keyword-val obj},
        (boolean? obj) {:box/boolean-val obj}))

(defn unbox
  "Walk through the form replacing boxed data with the data.
   In the reduce DB, for simplicity, all values are :db.type/ref."
  [data]
  (letfn [(box? [obj]
            (and (map? obj)
                 (#{:box/string-val :box/number-val :box/keyword-val :box/boolean-val}
                  (-> obj seq first first))))  ; There is just one key in a boxed object.
          (ub [obj]
            (if-let [box-typ (box? obj)]
              (box-typ obj)
              (cond (map? obj)      (reduce-kv (fn [m k v] (assoc m k (ub v))) {} obj)
                    (vector? obj)   (mapv ub obj)
                    :else           obj)))]
    (ub data)))

;;; This seems to cause problems in recursive resolution. (See resolve-db-id)"
(defn db-ref?
  "It looks to me that a datahike ref is a map with exactly one key: :db/id."
  [obj]
  (and (map? obj) (= [:db/id] (keys obj))))

;;; {:db/id 3779}
(defn resolve-db-id
  "Return the form resolved, removing properties in filter-set,
   a set of db attribute keys, for example, #{:db/id}."
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

;;; ToDo:
;;;  - cljs complains about not finding x/element-nss, which I don't see in the  0.2.0-alpha8 source at all.
;;;    (Yet it does work in clj!) I suppose reading xml isn't something I need in cljs, but it would be
;;;    nice to know what is going on here.
;;; ToDo: Get some more types in here, and in implementation generally.
(defn db-type-of
  "Return a Datahike schema :db/valueType object for the argument"
  [obj]
  (cond (string? obj)  :db.type/string
        (number? obj)  :db.type/number
        (keyword? obj) :db.type/keyword
        (map? obj)     :db.type/ref
        (boolean? obj) :db.type/boolean))

(def max-duration
  "This is used in places where doing set-clock might not make sense."
  30000)

(def timeout-info "Used for calls to cljs-ajax and progress bar."
  (atom {:valid? false :max-millis max-duration :start-time nil :timeout-at nil}))

(defn invalidate-timeout-info
  []
  (swap! timeout-info #(assoc % :valid? false)))

(defn start-clock
  "Set the timeout-info object and return the argument."
  ([] (start-clock max-duration))
  ([max-millis]
   (swap! timeout-info
          #(let [now #?(:clj (inst-ms (java.util.Date.)) :cljs (.getTime (js/Date.)))]
             (assoc % :valid? true :max-millis max-millis :start-time now :timeout-at (+ now max-millis))))
   max-millis))

;;;=============================================================================================================
;;; Utils for macros: It seems the CLJS macros file cannot have these in them! See javascript.org [2023-01-25].
;;;=============================================================================================================
;;; --- rewrite
(defn rewrite-dispatch [tag _ & _] tag)
(defmulti rewrite-meth #'rewrite-dispatch)

(defstate logging
  :start
  (do (config-log :info)))
