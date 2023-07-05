(ns rad-mapper.builtin
  "Built-in functions implementing the expression language of the mapping language.
   Functions with names beginning with a '$' are available to the user (e.g. $filter).
   Others (such as bi/key and bi/strcat) implement other parts of the expression language
   but are not available directly to the user except through the operators (e.g. the dot, and &
   respectively for navigation to a property and concatenation).

   This is a big file in part because of Javascript's inability to allow the sorts of operations
   on namespaces that are possible in Java/Clojure. Thus, for example when it was useful to
   be able to call processRM, it had to be done here, not in something that :requires this,
   because this file :requires pretty much all of rad-mapper.
   Specifically, all the Small Clojure Interpreter stuff is done right here.

   N.B. LSP annotates many operators here as '0 references'; of course, they are used."
  (:refer-clojure :exclude [loop])
  (:require
   [clojure.core :as c]
   #?@(:clj  [[clojure.data.json            :as json]
              [clojure.data.codec.base64    :as b64]
              [dk.ative.docjure.spreadsheet :as ss]
              [datahike.api                 :as d]
              [datahike.pull-api            :as dp]
              [muuntaja.core                :as m]
              [rad-mapper.resolvers         :as schema :refer [pathom-resolve]]
              [rad-mapper.codelib           :as codelib]
              [wkok.openai-clojure.api      :as openai]]
       :cljs [[ajax.core :refer [GET POST]]
              [datascript.core           :as d]
              [datascript.pull-api       :as dp]
              ["nata-borrowed"           :as nb] ; ToDo: Replaces this with cljs-time, which wraps goog.time
              [goog.crypt.base64         :as jsb64]
              [rad-mapper.promesa-config :as scip]])
   [cemerick.url                    :as url]
   [camel-snake-kebab.core          :as csk]
   [clojure.spec.alpha              :as s]
   [clojure.pprint                           :refer [cl-format pprint]]
   [clojure.string                  :as str  :refer [index-of]]
   [clojure.walk                    :as walk :refer [keywordize-keys]]
   [promesa.core                    :as p]
   [rad-mapper.parse                :as par]
   [rad-mapper.parse-macros         :as pm]
   [rad-mapper.query                :as qu]
   [rad-mapper.rewrite              :as rew]
   [rad-mapper.rewrite-macros       :as rewm]
   [rad-mapper.util                 :as util :refer [qvar? box unbox start-clock exception? rm-id->clj-key]]
   [sci.core                        :as sci]
   [taoensso.timbre                 :as log :refer-macros[error debug info log!]]
   [rad-mapper.builtin-macros       :as bim
    :refer [$ $$ set-context! defn* value-step-m primary-m init-step-m map-step-m
            threading? jflatten containerize containerize? container? flatten-except-json]])
  #?(:cljs (:require-macros [rad-mapper.builtin-macros]
                            [promesa.core :refer [loop]])) ; ToDo: probably not necessary.
   #?(:clj
      (:import java.text.DecimalFormat
               java.text.DecimalFormatSymbols
               java.math.BigDecimal
               java.math.MathContext
               java.math.RoundingMode
               java.time.format.DateTimeFormatter
               java.time.Instant
               java.time.LocalDateTime
               java.time.ZonedDateTime ; /Where possible, it is recommended to use a simpler class without a time-zone./
               java.time.ZoneId
               java.time.ZoneOffset)))

(declare aref)

(def diag (atom nil))

(s/def ::number number?)
(s/def ::pos-number (s/and number? pos?))
(s/def ::integer integer?)
(s/def ::string string?)
(s/def ::pos-limit (s/or :number #(and (number? %) (pos? %)) :unlimited #{:unlimited}))
(s/def ::limit (s/or :number number? :unlimited #{:unlimited}))
(s/def ::str|regex (s/or :string string? :regex util/regex?))
(s/def ::non-zero (s/and number? #(-> % zero? not)))
(s/def ::vector vector?)
(s/def ::numbers (s/and vector? (s/coll-of ::number :min-count 1)))
(s/def ::strings (s/and vector? (s/coll-of ::string :min-count 1)))
(s/def ::vectors (s/and vector? (s/coll-of ::vector :min-count 1)))
(s/def ::objects (s/and vector? (s/coll-of map? :min-count 1)))
(s/def ::radix (s/and number? #(<= 2 % 36)))
(s/def ::fn fn?)

;;; AFAICS, I don't see how I can allow port to be one thing when this file is used in a library,
;;; and another when I'm testing RM in isolation.
(def svr-prefix "http://localhost:3000")

(defn handle-builtin
  "Generic handling of errors for built-ins"
  [e]
  (log/error (:message e))
  e)

(def now (atom nil))

(defn get-set-now
  "Set the time if it hasn't been set in this evaluation."
  []
  (or @now
      (let [new-now #?(:clj (Instant/now) :cljs (js/Date.))] ; cljs is a #inst, clj is java.time.Instant.
        (reset! now
                {:instant  new-now
                 :millis #?(:clj  (.toEpochMilli (Instant/now))
                            :cljs (long new-now))}))))

(defn reset-env
  "Clean things up just prior to running user code."
  ([] (reset-env nil))
  ([context]
   (set-context! context)
   (reset! now nil) ; Don't set time until it is asked for (with $now() or $millis()).
   (reset! $$ :bi/unset)))

(defn cmap
  "If the object isn't a container, run the function on it,
   otherwise, mapv over the argument and containerize the result."
  [f arg]
  (if (container? arg)
    (->> (mapv #(binding [$ (atom %)] (f %)) arg)
         containerize)
    (f arg)))

(defn singlize [v] (if (vector? v) v (vector v)))

(defn finalize
  [obj]
  (letfn [(fin [obj]
            (cond (map? obj)              (let [m (meta obj)]
                                            (-> (reduce-kv (fn [m k v] (assoc m (fin k) (fin v))) {} obj) (with-meta m)))
                  (vector? obj)           (let [m (meta obj)] (-> (mapv fin obj) (with-meta m)))
                  (p/promise? obj)        (util/await-promise obj)
                  :else                   obj))]
    (-> obj fin jflatten)))

(defn deref$
  "Dereference the $ atom.
   Expressions such as [[1,2,3], [1]].$ will translate to (bi/run-steps [[1 2 3] [1]] (deref bi/$))
   making it advantageous to have a deref that sets the value and returns it."
  ([] (containerize? @$))
  ([val] ; ToDo: Is this one ever used?
   (set-context! (containerize? val))))

;;;========================= JSONata built-ins  =========================================
(defn add      "plus"   [x y] (+ x y))
(defn subtract "minus"  [x y] (- x y))
(defn multiply "times"  [x y] (* x y))
(defn div      "divide" [x y] (s/assert ::non-zero y) (double (/ x y)))  ; cljs.core/divide
(defn gt       "greater-than"          [x y] (>  x y))
(defn lt       "less-than "            [x y] (<  x y))
(defn gteq     "greater-than-or-equal" [x y] (>= x y))
(defn lteq     "less-than-or-equal"    [x y] (<= x y))
(defn eq       "equal, need not be numbers"     [x y] (= (jflatten x) (jflatten y)))
(defn !=       "not equal, need not be numbers" [x y] (not= (jflatten x) (jflatten y)))

(declare $string)

(defn thread
  "Apply the function to the object."
  [obj func]
  (log/info "func = " func)
  (if (p/promise? obj)
    (-> obj
        (p/then #(func %))
        (p/catch #(ex-info (str "In bi/thread: " %) {:obj obj :func func :err %})))
    (try
      (func obj)
      (catch #?(:clj Exception :cljs :default) e
        (ex-info "In bi/thread (ordinary):" {:obj obj :func func :err e})))))

(defn step-type
  "Return a keyword describing what type of step to take next."
  [steps]
  (let [step-one (-> steps first  meta :bi/step-type)
        step-two (-> steps second meta :bi/step-type)]
    (if (and (= :bi/get-step step-one) (= :bi/filter-step step-two))
      :bi/get-filter ; The special non-compositional one.
      (#{:bi/init-step :bi/filter-step :bi/get-step :bi/value-step :bi/primary :bi/map-step} step-one))))

;;; ------------------ Path implementation ---------------------------------
(defn run-steps
  "Run or map over each path step function, passing the result to the next step."
  [& steps]
  ;; ToDo: Wrap next in dynamic *debug-eval?* or some such thing.
  ;;(log/info "--- run-steps ---")
  (binding [$ (atom @$)] ; Make a new temporary context that can be reset in the steps.
    (c/loop
      [steps steps
       res   @$]
      (if (empty? steps) res
          (let [styp (step-type steps)
                sfn  (first steps)
                new-res (case styp ; init-step, value-step, map-step, #_thread, and primary use SCI's notion of macros.
                          :bi/init-step    (-> (sfn @$) containerize?),
                          :bi/get-filter   ((second steps) res {:bi/prior-step-type :bi/get-step
                                                                :bi/attr (-> sfn meta :bi/arg)}),
                          :bi/filter-step  (sfn res nil), ; containerizes arg; will do (-> (cmap aref) jflatten) | filterv
                          :bi/get-step     (sfn res nil), ; containerizes arg if not map; will do cmap or map get.
                          :bi/value-step   (if (vector? res)
                                             (mapv #(binding [$ (atom %)] (sfn $)) res)
                                             (sfn res)),
                          :bi/primary      (if (vector? res) (cmap sfn (containerize? res)) (sfn res)),
                          :bi/map-step     (cmap sfn (containerize? res)), ; get-step is a function; this is a macro; it just executes its body.
                          (throw (ex-info "Invalid step" {:sfn  sfn})))]
            ;; ToDo: Wrap next in dynamic *debug-eval?* or some such thing.
            ;;(log/info (cl-format nil "    styp = ~S meta = ~S res = ~S" styp (-> sfn meta (dissoc :bi/step-type)) new-res))
             (recur
              (if (= styp :bi/get-filter) (-> steps rest rest) (rest steps))
              (set-context! new-res)))))))

;;; The spec's viewpoint on 'non-compositionality': The Filter operator binds tighter than the Map operator.
;;; This means, for example, that books.authors[0] will select the all of the first authors from each book
;;; rather than the first author from all of the books. http://docs.jsonata.org/processing

;;; ToDo: You can specify multiple conditions (a disjunction)  in filtering http://docs.jsonata.org/processing
;;; ToDo: There is likely to be more to do here owing to promises.
#_(defn filter-step
  "Performs array reference or predicate application (filtering) on the arguments following JSONata rules:
    (1) If the expression in square brackets (second arg) is non-numeric, or is an expression that doesn't evaluate to a number,
        then it is treated as a predicate.
    (2) See aref below.
    (3) If no index is specified for an array (i.e. no square brackets after the field reference), then the whole array is selected.
        If the array contains objects, and the location path selects fields within these objects, then each object within the array
        will be queried for selection. [This rule has nothing to do with filtering!]"
  [pred|ix-fn]
  (-> (fn filter-step [obj prior-step]
        (letfn [(fbody [prix]
                  (let [ob (if (= :bi/get-step (:bi/prior-step-type prior-step))
                             (let [k (:bi/attr prior-step)] ; non-compositional semantics
                               (if (vector? obj)
                                 (cmap #(get % k) (containerize obj))
                                 (get obj k)))
                             obj)]
                    (if (number? prix)   ; Array behavior. Caller will map over it.
                      (let [ix (-> prix Math/floor int) ; Really! I checked!
                            m (meta obj)]
                        (if (or (:bi/json-array? m) (:bi/b-set? m))
                          (aref ob ix)
                          (-> (cmap #(aref % ix) ob) jflatten)))
                      (as-> ob ?o          ; Filter behavior.
                        (singlize ?o)
                        (-> (filterv #(binding [$ (atom %)] (pred|ix-fn %)) ?o) containerize?)))))]
        (let [prix (pred|ix-fn @$)] ; If it returns a number, it is indexing.
          (cond (number? prix)     (fbody prix)
                (p/promise? prix)  (-> prix
                                       (p/then #(fbody %))
                                       (p/catch #(ex-info (str "In bi/filter-step: " %) {:prix prix :err %})))
                :else              (fbody prix))))) ; Not sure this one happens!
      (with-meta {:bi/step-type :bi/filter-step})))

(defn filter-step
  "Performs array reference or predicate application (filtering) on the arguments following JSONata rules:
    (1) If the expression in square brackets (second arg) is non-numeric, or is an expression that doesn't evaluate to a number,
        then it is treated as a predicate.
    (2) See aref below.
    (3) If no index is specified for an array (i.e. no square brackets after the field reference), then the whole array is selected.
        If the array contains objects, and the location path selects fields within these objects, then each object within the array
        will be queried for selection. [This rule has nothing to do with filtering!]"
  [pred|ix-fn]
  (-> (fn filter-step [obj prior-step]
        (let [prix (try (pred|ix-fn @$) (catch #?(:clj Exception :cljs :default) _e nil))
              ob (if (= :bi/get-step (:bi/prior-step-type prior-step))
                   (let [k (:bi/attr prior-step)] ; non-compositional semantics
                     (if (vector? obj)
                       (cmap #(get % k) (containerize obj))
                       (get obj k)))
                   obj)]
          (if (number? prix)   ; Array behavior. Caller will map over it.
            (let [ix (-> prix Math/floor int) ; Really! I checked!
                  m (meta obj)]
              (if (or (:bi/json-array? m) (:bi/b-set? m))
                (aref ob ix)
                (-> (cmap #(aref % ix) ob) jflatten)))
            (as-> ob ?o          ; Filter behavior.
              (singlize ?o)
              (-> (filterv #(binding [$ (atom %)] (pred|ix-fn %)) ?o) containerize?)))))
      (with-meta {:bi/step-type :bi/filter-step})))

(defn get-step
  "Perform the mapping activity of the 'a' in $.a, for example.
   This function is called with the state object."
  [k]
  (-> (fn get-step [& args] ; No arg if called in a primary.
        (let [obj (cond (-> args first p/promise?) (first args)
                        (-> args first empty?)     (first args)
                        :else @$)]
          (cond (p/promise? obj) (-> obj
                                     (p/then #(get-step %))
                                     (p/catch #(ex-info (str "In bi/get-step: " %) {:k k :err %})))
                (map? obj)       (get obj k)
                (vector? obj)    (->> obj
                                      containerize
                                      (cmap #(get % k))
                                      ;; lightweight flatten
                                      (reduce (fn [res x] (if (vector? x) (into res x) (conj res x))) [])
                                      containerize)
                :else            nil)))
      (with-meta {:bi/step-type :bi/get-step :bi/arg k})))

#_(def value-step ^:sci/macro
  (fn [_&form _&env body]
      `(-> (fn [& ignore#] ~body)
           (with-meta {:bi/step-type :bi/value-step :body '~body}))))

(defn get-scoped
  "Access map key like clj/get, but with arity overloading for $.

   Don't do a set-context! here; it will mess up 'distribution'.
   For example, setting $ to the value of 'c' in a.b.(c + f) is wrong."
  ([k] (get-scoped @$ k))
  ([obj k] (get obj k)))

#_(def primary ^:sci/macro
  (fn [_&form _&env body]
    `(-> (fn [& ignore#] ~body)
         (with-meta {:bi/step-type :bi/primary}))))

#_(def init-step ^:sci/macro
  (fn [_&form _&env body]
    `(-> (fn [_x#] ~body)
         (with-meta {:bi/step-type :bi/init-step :bi/body '~body}))))

#_(def map-step ^:sci/macro
  (fn [_&form _&env body]
    `(-> (fn [_x#] ~body)
         (with-meta {:bi/step-type :bi/map-step :body '~body}))))

;;; Implements the JSONata-like <test> ? <then-exp> <else-exp>."
#_(def conditional ^:sci/macro
  (fn [_&form _&env condition e1 e2]
    `(let [cond# ~condition
           answer# (if (fn? cond#) (cond#) cond#)]

       (cond (or (and (fn? cond#) (cond#))
                 (and (not (fn? cond#)) cond#))   (let [res# ~e1]
                                                    (if (fn? res#) (res#) res#))
             :else                                (let [res# ~e2]
                                                    (if (fn? res#) (res#) res#))))))

(defn aref
  "Negative indexes count from the end of the array, for example, arr[-1] will select the last value,
   arr[-2] the second to last, etc.
   If an index is specified that exceeds the size of the array, then nothing is selected."
  [obj ix]
  (let [len (if (vector? obj) (count obj) 1)
        ix  (if (neg? ix) (+ len ix) ix)]
    (if (or (and (pos? ix) (>= ix len))
            (and (neg? ix) (> (Math/abs ix) len)))
      nil ; Rule 2, above.
      (if (vector? obj) (nth obj ix) obj))))

;;; rewrite.clj would have been a good place for this, but here we need it at runtime!
;;; ToDo: sym-bi-access IS executed for $map in rewrite. So can that for filter too?
(defn sym-bi-access
  "When doing mapping such as '$.(A * B)' the expressions A and B
   were rewritten (bi/key 'A'). There needs to be a first argument
   to that, before the 'A'. This function inserts the argument symbol
   in such forms so that they can be wrapped in (fn [<that symbol>] ...)
   and called in map/filter/reduce settings."
  [form sym]
  (letfn [(sba-aux [form]
            (let [m (meta form)]
              (-> (cond (map? form) (reduce-kv (fn [m k v] (assoc m k (sba-aux v))) {} form)
                        (vector? form) (mapv sba-aux form)
                        (seq? form) (cond (and (= (first form) 'bi/key) (== 2 (count form)))
                                          (list 'bi/key sym (second form))
                                          (= form '(deref rad-mapper.builtin-macros/$)) sym
                                          :else (map sba-aux form))
                        :else form)
                  (with-meta m))))]
    (sba-aux form)))

;;; ToDo: Put some code around assignments so that you know the 'name' of the function
;;;       that is failing the arg count requirement.
#_(defn fncall
  [{:keys [func args]}]
  (if-let [cnt (-> func meta :bi/expected-arg-cnt)]
    (if (== cnt (count args))
      (apply func args)
      (throw (ex-info (cl-format nil "A function of type ~A expected ~A args; it got ~A."
                                 (-> func meta :bi/fn-type) cnt (count args)) {})))
    (apply func args)))

(defn fncall
  [{:keys [func args]}]
  (when-let [cnt (-> func meta :bi/expected-arg-cnt)]
    (when (not= cnt (count args))
      (throw (ex-info (cl-format nil "A function of type ~A expected ~A args; it got ~A."
                                 (-> func meta :bi/fn-type) cnt (count args)) {}))))
  (if (some p/promise? args)
    (-> args
        p/all
        (p/then #(apply func %))
        (p/catch #(ex-info (str "In bi/fncall: " %) {:args args :func func :err %})))
    (apply func args)))

;;;--------------------------- JSONata built-in functions ------------------------------------

;;;------------- String --------------
(defn concat-op
  "JSONata & operator."
  [s1 s2]
  (s/assert ::string s1)
  (s/assert ::string s2)
  (str s1 s2))

;;; ToDo: Investigate problem with defn* on the next few.
;;; $base64decode
(defn* $base64decode
  "Converts base 64 encoded bytes to a string, using a UTF-8 Unicode codepage."
  [c_]
#?(:clj  (-> c_ .getBytes b64/decode String.)
   :cljs (jsb64/decodeString c_)))

;;; $base64encode
(defn* $base64encode
  "Converts an ASCII string to a base 64 representation.
   Each each character in the string is treated as a byte of binary data.
   This requires that all characters in the string are in the 0x00 to 0xFF range,
   which includes all characters in URI encoded strings.
   Unicode characters outside of that range are not supported."
  [s_]
   #?(:clj  (-> s_ .getBytes b64/encode String.)
      :cljs (jsb64/encodeString s_)))

;;;  ToDo: (generally) when arguments do not pas s/assert, need to throw an error.
;;;        For example, "Argument 1 of function "contains" does not match function signature."
;;;        This could be built into defn$ and defn$ could be generalized and used more widely.
;;; $contains
(defn* $contains
  "Returns true if str is matched by pattern, otherwise it returns false.
   If str is not specified (i.e. this function is invoked with one argument),
   then the context value is used as the value of str.

   The pattern parameter can either be a string or a regular expression (regex).
   If it is a string, the function returns true if the characters within pattern are
   contained contiguously within str.
   If it is a regex, the function will return true if the regex matches the contents of str."
  [s_ pat]
  (s/assert ::string s_)
  (if (util/regex? pat)
    (if (re-find pat s_) true false)
    (if (index-of s_ pat) true false)))

;;; $decodeUrl
(defn* $decodeUrl
  "Decode the url" ; ToDo: A lot of these doc-strings are bogus.
  [url-string_]
  (s/assert ::string url-string_)
  (url/url-decode url-string_))

;;; $decodeUrlComponent
(defn* $decodeUrlComponent
  "Decode the url component"
  [s_]
  (s/assert ::string s_)
  (url/url-decode s_))

;;; $encodeUrl
(defn* $encodeUrl
  "Encode the stringa as url"
  [s_]
  (s/assert ::string s_)
  (-> s_ url/url str))

;;; $encodeUrlComponent
(defn* $encodeUrlComponent
  "Encode the URL component"
  [s_]
  (s/assert ::string s_)
  (url/url-encode s_))

;;; $eval
#?(:clj
(defn $eval
  ([]  (partial $eval))
  ([s] ($eval s @$))
  ([s context]
   (s/assert ::string s)
   (let [rewrite (ns-resolve 'rad-mapper.rewrite 'processRM)
         form (rewrite :ptag/exp s :rewrite? true)]
     (binding [*ns* (find-ns 'user)]
       (try
         (reset-env context)
         (let [res (eval form)]
           (if (and (fn? res) (= :bi/primary (-> res meta :bi/step-type)))
             (jflatten (res))
             (jflatten res)))
         (catch Throwable e (str "$eval failed:" (.getMessage e)))))))))

;;; $join
(defn $join
  "Joins an array of component strings into a single concatenated string with each component string
   separated by the optional separator parameter.
   It is an error if the input array contains an item which isn't a string.
   If separator is not specified, then it is assumed to be the empty string, i.e. no separator between
   the component strings. It is an error if separator is not a string."
  ([] (partial $join))
  ([strings]
   (s/assert ::strings strings)
   (apply str strings))
  ([strings separator]
   (s/assert ::strings strings)
   (s/assert ::string separator)
   (->> strings
        (interpose separator)
        (apply str))))

;;; $length
(defn* $length
  "Return the length of the string."
  [s_]
  (s/assert ::string s_)
  (count s_))

;;; $lowercase
(defn* $lowercase
  "Returns a string with all the characters of str converted to lowercase.
   If str is not specified (i.e. this function is invoked with no arguments),
   then the context value is used as the value of str. An error is thrown if str is not a string."
  [s_]
  (s/assert ::string s_)
  (str/lower-case s_))

;;; ToDo: match is in nata-borrowed. Should I use it?
;;; $match
#?(:cljs
(defn $match
  ([pattern] ($match @$ pattern :unlimited))
  ([p1 p2] (if (util/regex? p1) ($match @$ p1 p2) ($match p1 p2 :unlimited)))
  ([s pattern _limit] ; ToDo Implement limit (looks easy in grouper)
   (let [s (if (keyword? s) ; query-roles are allowed.
             (if (namespace s) (str (namespace s) "/" (name s)) (name s))
             s)
         result (as-> (util/grouper pattern s) ?r
                  (map #(dissoc % :last-index :input) ?r)
                  (map #(update % :groups (fn [x] (-> x rest vec))) ?r) ; ToDo: Investigate why rest here.
                  (mapv #(update-keys % name) ?r)
                  (with-meta ?r {:bi/regex-result? true}))]
     ;; ToDo: Or should I jflatten (which currently doesn't change things)?
     (cond
       (empty? result) nil,
       (and (vector? result) (== 1 (count result))) (first result),
       :else result)))))

;;; ToDo: MAYBE write a clojure util/grouper
#?(:clj
(defn $match
  "Applies the str string to the pattern regular expression and returns an array of objects,
   with each object containing information about each occurrence of a match withing str.
   The object contains the following fields:
      * match - the substring that was matched by the regex.
      * index - the offset (starting at zero) within str of this match.
      * groups - if the regex contains capturing groups (parentheses), this contains an
                array of strings representing each captured group.
   If str is not specified, then the context value is used as the value of str.
   It is an error if str is not a string."
  ([pattern] ($match @$ pattern :unlimited))
  ([p1 p2] (if (util/regex? p1) ($match @$ p1 p2) ($match p1 p2 :unlimited)))
  ([s pattern limit]
   ;(s/assert (s/or ::string keyword?) s)
   ;(s/assert ::regex pattern)
   ;(s/assert ::pos-limit limit)
   (let [s (if (keyword? s) ; query-roles are allowed.
             (if (namespace s) (str (namespace s) "/" (name s)) (name s))
             s)
         matcher (re-matcher pattern s)
         result (c/loop [res []
                       adv 0
                       lim limit]
                  (let [m (re-find matcher)
                        match (if (string? m) m (first m))
                        groups (if (string? m) [] (-> m rest vec))]
                    (cond (and (number? limit) (zero? lim)) res
                          (empty? match) res
                          :else
                          (let [ix (+ adv (or (index-of (subs s adv) match) 0))]
                            (recur
                             (conj res (-> {"match" match, "index" ix, "groups" (vec groups)}
                                           (with-meta {:bi/regex-result? true})))
                             (+ adv (count match))
                             (if (number? lim) (dec lim) lim))))))]
     ;; ToDo: Or should I jflatten (which currently doesn't change things)?
     (cond
       (empty? result) nil,
       (and (vector? result) (== 1 (count result))) (first result),
       :else result)))))

;;; JSONata looks like it uses a matcher:
;;;       "Product thing" ~> /^Product/
;;;       Returns
;;;             {"match": "Product",
;;;              "start": 0,
;;;               "end": 7,
;;;               "groups": [],
;;;               "next": "<native function>#0"} <===============
;;;
;;; So I'm going to do something similar.
;;; ToDo: It seems like there are lots of opportunities to use defn* where I'm not using it. Should I care?
#?(
:cljs
   (defn match-regex [& args] :nyi) ; ToDo: re-matcher differs in CLJS
:clj
(defn match-regex
  "Return the match object (map with bi/regex-result? true) for
   This is similar to $match only in as far as they both do matching on a regular expressions;
   the map returned is different, and it does not use clojure.string/re-find."
  ([pattern] (match-regex @$ pattern))
  ([s pattern]
   (let [matcher (re-matcher pattern s)
         m (re-find matcher)
         match (if (string? m) m (first m))
         groups (if (string? m) [] (-> m rest vec))]
     (when match
       (let [ix (index-of s match)]
         (-> {"match" match
              "start" ix
              "end"   (+ ix (count match))
              "groups" groups
              "next" matcher}
             (with-meta {:bi/regex-result? true}))))))))

;;; $pad
(defn $pad
  "Returns a copy of the string str with extra padding, if necessary, so that its
   total number of characters is at least the absolute value of the width parameter.
   If width is a positive number, then the string is padded to the right;
   if negative, it is padded to the left.
   The optional char argument specifies the padding character(s) to use.
   If not specified, it defaults to the space character."
  ([s width] ($pad s width " "))
  ([s width char]
   (s/assert ::string s)
   (s/assert ::string char)
   (assert (== 1 (count char)))
   (s/assert ::integer width)
   (let [len (count s)
         awidth (abs width)]
     (if (>= len awidth)
       s
       (let [extra (->> (repeat (- awidth len) char) (apply str))]
         (if (pos? width)
           (str s extra)
           (str extra s)))))))

;;; $replace
(defn $replace
  "Finds occurrences of pattern within str and replaces them with replacement.

   If str is not specified, then the context value is used as the value of str.
   It is an error if str is not a string.

   The pattern parameter can either be a string or a regular expression (regex).
   If it is a string, it specifies the substring(s) within str which should be replaced.
   If it is a regex, its is used to find [a match].

   The replacement parameter can either be a string or a function. If it is a string,
   it specifies the sequence of characters that replace the substring(s) that are matched by pattern.
   If pattern is a regex, then the replacement string can refer to the characters that were matched by
   the regex as well as any of the captured groups using a $ followed by a number N:

     * If N = 0, then it is replaced by substring matched by the regex as a whole.
     * If N > 0, then it is replaced by the substring captured by the Nth parenthesised group in the regex.
     * If N is greater than the number of captured groups, then it is replaced by the empty string.
     * A literal $ character must be written as $$ in the replacement string

   If the replacement parameter is a function, then it is invoked for each match occurrence of the pattern regex.
   The replacement function must take a single parameter which will be the object structure of a regex match as
   described in the $match function; and must return a string.

   The optional limit parameter, is a number that specifies the maximum number of replacements to make before stopping.
   The remainder of the input beyond this limit will be copied to the output unchanged.

   Example: $replace('265USD', /([0-9]+)USD/, '$$$1') ==> '$256'
   (I don't see why this doesn't return $$256 but both str/replace and JSONata return $256!)"
  ;; ToDo: (clojure.string/replace "265USD" #"([0-9]+)USD" "$$$1") works in CLJS
  ;;       (clojure.string/replace "265USD" #"([0-9]+)USD" "$$$1") DOES NOT WORK in Clojure.
  ;;       (clojure.string/replace "265USD" #"([0-9]+)USD" "$1")   DOES WORK in Clojure.
  ;;  It gets a "illegal group reference because it thinks $$ is a group reference.
  ([pattern replacement]   ($replace @$ pattern replacement :unlimited))
  ([s pattern replacement] ($replace s  pattern replacement :unlimited))
  ([s pattern replacement limit]
   (s/assert ::string s)
   (s/assert ::str|regex pattern)
   (s/assert ::pos-limit limit)
   (let [lim  (if (number? limit) (-> limit Math/floor int) limit)]
     (cond (string? replacement)
           (if (= :unlimited lim) ; Like JSONata, Clojure uses $1 type syntax for replacement.
             (str/replace s pattern replacement)
             ;; Next line is to handle difference between a literal $ and a variable (e.g. $1)???
             (let [repl (str/replace replacement "$$" "\\$")]
               (reduce (fn [res _i] (str/replace-first res pattern repl))
                       s
                       (if (= :unlimited lim) (-> s count range) (range lim))))),
           (fn? replacement)
           (reduce (fn [res _i]
                     (try (let [repl (-> ($match res pattern) replacement)]
                            (str/replace-first res pattern repl))
                          ;; ToDo: Need a better way! See last test of $replace in builtin_test.clj
                          (catch #?(:clj Exception :cljs :default) _e res)))
                   s
                   (if (= :unlimited lim) (-> s count range) (range lim))), ; ToDo: (-> s count range) is a guess.
           :else (throw (ex-info "Replacement pattern must be a string or function" {:rep replacement}))))))

;;; $split
(defn $split
  "Splits the str parameter into an array of substrings.
   If str is not specified, then the context value is used as the value of str.
   It is an error if str is not a string.

   The separator parameter can either be a string or a regular expression (regex).
   If it is a string, it specifies the characters within str about which it should be split.
   If it is the empty string, str will be split into an array of single characters.
   If it is a regex, it splits the string around any sequence of characters that match the regex.

   The optional limit parameter is a number that specifies the maximum number of substrings to include
   in the resultant array. Any additional substrings are discarded. If limit is not specified, then str
   is fully split with no limit to the size of the resultant array.
   It is an error if limit is not a non-negative number."
  ([separator]   ($split @$ separator))
  ([s separator] ($split s separator :unlimited))
  ([s separator limit]
   (s/assert ::string s)
   (s/assert ::str|regex separator)
   (s/assert ::pos-limit limit)
   (let [lim (if (number? limit) (-> limit Math/floor int) limit)
         regex (if (string? separator) (re-pattern separator) separator)]
     (if (= lim :unlimited)
       (str/split s regex)
       (-> (str/split s regex) (subvec 0 lim))))))

;;; $string
(defn* $string
  "Return the argument as a string."
  [s_]
  (str s_))

;;; $substring
(defn $substring
  "Returns a string containing the characters in the first parameter str starting at position start (zero-offset).
   If str is not specified (i.e. this function is invoked with only the numeric argument(s)), then the context
   value is used as the value of str. An error is thrown if str is not a string.
   If length is specified, then the substring will contain maximum length characters.
   If start is negative then it indicates the number of characters from the end of str.
   See substr for full definition."
  ([start] ($substring @$ start))
  ([str start] (if (and (number? str) (number? start))
                 ($substring @$ str start)
                 ($substring str start :unlimited)))
   ([str start length]
    (s/assert ::string str)
    (s/assert ::limit start)
    (s/assert ::pos-limit length)
    (let [len (count str)
          res (if (neg? start)
                (subs str (+ len start))
                (subs str start))]
      (if (or (= :unlimited length) (> length len))
        res
        (subs res 0 length)))))

;;; $substringAfter
(defn $substringAfter
  "Returns the substring after the first occurrence of the character sequence chars in str.
   If str is not specified (i.e. this function is invoked with only one argument), then the
   context value is used as the value of str.
   If str does not contain chars, then it returns str.
   An error is thrown if str and chars are not strings."
  ([chars] ($substringAfter @$ chars))
  ([str chars]
   (if-let [ix (index-of str chars)]
     (subs str (inc ix))
     str)))

;;; $substringBefore
(defn $substringBefore
  "Returns the substring before the first occurrence of the character sequence chars in str.
   If str is not specified (i.e. this function is invoked with only one argument), then the
   context value is used as the value of str.
   If str does not contain chars, then it returns str.
   An error is thrown if str and chars are not strings."
  ([chars] ($substringBefore @$ chars))
  ([str chars]
   (if-let [ix (index-of str chars)]
     (subs str 0 ix)
     str)))

(defn $trim
  "Normalizes and trims all whitespace characters in str by applying the following steps:
      * All tabs, carriage returns, and line feeds are replaced with spaces.
      * Contiguous sequences of spaces are reduced to a single space.
      * Trailing and leading spaces are removed.
   If str is not specified (i.e. this function is invoked with no arguments), then the
  context value is used as the value of str. An error is thrown if str is not a string."
  ([] ($trim @$))
  ([s]
   (s/assert ::string s)
   (-> s (str/replace #"\s+" " ") str/trim)))

;;; $uppercase
(defn $uppercase
  "Returns a string with all the characters of str converted to uppercase.
   If str is not specified (i.e. this function is invoked with no arguments),
   then the context value is used as the value of str.
   An error is thrown if str is not a string."
  ([]  ($uppercase @$))
  ([s] (s/assert ::string s) (str/upper-case s)))

;;;------------- Numeric --------------
;;; $abs
(defn $abs
  "Returns the absolute value of the number parameter, i.e. if the number is negative,
   it returns the positive value. If number is not specified (i.e. this function is invoked
   with no arguments), then the context value is used as the value of number."
  ([] ($abs @$))
  ([num]
   (s/assert ::number num)
   (abs num)))

;;; $average - Not documented.
;;; Experimentation with JSONata Exerciser suggests it must take exactly one arg, a vector of numbers.
(defn $average
  "Take the average of the vector of numbers"
  [nums]
  (s/assert ::numbers nums)
  (/ (apply + nums)
          (-> nums count double)))

;;; $ceil
(defn $ceil
  "Returns the value of number rounded up to the nearest integer that is greater than or equal to number.
   If number is not specified (i.e. this function is invoked with no arguments), then the context value
   is used as the value of number."
  ([] ($ceil @$))
  ([num]
   (s/assert ::number num)
   (-> num Math/ceil long)))

;;; $floor
(defn $floor
  "Returns the value of number rounded down to the nearest integer that is smaller or equal to number.
   If number is not specified (i.e. this function is invoked with no arguments), then the context value
   is used as the value of number."
  ([] ($floor @$))
  ([num]
   (s/assert ::number num)
   (-> num Math/floor long)))

;;; $formatBase
(defn $formatBase
  "Casts the number to a string and formats it to an integer represented in the
   number base specified by the radix argument.
   If radix is not specified, then it defaults to base 10.
   radix can be between 2 and 36, otherwise an error is thrown."
  ([num] ($formatBase num 10))
  ([num radix]
   (s/assert ::radix num)
   (cl-format nil (str "~" radix "R") num)))

;;; https://java2blog.com/java-decimalformat/
;;; https://www.w3.org/TR/xpath-functions-31/#syntax-of-picture-string
;;; https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html

;;; | 0          | Number              | Yes | Digit                                                       |
;;; | #          | Number              | Yes | Digit, zero shows as absent                                 |
;;; | .          | Number              | Yes | Decimal separator or monetary decimal sepaator              |
;;; | \-         | Number              | Yes | Minus sign                                                  |
;;; | ,          | Number              | Yes | Grouping separator                                          |
;;; | E          | Number              | Yes | Separates mantissa and exponent in scientific notation. (1) |
;;; | ;          | Subpattern boundary | Yes | Separates positive and negative subpatterns                 |
;;; | %          | Prefix or suffix    | Yes | Multiply by 100 and show as percentage                      |
;;; | \u2030     | Prefix or suffix    | Yes | Multiply by 1000 and show as per mille value                |
;;; | ¤ (\u00A4) | Prefix or suffix    | No  | Currency sign, replaced by currency symbol. (2)             |
;;; | '          | Prefix or suffix    | No  | Used to quote special characters in a prefix or suffix (3)  |

;;;  (1) Need not be quoted in prefix or suffix.
;;;  (2) If doubled, replaced by international currency symbol. If present in a pattern,
;;;      the monetary decimal separator is used instead of the decimal separator.
;;;  (3) For example, "'#'#" formats 123 to "#123". To create a single quote itself, use two in a row: "# o''clock".

;;; $formatNumber
(defn $formatNumber
  "Casts the number to a string and formats it to a decimal representation as specified by the picture string.

   The behaviour of this function is consistent with the XPath/XQuery function fn:format-number as defined in
   the XPath F&O 3.1 specification. The picture string parameter defines how the number is formatted and has
   the same syntax as fn:format-number.

   The optional third argument options is used to override the default locale specific formatting characters
   such as the decimal separator. If supplied, this argument must be an object containing name/value pairs
   specified in the decimal format section of the XPath F&O 3.1 specification."
  ([number picture]  ($formatNumber number picture {}))
  ([number picture options]
   #?(:clj
      (let [pic (str/replace picture "e" "E")
            opts (keywordize-keys options)
            symbols (DecimalFormatSymbols.)]
        (.setExponentSeparator symbols "e")
        ;; ToDo: More options.
        (doseq [k (keys opts)]
          (when-not (#{:zero-digit :minus-sign :per-mille} k)
            (log/warn "$formatNumber: Unimplemented option:" (name k))))
        (doseq [[k v] (seq opts)]
          (case k
            :zero-digit (.setZeroDigit symbols (nth v 0))
            :minus-sign (.setMinusSign symbols (nth v 0))
            nil))
        (let [df (DecimalFormat. pic symbols)]
          (doseq [[k _v] (seq opts)]
            (when (=  k :per-mille) (.setMultiplier df 1000)))
          (.format df number)))
      :cljs
      (nb/formatNumber number picture (clj->js options)))))

;;; https://www.altova.com/xpath-xquery-reference/fn-format-integer

;;; $formatInteger
(defn $formatInteger
  "Casts the number to a string and formats it to an integer representation as specified by the picture string.
   The behaviour of this function is consistent with the two-argument version of the XPath/XQuery function
   fn:format-integer as defined in the XPath F&O 3.1 specification. The picture string parameter defines how
   the number is formatted and has the same syntax as fn:format-integer."
  [num pic]
  (s/assert num ::number)
#?(:clj   ; ToDo: Calls $formatNumber, which is :clj-only so far.
  (let [simple (case pic
                 "A"  (-> (take (abs num) (util/string-permute)) last)
                 "a"  (-> (take (abs num) (util/string-permute)) last str/lower-case)
                 "I"  (cl-format nil "~@r" (abs num))
                 "i"  (-> (cl-format nil "~@r" (abs num)) str/lower-case)
                 "W"  (-> (cl-format nil "~r"  num) str/upper-case)
                 "w"  (cl-format nil "~r"  num)
                 "Ww" (as-> (cl-format nil "~r" num) ?s ; Really? You can't do this yourself?
                        (str/split ?s #" ")
                        (map str/capitalize ?s)
                        (interpose " " ?s)
                        (apply str ?s))
                 nil)]
    (cond (and simple (#{"A" "a" "i" "I"} pic) (neg? num)), (str "-" simple)
          simple  simple
          :else ($formatNumber num pic)))
   :cljs
   (nb/formatInteger num pic)))

;;; $number
(defn* $number
  " * Numbers are unchanged.
    * Strings that contain a sequence of characters that represent a legal JSON number are converted to that number.
    * Boolean true casts to 1, Boolean false casts to 0.
    All other values cause an error to be thrown."
  [v_]
  (cond (number? v_) v_
        (string? v_) (let [n (util/read-str v_)]
                      (if (number? n)
                        n
                        (throw (ex-info "Cannot be cast to a number:" {:v_ v_}))))
        (boolean? v_) (if v_ 1 0)
        :else (throw (ex-info "Cannot be cast to a number:" {:v_ v_}))))

;;; $max
(defn* $max
  "Return the largest the numeric argument (an array or singleton)."
  [v_]
  (let [v (singlize v_)]
    (s/assert ::numbers v)
    (apply max v)))

;;; $min
(defn* $min
  "Return the smallest the numeric argument (an array or singleton)."
  [v_]
  (let [v (singlize v_)]
    (s/assert ::numbers v)
    (apply min v)))

;;; $parseInteger
(defn $parseInteger
  "Parses the contents of the string parameter to an integer (as a JSON number) using the format
   specified by the picture string. The picture string parameter has the same format as $formatInteger.
   Although the XPath specification does not have an equivalent function for parsing integers,
   this capability has been added to JSONata."
  [string pic] ; ToDo: Other pictures
  (case pic
    "w" (util/parse-num-string string)))

;;; $power
(defn $power
  "Return the largest the numeric argument (an array or singleton)."
  [x y]
  (s/assert ::number x)
  (s/assert ::number y)
  (if (and (integer? x) (pos-int? y))
    (long (Math/pow x y))
    (Math/pow x y)))

;;; $random
(defn $random
  "Returns a pseudo random number greater than or equal to zero and less than one (0 ≤ n < 1)"
  []
  (rand))

;;; $round
(defn $round
  "Returns the value of the number parameter rounded to the number of decimal places specified by the optional precision parameter.
   The precision parameter (which must be an integer) species the number of decimal places to be present in the rounded number.
   If precision is not specified then it defaults to the value 0 and the number is rounded to the nearest integer.
   If precision is negative, then its value specifies which column to round to on the left side of the decimal place.
   This function uses the ROUND_HALF to even (Java ROUND_EVEN ?) strategy to decide which way to round numbers that
   fall exactly between two candidates at the specified precision.
   This strategy is commonly used in financial calculations and is the default rounding mode in IEEE 754."
  ([num] ($round num 0))
  ([num precision]
   (s/assert ::number num)
   (s/assert ::number precision)
   ;; The Java notion of precision is the number of digits in scientific notation.
   #?(:clj
      (let [[left _right] (-> num double str (str/split #"\."))
            rnum (BigDecimal.
                  num
                  (MathContext.
                   (if (zero? precision)
                     (count left)
                     (+ (count left) precision))
                   RoundingMode/HALF_EVEN))]
        (if (pos? precision) (double rnum) (long rnum)))
      :cljs
      (nb/round num precision))))

(defn* $sum
  "Take one number of a vector of numbers; return the sum."
  [nums_]
  (letfn [(sum [nums]
            (let [v (singlize nums)]
              (s/assert ::numbers v)
              (apply + v)))]
    (if (p/promise? nums_) ; ToDo: Make the defn* macro do this!
      (-> nums_
          (p/then sum)
          (p/catch #(ex-info (str "In bi/$sum: " %) {:nums nums_ :err %})))
      (sum nums_))))

;;; $sqrt
(defn* $sqrt
  "Returns the square root of the argument."
  [v_]
  (s/assert ::number v_)
  (Math/sqrt v_))

;;;--------------- Boolean ------------
;;; $boolean
(defn* $boolean
   " Casts the argument to a Boolean using the following rules:

    | Argument type                               | Result    |
    |---------------------------------------------+-----------|
    | Boolean                                     | unchanged |
    | string: empty                               | false     |
    | string: non-empty                           | true      |
    | number: 0                                   | false     |
    | number: non-zero                            | true      |
    | null                                        | false     |
    | array: empty                                | false     |
    | array: contains a member that casts to true | true      |
    | array: all members cast to false            | false     |
    | object: empty                               | false     |
    | object: non-empty                           | true      |
    | function                                    | false     |"
  [arg_]
  (cond (map? arg_)     (if (empty? arg_) false true)
        (vector? arg_)  (if (some boolean? arg_) true false)
        (string? arg_)  (if (empty? arg_) false true)
        (number? arg_)  (if (zero? arg_) false true)
        (fn? arg_)      false
        (nil? arg_)     false
        (boolean? arg_) arg_
        (keyword? arg_) true)) ; query role.

;;; ToDo: I'll need more information than just the comment line here to understand what this
;;;       is suppose to do. It might require that I have better established how try/catch is handled.
;;; $exists
(defn* $exists
  "Returns Boolean true if the arg expression evaluates to a value, or false if the expression
   does not match anything (e.g. a path to a non-existent field reference)."
  [arg_]
  ($boolean arg_))

;;; $not
(defn* $not
  "Returns Boolean NOT on the argument. arg is first cast to a boolean."
  [arg_]
  (-> arg_ $boolean not))

;;;--------------- Arrays -------
;;; $append
(defn $append
  "Returns an array containing the values in array1 followed by the values in array2.
   If either parameter is not an array, then it is treated as a singleton array containing that value."
  [v1 v2]
  (into (singlize v1) (singlize v2)))

;;; $count
(defn $count
  "Returns the number of items in the array parameter. If the array parameter is not an array,
   but rather a value of another JSON type, then the parameter is treated as a singleton array
   containing that value, and this function returns 1.
   If array is not specified, then the context value is used as the value of array."
  ([] ($count @$))
  ([arr]
   (if (vector? arr) (count arr) 1)))

(defn* $distinct
  "Returns an array containing all the values from the array parameter, but with any duplicates removed.
   Values are tested for deep equality as if by using the equality operator."
  [arr_]
    (s/assert ::vector arr_)
  (-> arr_ distinct vec))

;;; $reverse
(defn* $reverse
  "Returns an array containing all the values from the array parameter, but in reverse order."
  [arr_]
  (s/assert ::vector arr_)
  (-> arr_ reverse vec))

;;; $shuffle
(defn* $shuffle
  "Returns an array containing all the values from the array parameter, but shuffled into random order."
  [arr_]
  (s/assert ::vector arr_)
  (c/loop [res []
           size (count arr_)
           rem arr_]
    (if (empty? rem) res
        (let [ix (rand-int size)]
          (recur (conj res (nth rem ix))
                 (dec size)
                 (let [[f b] (split-at ix rem)]
                   (into f (rest b))))))))

(declare sort-internal)

(defn* sort-internal
  "Do work of $sort. Separate so can def$ it."
  [fun arr_]
  (if (p/promise? arr_)
    (-> arr_
        (p/then #(sort-internal % fun))
        (p/catch #(ex-info (str "In bi/sort-internal: " %) {:arr_ arr_ :fun fun :err %})))
     (do (s/assert ::vector arr_)
         (s/assert ::fn fun)
         (vec (sort fun arr_)))))

;;; $sort
(defn $sort
  "Returns an array containing all the values in the array parameter, but sorted into order.
    If no function parameter is supplied, then the array parameter must contain only numbers or only strings,
    and they will be sorted in order of increasing number, or increasing unicode codepoint respectively.
    If a comparator function is supplied, then is must be a function that takes two parameters:
    function(left, right)
    This function gets invoked by the sorting algorithm to compare two values left and right.
    If the value of left should be placed after the value of right in the desired sort order,
    then the function must return Boolean true to indicate a swap. Otherwise it must return false."
  ([] (sort-internal compare))
  ([arr] (sort-internal compare arr))
  ([arr fun] (sort-internal fun arr)))

;;; $zip
(defn $zip
  "Returns a convolved (zipped) array containing grouped arrays of values from the array1 ... arrayN
   arguments from index 0, 1, 2, etc.
   This function accepts a variable number of arguments. The length of the returned array is equal to
   the length of the shortest array in the arguments."
  [& arrays]
  (s/assert ::vectors arrays)
  (let [size (->> arrays (map count) (apply min))]
    (c/loop [res []
             ix 0]
      (if (>= ix size) res
          (recur (conj res
                       (reduce
                        (fn [r a] (conj r (nth a ix)))
                        []
                        arrays))
                 (inc ix))))))

;;;---------------- JSON Object ------
;;; $assert
(defn $assert
  "If condition is true, the function returns undefined.
   If the condition is false, an exception is thrown with the message as the message of the exception."
  [exp msg]
  (when exp msg))

;;; $each
(defn* $each
  "Returns an array containing the values return by the function when applied to each key/value pair in the object.
   The function parameter will get invoked with two arguments:
   function(value, name)
   where the value parameter is the value of each name/value pair in the object and name is its name.
   The name parameter is optional."
  [obj_ func]
  (s/assert ::map obj_)
  (s/assert ::fn func)
  (if (== 2 (-> func meta :bi/params count))
    (reduce-kv (fn [r  k v] (conj r (func v k))) [] obj_)
    (reduce-kv (fn [r _k v] (conj r (func v  ))) [] obj_)))

;;; $error
(defn* $error
  "Deliberately throws an error with an optional message"
  [msg_]
  (s/assert ::string msg_)
  (throw (ex-info msg_ {})))

;;; $keys
(defn* $keys
  "Returns an array containing the keys in the object.
   If the argument is an array of objects, then the array returned contains a
   de-duplicated list of all the keys in all of the objects."
  [obj_] ; ToDo (use s/assert?)
  (cond (map? obj_) (-> obj_ keys vec)
        (vector? obj_) (->> obj_ (map keys) distinct)
        :else (throw (ex-info "The argument to $keys must be an object or array:" {:obj obj_}))))

;;; $lookup
(defn* $lookup
  "Returns the value associated with key in object.
   If the first argument is an array of objects, then all of the objects in the array are searched,
   and the values associated with all occurrences of key are returned."
  [obj_ k]
  (cond (map? obj_) (get obj_ k)
        (vector? obj_) (mapv #(get % k) obj_)
        :else (throw (ex-info "The argument to $keys must be an object or array." {:arg obj_}))))

;;; $merge
(defn $merge
  "Merges an array of objects into a single object containing all the key/value pairs from each
   of the objects in the input array. If any of the input objects contain the same key, then the
   returned object will contain the value of the last one in the array.
   It is an error if the input array contains an item that is not an object."
  [objs]
  (s/assert ::objects objs)
  (merge objs))

;;; $sift (listed under higher-order too)
(defn $sift
  "Returns an object that contains only the key/value pairs from the object parameter that satisfy
   the predicate function passed in as the second parameter.
   If object is not specified, then the context value is used as the value of object.
   It is an error if object is not an object.
   The function that is supplied as the second parameter must have the following signature:
   function(value [, key [, object]])
   Each value in the input object is passed in as the first parameter in the supplied function.
   The key (property name) of that value in the input object is passed in as the second parameter, if specified.
   The whole input object is passed in as the third parameter, if specified."
  ([func] ($sift @$ func))
  ([obj func]
   (s/assert ::object obj)
   (s/assert ::fn func)
   (let [nargs (-> func meta :bi/params count)]
     (cond (== nargs 1) (reduce-kv (fn [m k v] (if (func v      ) (assoc m k v) m)) {} obj)
           (== nargs 2) (reduce-kv (fn [m k v] (if (func v k    ) (assoc m k v) m)) {} obj)
           (== nargs 3) (reduce-kv (fn [m k v] (if (func v k obj) (assoc m k v) m)) {} obj)
           :else (throw (ex-info "The function provided to $sift must specify 1 to 3 arguments:" {:nargs nargs}))))))

;;; $spread
(defn* $spread
  "Splits an object containing key/value pairs into an array of objects,
   each of which has a single key/value pair from the input object.
   If the parameter is an array of objects, then the resultant array contains an object
   for every key/value pair in every object in the supplied array."
  [obj_]
  (when-not (or (map? obj_)
                (and (vector? obj_) (every? map? obj_)))
    (throw (ex-info "The argument to $spread must be an object or array of objects:" {:obj obj_})))
  (if (map? obj_)
                                (reduce-kv (fn [r k v] (conj r {k v})) [] obj_)
      (reduce (fn [r o] (into r (reduce-kv (fn [r k v] (conj r {k v})) []  o)))
              []
              obj_)))

;;; $type
(defn* $type
  "Evaluates the type of value and returns one of the following strings:
   'null', 'number', 'string', 'boolean', 'array', 'object', 'function'.
   Returns (non-string) undefined when value is undefined."
  [arg_]
  (cond (nil? arg_) "null"
        (number? arg_) "number"
        (string? arg_) "string"
        (boolean? arg_) "boolean"
        (vector? arg_) "array"
        (map? arg_) "object"
        (fn? arg_) "function"))

;;;------------- DateTime -----------
;;; What JSONata calls a timestamp is a string
;;;  $toMillis("2017-11-07T15:07:54.972Z") => 1510067274972
;;; I use 'tstamp' for these and 'java-tstamp' for a ZonedDateTime.

;;; https://platform.deloitte.com.au/articles/2013/formatting-dates-and-times-using-xslt-2.0-and-xpath
;;;
;;; | Format Token | Sequence                          |
;;; |--------------+-----------------------------------|
;;; | 1            | 1 2 3 ... 100 ...                 |  /I'm just doing this one, typically the default, for now!/
;;; | A            | A B C .... Z AA AB AC ...         |
;;; | a            | a b c ... z aa ab ac ...          |
;;; | i            | i ii iii iv v vi vii .. x .. m .. |
;;; | I            | I II III IV V VI VII .....        |
;;; | w            | one two three four ...            |
;;; | W            | ONE TWO THREE FOUR ...            |
;;; | Ww           | One Two Three Four ...            |
;;; | o            | first second third ...            |

;;; XPATH
;;; | Specifier | Meaning                     | Default presentation |
;;; |-----------+-----------------------------+----------------------|
;;; | Y         | year (absolute value)       |                    1 |
;;; | M         | month in year               |                    1 |
;;; | D         | day in month                |                    1 |
;;; | d         | day in year                 |                    1 |
;;; | F         | day of week                 |                    n |
;;; | W         | week in year                |                    1 |
;;; | w         | week in month               |                    1 |
;;; | H         | hour in day (24 hours)      |                    1 |
;;; | h         | hour in half-day (12 hours) |                    1 |
;;; | P         | am/pm marker                |                    n |
;;; | m         | minute in hour              |                   01 | (meaning pad with zero)
;;; | s         | second in minute            |                   01 |
;;; | f         | fractional seconds          |                    1 |
;;; | Z         | timezone                    |                01:01 |
;;; | z         | timezone (1)                |                01:01 |
;;; | C         | calendar (2)                |                    n |
;;; | E         | era: (3)                    |                    n |
;;;
;;; (1) same as Z, but modified where appropriate to include a prefix as a time offset using GMT,
;;;     for example GMT+1 or GMT-05:00.
;;;     For this component there is a fixed prefix of GMT, or a localized variation thereof
;;;     for the chosen language, and the remainder of the value is formatted as for specifier Z.
;;;
;;; (2) the name or abbreviation of a calendar name
;;;
;;; (3) the name of a baseline for the numbering of years, for example the reign of a monarch

;;; https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
(defn translate-part
  [fmt-map]
  (let [[_ var fmt] (re-matches #"\[(\w)([^\]]*)\]" (:item fmt-map))
        fmt-map (-> fmt-map (assoc :var var) (assoc :fmt fmt))]
    (assoc fmt-map
           :java
           (case var
             "P" "a"
             ("z" "Z") "Z" ; ToDo: Investigate
             "Y"       (->> (repeat (-> fmt-map :fmt count) "y") (apply str))
             "M"       (->> (repeat (-> fmt-map :fmt count) "M") (apply str))
             "D"       (->> (repeat (-> fmt-map :fmt count) "d") (apply str))
             ("H" "h") (->> (repeat (-> fmt-map :fmt count) "h") (apply str))
             "m"       (->> (repeat (-> fmt-map :fmt count) "m") (apply str))
             "s"       (->> (repeat (-> fmt-map :fmt count) "s") (apply str))
             ""))))

;;; https://www.w3.org/TR/xpath-functions-31/#rules-for-datetime-formatting, 9.8.4.1 The picture string
;;; (date-fmt-xpath2java "[M01]/[D01]/[Y0001] [h#1]:[m01][P]")
;;; =======> USE THIS SOLUTION STYLE IN $MATCH <====================================
(defn date-fmt-xpath2java
  "Return a string iterpreting an xpath picture to the equivalent string for
   for java.time.format.DateTimeFormatter.

   The picture consists of a sequence of variable markers and literal substrings.
   A substring enclosed in square brackets is interpreted as a variable marker;
   substrings not enclosed in square brackets are taken as literal substrings.
   The literal substrings are optional and if present are rendered unchanged, including any whitespace.
   If an opening or closing square bracket is required within a literal substring, it must be doubled.
   The variable markers are replaced in the result by strings representing aspects of the date
   and/or time to be formatted."
  [pic]
  (let [loop-cnt (atom 0)
        parts
        (->>
         (c/loop [str pic
                res []]
           (cond (> @loop-cnt 10) :stuck!
                 (empty? str) res
                :else (let [[success? front item back]
                            (re-matches #"([^\[]*)(\[[YMDdFWwHhPmsfZzCE0-9#]+\])(.*)" str)]
                        (if success?
                          (let [[_ bback]
                                (re-matches #"([^\[]*)(\[[YMDdFWwHhPmsfZzCE0-9#]+\])?(.*).*" back)]
                            (recur back
                                   (conj res
                                         {:item item
                                          :front front
                                          :back bback})))
                          (recur "" {:back str})))))
         (mapv translate-part))]
    (apply str (into (-> parts first :front vector)
                     (mapv #(str (:java %) (:back %)) parts)))))
#?(:clj
(defn format-time
  "Format a timestamp according to a picture specified by XPath (converted to
   a java DateTimeFormatter/ofPattern  ."
  [java-tstamp pic]
  (if pic
    (let [fmt-string (date-fmt-xpath2java pic)
          dtf (DateTimeFormatter/ofPattern fmt-string)]
      (str (.format dtf java-tstamp)))
    (str java-tstamp))))

;;; $fromMillis
(defn $fromMillis
  "Convert the number representing milliseconds since the Unix Epoch (1 January, 1970 UTC) to a formatted
   string representation of the timestamp as specified by the picture string.
   If the optional picture parameter is omitted, then the timestamp is formatted in the ISO 8601 format.
   If the optional picture string is supplied, then the timestamp is formatted occording to the representation
   specified in that string. The behaviour of this function is consistent with the two-argument version of
   the XPath/XQuery function fn:format-dateTime as defined in the XPath F&O 3.1 specification.
   The picture string parameter defines how the timestamp is formatted and has the same syntax as fn:format-dateTime.

   If the optional timezone string is supplied, then the formatted timestamp will be in that timezone.
   The timezone string should be in the format '±HHMM', where ± is either the plus or minus sign and
   HHMM is the offset in hours and minutes from UTC. Positive offset for timezones east of UTC,
   negative offset for timezones west of UTC."
  ([]           (partial $fromMillis))
  ([millis]     ($fromMillis millis nil nil))
  ([millis pic] ($fromMillis millis pic nil))
  ([millis pic tzone]
   #?(:clj
      (let [zone-offset (if tzone (ZoneId/of tzone) ZoneOffset/UTC)
            java-tstamp (ZonedDateTime/ofInstant (Instant/ofEpochMilli (long millis)) zone-offset)]
        (format-time java-tstamp pic))
      :cljs
      (cond (and pic tzone) (nb/fromMillis millis pic tzone)
            pic             (nb/fromMillis millis pic)
            :else           (nb/fromMillis millis)))))

;;; ToDo: What does this mean?:
;;;   "All invocations of $millis() within an evaluation of an expression will all return the same timestamp value."
;;;    N.B. It says the same thing on $now().
;;; $millis
(defn $millis
  "Returns the number of milliseconds since the Unix Epoch (1 January, 1970 UTC) as a number.
   All invocations of $millis() within an evaluation of an expression will all return the same value."
  []
  (:millis (get-set-now)))

;;; $now
#?(:clj
(defn $now
  "Generates a UTC timestamp in ISO 8601 compatible format and returns it as a string.
   All invocations of $now() within an evaluation of an expression will all return the same timestamp value.
   If the optional picture and timezone parameters are supplied, then the current timestamp is formatted
   as described by the $fromMillis() function."
  ([] (-> (get-set-now) :instant str)) ; Prints as e.g. "2022-08-09T14:01:25.849575"
  ([pic] ($now pic nil))
  ([pic tzone]
   (let [zone-offset (if tzone (ZoneId/of tzone) ZoneOffset/UTC)]
     (format-time (-> (get-set-now) :instant (ZonedDateTime/ofInstant zone-offset)))))))

#?(:cljs
(defn $now
  ([]              (-> (get-set-now) :millis ($fromMillis)))
  ([pic]           (-> (get-set-now) :millis ($fromMillis pic)))
  ([pic tzone]     (-> (get-set-now) :millis ($fromMillis pic tzone)))))

;;; $toMillis
(defn $toMillis
  "Signature: $toMillis(timestamp [, picture])
   Convert a timestamp string to the number of milliseconds since the Unix Epoch (1 January, 1970 UTC) as a number.
   If the optional picture string is not specified, then the format of the timestamp is assumed to be ISO 8601.
   An error is thrown if the string is not in the correct format.
   If the picture string is specified, then the format is assumed to be described by this picture string using
   the same syntax as the XPath/XQuery function fn:format-dateTime, defined in the XPath F&O 3.1 specification."
  ([str] ($toMillis str nil))
  ([str _pic] ; ToDo: implement pic
   (s/assert ::string str)
   #?(:clj
      (LocalDateTime/parse str) ; ZonedDateTime will not work.
      :cljs
      (if _pic
        (nb/toMillis str _pic)
        (nb/toMillis str)))))

;;;-------------- Higher (the higher not yet defined)
;;; $filter
(defn $filter
  "Return a function for the argument form."
  [coll_ func]
  (let [nvars (-> func meta :bi/params count)]
    (cond
      (== nvars 3)
                (c/loop [c coll_, i 0, r []]
                  (if (empty? c)
                    r
                    (let [val (when (func (first c) i coll_) (first c))]
                      (recur (rest c), (inc i), (if val (conj r val) r)))))
                (== nvars 2)
                (c/loop [c coll_, i 0, r []]
                  (if (empty? c)
                    r
                    (let [val (when (func (first c) i) (first c))]
                      (recur (rest c), (inc i) (conj (if val (conj r val) r))))))
                (== nvars 1)
                (c/loop [c coll_, r []]
                  (if (empty? c)
                    r
                    (let [val (when (func (first c)) (first c))]
                      (recur (rest c) (if val (conj r val) r)))))
                :else (throw (ex-info "$filter expects a function of 1 to 3 parameters:"
                                      {:params (-> func meta :bi/params)})))))

(def ^:dynamic *in-reduce?* "true if executing a function inside reduce" false)

;;; $map
(defn $map
  "Signature: $map(array, function)

   Returns an array containing the results of applying the function parameter to each value in the array parameter.
   The function that is supplied as the second parameter must have the following signature:
   function(value [, index [, array]])
   Each value in the input array is passed in as the first parameter in the supplied function.
   The index (position) of that value in the input array is passed in as the second parameter, if specified.
   The whole input array is passed in as the third parameter, if specified.

   Example: $map([1..5], $string) => ['1', '2', '3', '4', '5']"
  [coll func]
  (let [nvars  (-> func meta :bi/params count)]
    (cond
      (== nvars 3)
      (c/loop [c coll, i 0, r []]
        (if (empty? c)
            r
            (recur (rest c) (inc i) (conj r (func (first c) i coll)))))
      (== nvars 2)
      (c/loop [c coll, i 0, r []]
        (if (empty? c)
          r
          (recur (rest c) (inc i) (conj r (func (first c) i)))))
      (== nvars 1)
      (c/loop [c coll, r []]
        (if (empty? c)
          r
          (recur (rest c) (conj r (func (first c))))))
      :else (throw (ex-info "$map expects a function of 1 to 3 parameters:"
                            {:params (-> func meta :bi/params)})))))

;;; $reduce
(declare reduce-typical reduce-express)
(defn $reduce
  "Signature: $reduce(array, function [, init])

  Returns an aggregated value derived from applying the function parameter successively to
  each value in array in combination with the result of the previous application of the function.

  The function must accept at least two arguments, and behaves like an infix operator
  between each value within the array. The signature of this supplied function must be of the form:
  myfunc($accumulator, $value [, $index [, $array]])


  Example 1:   ( $product := function($i, $j){$i * $j};
               $reduce([1..5], $product)
               )

  Example 2: ( $add := function($i, $j){$i + $j};
               $reduce([1..5], $add, 100))
             )

  If the optional init parameter is supplied, then that value is used as the initial value in the aggregation (fold) process.
  If not supplied, the initial value is the first value in the array parameter"
  ([coll func] ($reduce coll func nil))
  ([coll func init]
   (binding [*in-reduce?* true]
     (let [met (meta func)]
       (cond (:bi/express-template? met)                          (throw (ex-info "Reducing called on express template fn." {})),
             (:bi/express? met)                                   (reduce-express coll func init),
             (not (<= 2 (-> met :bi/params count) 4))             (throw (ex-info "Reduce function must take 2 to 4 args." {}))
             (not init)                                           (reduce-typical coll func)
             :else                                                (reduce-typical (into (vector init) coll) func))))))

;;; ToDo: What is the point of the 4th parameter in function called by $reduce?
;;;       My doc-string below doesn't say.
;;;       In the above I pass it the collection untouched every time.
;;;       The JSONata documentation (http://docs.jsonata.org/higher-order-functions) doesn't say either!
(defn reduce-typical
  "This is for $reduce with JSONata semantics."
  [coll func]
  (let [num-params (-> func meta :bi/params count)]
    (c/loop [c (rest coll),
           r (first coll)
           i 0]
      (if (empty? c)
        r
        (recur (rest c),
               (case num-params
                 4 (func r (first c) i coll),
                 3 (func r (first c) i),
                 2 (func r (first c))),
               (inc i))))))

;;; $single
(defn* $single
  "See http://docs.jsonata.org/higher-order-functions
   Signature: $single(array, function)

   Returns the one and only one value in the array parameter that satisfy the function predicate
   (i.e. function returns Boolean true when passed the value). Throws an exception if the number
   of matching values is not exactly one.

   The function that is supplied as the second parameter must have the following signature:
   function(value [, index [, array]])

   Each value in the input array is passed in as the first parameter in the supplied function.
   The index (position) of that value in the input array is passed in as the second parameter,
   if specified. The whole input array is passed in as the third parameter, if specified."
  [coll_ func]
  (let [nvars  (-> func meta :bi/params count)]
    (cond
      (== nvars 3)
      (c/loop [c coll_, i 0, r false]
        (cond r r
              (empty? c) false
              :else (recur (rest c) (inc i) (func (first c) i coll_))))
      (== nvars 2)
      (c/loop [c coll_, i 0, r false]
        (cond r r
              (empty? c) false
              :else (recur (rest c) (inc i) (func (first c) i))))
      (== nvars 1)
      (c/loop [c coll_, r false]
        (cond r r
              (empty? c) false
              :else (recur (rest c) (func (first c))))),
      :else (throw (ex-info "$single expects a function of 1 to 3 parameters:"
                            {:params (-> func meta :bi/params)})))))

;;;==========================================================================
;;;=================== Non-JSONata functions ================================
#?(:clj  (defn read-local
           [fname opts]
           (let [type (-> (re-matches #"^.*\.([a-z,A-Z,0-9]{1,5})$" fname) second)]
             (-> (case (or (get opts "type") type "xml")
                   "json" (-> fname slurp json/read-str)
                   "xml"  (-> fname util/read-xml :xml/content first :xml/content util/simplify-xml)
                   "edn"  (-> fname slurp util/read-str util/clj-key->rm-id))
                 set-context!)))
   :cljs (defn read-local
           [_fname _opts]
           (throw (ex-info "$get() from the browser requires a graph query argument." {}))))

(declare processRM)
(defn compile-rm
  "Return the object produced by rad-mapper compiling the argument string."
  [src]
  (log/info "compile-rm: src =" src)
  (try (reset! diag (processRM :ptag/exp src {:execute? true}))
       (catch #?(:clj Throwable :cljs :default) e
         (reset! diag e)
         (ex-info "compile-rm: Did not compile!" {:src src :err e}))))

(defn $get
  "Read a file of JSON or XML, creating a map."
  [ident|file-string other]
  (cond (string? ident|file-string)                           (let [file-string ident|file-string
                                                                    opts other]
                                                                (read-local file-string opts)),
        (and (vector? ident|file-string)
             (= ident|file-string ["db_name" "schemaDB"]))    {"db_connection" "_rm_schema-db"},

        :else   ;; It is a DB get.
        (let [[ident-type ident-val] ident|file-string
              out-props other
              lib-fn?  (= "library_fn" ident-type)
              wants-exe? (some #(= % "fn_exe") out-props)
              wants-src? (some #(= % "fn_src") out-props)
              new-props (if (and lib-fn? wants-exe? (not wants-src?)) (conj out-props "fn_src") out-props)] ; Need src to compile!
          #?(:clj
             ;; Conversion to keywords is always done close to pathom.
             ;; ident-val is not coerced here because it need not be a key (e.g. 'addOne').
             ;; ident-val may be coerced in the resolver, as needed.
             ;; See resolvers.clj for things like ($put ["list_id", "ccts_message-schema"], ["list_content"])
             (let [ident-type (keyword ident-type)
                   new-props  (mapv keyword new-props)]
               (log/info "$get: ident-type = " ident-type "ident-val = " ident-val "new-props =" new-props)
               (as-> (pathom-resolve {ident-type ident-val} new-props) ?x
                 (update-keys ?x name)
                 (if (and lib-fn? wants-exe?) (assoc ?x "fn_exe" (compile-rm (get ?x "fn_src"))) ?x) ; ToDo: This is wasted if call is from CLJS.
                 (if (not wants-src?) (dissoc ?x "fn_src") ?x)))
             :cljs (let [prom (p/deferred)
                         req-data {:params {:ident-type ident-type
                                            :ident-val ident-val
                                            :request-objs (cl-format nil "~{~A~^|~}" new-props)}
                                   :handler (fn [resp] (p/resolve! prom resp))
                                   :error-handler (fn [{:keys [status status-text]}]
                                                    (p/reject! prom (ex-info "CLJS-AJAX error on /api/graph-get"
                                                                             {:status status :status-text status-text})))
                                   :timeout 5000}]
                     (GET (str svr-prefix "/api/graph-get") req-data) ; ToDo: use Martian.
                     (-> prom
                         (p/then #(if (and lib-fn? wants-exe?) (assoc % "fn_exe" (compile-rm (get % "fn_src"))) %))
                         (p/then #(if (not wants-src?) (dissoc % "fn_src") %))
                         (p/catch #(ex-info (str "In $get: " %) {:ident-type ident-type :ident-val ident-val :props new-props}))))))))

 #?(:clj
(defn $put
  "Read a file of JSON or XML, creating a map."
  [[ident-type ident-val] obj]
  (log/info "$put: ident-type = " ident-type "ident-val =" ident-val "obj = " obj)
  (if (= ident-type "library_fn")
    (try (let [ident-type (keyword ident-type)
               obj (update-keys obj keyword)]
           (d/transact (codelib/connect-atm) [(into {:fn_name ident-val} obj)])
           "success")
         (catch Throwable e (ex-info "$put to library failed." {:obj obj :message (.getMessage e)})))
    (throw (ex-info "Only $put to library_fn currently supported." {:ident-type ident-type})))))

#?(:cljs
(defn $put
  "Call server for $put (an api/graph-put call)."
  [[ident-type ident-val] obj]
  (log/info "$put: ident-type = " ident-type "ident-val =" ident-val "obj = " obj)
  (let [prom (p/deferred)
        req-data {:params {:put-ident-type ident-type :put-ident-val ident-val :put-obj obj}
                  :handler (fn [resp] (p/resolve! prom resp))
                  :error-handler (fn [{:keys [status status-text]}]
                                   (p/reject! prom (ex-info "CLJS-AJAX error on /api/graph-put"
                                                            {:status status :status-text status-text})))
                  :timeout 5000}]
    (POST (str svr-prefix "/api/graph-put") req-data) ; ToDo: use Martian.
    prom)))

(defn rewrite-sheet-for-mapper
  "Reading a sheet returns a vector of maps in which the first map is assumed to
   be a header. The names of the columns (map keys) uses the conventional :A,:B,:C,...
   This returns a vector with the header element removed and the other content
   mapped to the corresponding keywordized values of the header element."
  [sheet]
  (let [[header & content] sheet
        key-header (reduce-kv (fn [res k v] (assoc res k (-> (str/replace v #"[\s+,\.+]" "_") keyword)))
                              {}
                              header)]
    (mapv #(reduce-kv (fn [res k v]
                        (assoc res (get key-header k) v))
                      {}
                      %)
          content)))

(defn transpose-sheet
  "Argument is a vector of maps such as returned from ss/select-columns.
   Result transposes those, producing a single map:
   [{:A 'Our Company Name',    :B 'Acme Supply'}
    {:A 'Our Company Address', :B '123 Mockinbird Lane, Ocean City, MD, 20878'}]
    ==>
    {'Our Company Name':    'Acme Supply'
     'Our Company Address': '123 Mockingbird Lane, Ocean City, MD, 20878'}
   Note that this assumes only two comumns :A and :B where :A is a key and :B is a value.
   Thus the values in :A should be unique!"
  [in-map]
  (->> (reduce (fn [res m] (assoc res (:A m) (:B m))) {} in-map)
       (reduce-kv (fn [res k v] (assoc res (-> (str/replace k #"[\s+,\.+]" "_") keyword) v))
                  {})))

;;; ToDo: make this part of $get.
;;; $getSpreadsheet
#?(:clj
(defn $getSpreadsheet
  "Read the .xlsx and make a clojure map for each row. No fancy names, just :A,:B,:C,...!"
  ([filename sheet-name] ($getSpreadsheet filename sheet-name false))
  ([filename sheet-name invert?]
   (reset! $$ (when-let [sheet (->> (ss/load-workbook filename) (ss/select-sheet sheet-name))]
               (let [row1 (mapv ss/read-cell (-> sheet ss/row-seq first ss/cell-seq ss/into-seq))
                     len  (c/loop [n (dec (-> sheet ss/row-seq first .getLastCellNum))]
                            (cond (= n 0) 0,
                                  (not (nth row1 n)) (recur (dec n)),
                                  :else (inc n)))
                     keys (map keyword (take len (util/string-permute)))
                     raw (ss/select-columns (zipmap keys keys) sheet)]
                 (if invert?
                   (transpose-sheet raw)
                   ;; ToDo This is all sort of silly. Can we access cells a better way?
                   (rewrite-sheet-for-mapper raw))))))))

(defn $pull
  "Pull all data about an entity using its entity ID."
  [id conn-atm]
  (dp/pull @conn-atm '[*] id))

(defn $db
  "Return a database for the data, a vector or maps."
  [data]
  (qu/db-for! data))

;;;============================= query, express ======================
(defn $schemaFor
  "Study the argument data and heuristically suggest the types and multiplicity of data.
   Note that this function does not make a guess at what the keys (db/key) are."
  [data]
  (qu/learn-schema data))

;;;---------- query ---------------------------------------
(defn final-query-form
  "Return an executable datalog query [:find...] form that substitutes values as
   though syntax quote were being used.
   This replaces '$vars' parameters with values using param-val-map.
   It provides everything needed to call d/q except the databases and predicate args.
   Note: predicate parameters (the non-database things of :in) were defined earlier in rewriting."
  [body in param-val-map]
  (letfn [(tp-aux [x]
            (cond (vector? x)                  (mapv tp-aux x),
                  (seq? x)                     (map  tp-aux x),
                  (contains? param-val-map x)  (param-val-map x),
                  :else x))]
    (let [vars (->> (flatten body) ; (reduce (fn [r x] (into r x)) [] body)
                    (filter #(when (symbol? %)
                               (str/starts-with? (name %) "?")))
                    distinct
                    vec)]
    `[:find  ~@vars
      :syms  ~@vars
      :in    ~@in
      :where ~@(tp-aux body)])))

(defn entity-qvars
  "Return the set of qvars in entity position of the argument body"
  [body]
  (let [dbs-specified? (some #(== 4 (count %)) body)]
    (if dbs-specified?
      (->> body
           (filter #(and (== 4 (count %)) (-> % second qvar?)))
           (map second)
           set)
      (->> body
           (filter #(and (== 3 (count %)) (-> % first qvar?)))
           (map first)
           set))))

;;; ToDo: This needs some thought. Should probably only happen when the qvar binds a data key.
;;;       Could use schema analysis such as used on the express body!
;;; 2023-01-07: I'm not running it! This causes errors by not keeping roles (keywords) as roles in user data.
;;;             I don't understand the rationale for this. The role position is either a keyword or qvar.
;;;             I wrote it for $reduce on express. So that's what needs to be investigated.
;;; (bi/query-fn-aux [(rrr/connect-atm)] '[[?e :schema/name ?name]] '[$] nil nil nil)
(defn query-fn-aux
  "The function that returns a vector of binding sets.
   Note that it attaches meta for the DB and body."
  [db-atms body-in in pred-args param-subs options]
  (let [dbs (mapv deref db-atms)
        body #?(:clj  (if (string? body-in) (m/decode "application/transit+json" body-in) body-in)
                :cljs body-in)
        e-qvar? (entity-qvars body)
        qform (final-query-form body in param-subs)]
    (as-> (apply d/q qform (into dbs pred-args)) ?bsets ; This is possible because body is data to d/q.
      ;; Remove binding sets that involve a schema entity.
      (remove (fn [bset] (some (fn [bval]
                                 (and (keyword? bval)
                                      (= "db" (namespace bval))))
                               (vals bset))) ?bsets)
      (cond->> ?bsets
        (-> options :keepDBid not) (map (fn [bset]
                                          (reduce-kv (fn [m k v]
                                                       (if (e-qvar? k) m (assoc m k v)))
                                                     {}
                                                     bset)))
        true                       (vec))
      (with-meta ?bsets {:bi/b-set? true}))))

#?(:clj
(defn immediate-query-fn
  "Return a function that can be used immediately to make the query defined in body."
  [body in pred-args options]
  (fn [& data|dbs]
    ;(log/info "data|dbs =" data|dbs)
    (let [db-atms
          ;; CLJ can also be called with $db := $get([['db_name', 'schemaDB'], ['db_connection']]);
          (if (= {"db_connection" "_rm_schema-db"} (first data|dbs))
            [(schema/connect-atm)]
            (map #(if (util/db-atm? %) % (-> % keywordize-keys qu/db-for!)) data|dbs))]
      (query-fn-aux db-atms body in pred-args {} options)))))

#?(:cljs
(defn immediate-query-fn
  "Return a function that can be used immediately to make the query defined in body.
   If it is called with data|dbs = :_rm/schema-db, then make a call to REST function
   that will cause the server to call query-fn-aux itself. Otherwise the function
   calls query here."
  [body in pred-args options]
  (fn [& data|dbs]
    ;;(log/info "immediate-query-fn: data|dbs =" data|dbs)
    ;;(reset! diag {:data|dbs data|dbs :body body})
    (if (= {"db_connection" "_rm_schema-db"} (first data|dbs)) ; Then we can't do it here.
      (let [prom (p/deferred)]
        (log/info "immediate-query-fn: body =" body)
        (POST (str svr-prefix "/api/datalog-query") ; This will do a query-fn-aux in the server.
              {:params {:qforms (str body)}
               :timeout 3000
               :handler (fn [resp] (p/resolve! prom resp))
               :error-handler (fn [{:keys [status status-text]}]
                                (log/info (str "CLJS-AJAX $datalog-query error: status = " status " status-text= " status-text))
                                (p/rejected (ex-info "CLJS-AJAX error on /api/datalog-query" {:status status :status-text status-text})))})
        prom)
      (let [db-atms (map #(if (util/db-atm? %) % (-> % keywordize-keys qu/db-for!)) data|dbs)]
        (query-fn-aux db-atms body in pred-args {} options)))))
)

(defn higher-order-query-fn
  "Return a function that can be called with parameters to return a function to m
   the parameterizes query defined by body and params. (It's just a closure...)"
  [body in pred-args options params]
  (-> (fn [& args]
        (let [param-subs (zipmap params args)] ; the closure.
          (-> (fn [& data|dbs]
                (let [db-atms (map #(if (util/db-atm? %) % (-> % keywordize-keys qu/db-for!)) data|dbs)]
                  ;;(swap! diag #(-> % (assoc :body body) (assoc :pred-args pred-args) (assoc :param-subs param-subs)))
                  (query-fn-aux db-atms body in pred-args param-subs options)))
              (with-meta {:bi/fn-type :query-fn
                          :bi/expected-arg-cnt (max 1 (->> body ; ToDo: Is this right?
                                                           (filter #(== (count %) 4))
                                                           (map first)
                                                           distinct
                                                           count))}))))
      (with-meta {:bi/fn-type :query-fn-template :bi/expected-arg-cnt (count params)})))

(defn query
  "There are two usage scenarios for query:
      (1) Calls to query where no parameters are specified return a function
          that takes data and returns binding sets.
      (2) Calls to query that provide parameters return a function that takes
          values for those parameters and return a function of type (1).

  'params' is an ordered vector of parameters (jvars) that will be matched to 'args'
   used to parameterized the query form, thus producing a 'customized' query function.

   Example usage (of the second sort):

  ( $data := $newContext() ~> $addSource($get('data/testing/owl-example.edn'));
    $q := query($type){[?class :rdf_type            $type]
                       [?class :resource_iri        ?class-iri]
                       [?class :resource_namespace  ?class-ns]
                       [?class :resource_name       ?class-name]};
    $q($data,'owl/Class') )"
  [{:keys [body in pred-args params options]}]
  (if (empty? params)
    (immediate-query-fn body in pred-args options)
    (higher-order-query-fn body in pred-args options params)))

;;;------------------ Express --------------------------------------------
;;; ToDo: Write a few paragraphs about how express works, particularly
;;;      (1) the difference  between ordinary operation and *id-reduce?* reduce-db,
;;;      (2) the use of :db.unique/identity for 'assoc-in-like' mapping over bodies instantiated by bsets.
;;;      (3) the differences in how the two key positions are handled (express keys vs. qvar in key position).
(declare evaluate-express-body express-sub)

;;; base-body = {"id" '?v1, "aAttr" {"val" '?v2}}
(defn reduce-body-and-schema
  "This is separate so that I can call it when testing.
   It returns a map containing :reduce-body and :schema."
  [base-body]
  (qu/schematic-express-body base-body)
  #_(let [res (qu/schematic-express-body base-body)
          sub-body
          '#:redex{:obj [#:redex{:?name--?name [:redex/express-key ?name],
                                 :user-key ?name,
                                 :val
                                 #:redex{:obj [#:redex{:aData--?name|aData [:redex/express-key ?name "aData"], :user-key "aData", :val ?aData}
                                               #:redex{:bData--?name|bData [:redex/express-key ?name "bData"], :user-key "bData", :val ?bData}
                                               #:redex{:id--?name|id [:redex/express-key ?name "id"], :user-key "id", :val ?id}]}}]}]
      ;;(swap! diag #(assoc % :reduce-body sub-body))
    (assoc res :reduce-body sub-body)))

;;; ToDo: It may be possible to push the reduce deeper, so it is in evaluate-express-body, but I'm not sure that's an improvement.
(defn express
  "Return an function that either
    (1) has no template variable and can be used directly with a binding set, or
    (2) has template variables and returns a parameterized version of (1) that
        can be called with parameter values to get the a function like (1) to
        be used with binding sets.
     The function returned has meta {express? true} so that, for example, $reduce
     knows that there should be a database involved."
  [& {:keys [params options base-body key-order]}]
  (let [{:keys [reduce-body schema]} (reduce-body-and-schema base-body)]
    (if (empty? params)
      ;; The immediate function:
      (-> (fn [bset] (evaluate-express-body bset base-body reduce-body))
          ;; :bi/schema is not needed for simple evaluation, only reduce evaluation. :bi/reduce-body for debugging.
          (with-meta {:bi/params '[b-set], :bi/express? true :bi/options options :bi/schema schema
                      :bi/base-body base-body :bi/reduce-body reduce-body :bi/key-order key-order}))
      ;; body has template params that must be replaced; new-body.
      (-> (fn [& psubs]
            (let [new-base-body   (express-sub base-body   (zipmap params psubs))
                  new-reduce-body (express-sub reduce-body (zipmap params psubs))]
              (-> (fn [bset] (evaluate-express-body bset new-base-body new-reduce-body))
                  (with-meta {:bi/params '[b-set], :bi/express? true :bi/options options :bi/schema schema
                              :bi/base-body base-body :bi/reduce-body reduce-body :bi/key-order key-order}))))
          (with-meta {:bi/express-template? true})))))

(defn box-vals
  "Walk through the form replacing non-map :redex/val and :redex/ek-val with boxed data."
  [obj]
  (cond (map? obj)       (reduce-kv (fn [m k v]
                                      (if (and (#{:redex/val :redex/ek-val} k)
                                               (some #(% v) [string? number? keyword? boolean?]))
                                        (assoc m k (box v))
                                        (assoc m k (box-vals v))))
                                    {} obj)
        (vector? obj)    (mapv box-vals obj)
        :else            obj))

(s/def ::b-set (s/and map? #(every? qvar? (keys %))))

(defn evaluate-express-body
  "Walk through the body of the express replacing qvars with their bset values,
   computing concatenated keys (and NYI) evaluating expressions."
  [bset base-body reduce-body]
  (if (s/valid? ::b-set bset)
    (letfn [(sub-bset [key-parts bset] ; Generate a value for a :redex/express-key expression.
              (if *in-reduce?*
                (->> (map #(if (qvar? %) (get bset %) %) key-parts) ; There can be constants in catkey.
                     (map str)
                     (interpose "|")
                     (apply str))
                (get bset (first key-parts))))
            (eeb-aux [obj]
              (cond (map? obj)         (reduce-kv (fn [m k v]
                                                    (if (= :redex/user-key k)
                                                      (assoc m k (-> (if (qvar? v) (get bset v) v) box))
                                                      (assoc m (eeb-aux k) (eeb-aux v)))) {} obj)
                    (qu/exp-key? obj)  (sub-bset (rest obj) bset) ; rest: strip off :redex/express-key.
                    (vector? obj)      (mapv eeb-aux obj)
                    (qvar? obj)        (get bset obj)
                    :else              obj))]
      (if *in-reduce?*        ; Or am I missing something? Something about cat keys???
        (-> reduce-body eeb-aux box-vals)
        (eeb-aux base-body)))
    (throw (ex-info "Argument is not a binding set:" {:arg bset}))))

(defn express-sub
  "Walk through form replacing template parameters in sub-map with their values."
  [form sub-map]
  (letfn [(es-aux [x]
            (cond (map? x)    (reduce-kv (fn [m k v] (assoc m (es-aux k) (es-aux v))) {} x)
                  (vector? x) (mapv es-aux x)
                  (symbol? x) (or (get sub-map x) x)
                  :else x))]
    (es-aux form)))

(defn redex-val+
  "Walk the object replacing :redex/val and :redex/vals with :redex/val+.
   This simplifies the work of redex-keys-values."
  [obj]
  (cond (map? obj)    (reduce-kv (fn [m k v] (if (#{:redex/val :redex/vals :redex/ek-val} k)
                                               (cond-> (assoc m :redex/val+ (redex-val+ v))
                                                 (= :redex/ek-val k) (assoc :redex/ek-val v)) ; for sorting later
                                               (assoc m k (redex-val+ v))))
                                 {} obj)
        (vector? obj) (mapv redex-val+ obj)
        :else         obj))

(defn redex-keys?
  "Return true if the object has redex keys."
  [obj]
  (and (map? obj) (some #(= "redex" (namespace %)) (keys obj))))

(defn qvar-key?
  "Return true if the object has a qvar user key. The key will be quoted." ; ToDo: quoted was a good idea because...?
  [obj]
  (-> obj :redex/user-key second util/qvar?))

(defn merge-cond?
  "Return true if the results of rkv from multiple EIDs can be merged"
  [data+ reduce-body]
  (or (and (map? reduce-body) (-> reduce-body :redex/user-key util/qvar?))
      (when (second data+)
        (or
         ;; They are qvar in key pos.
         (and (-> data+ first map?)
              (-> data+ first (contains? :redex/obj))
              (->> reduce-body :redex/obj (every? #(-> % :redex/user-key qvar?))))
         ;; They are slot values
         (not-any? #(or (contains? % :redex/more) (contains? % :redex/obj)) data+)))))

;;; "redex" is REDuce on EXpress body.
(defn redex-keys-values
  "Rewrite the :redex/ROOT object(s) retrieved from the database so that it/they match the shape that
   was (1) encoded in the schema, and (2) built-up through objects using the reduce body.
   Since those two tasks did most of the work shaping the data, what remains here is mostly just to make ordinary
   maps, vectors, and values from the retrieved content. Buth there are a few kinds of maps to deal with:
     - The ones with express keys (have :redex/ek-val) get their express key processed first and then their :redex/more.
     - The ones with just :redex/more are similar but the value of the :redex/user-key is the reduced :redex/more.
     - The ones with :redex/obj are similar the :redex/more ones, but put content into a fresh object at the key.
     - The ones without :redex/more or :redex/obj are maps of just one key that isn't an express-key nor qvar-in-key-position.
   Further, there can be a redex-toplevel-merge when the express form leads off with a qvar." ; ToDo: Is this the only time this kind of work is needed?
  [data reduce-body]
  ;(swap! diag #(assoc % :redex-data data))
  (letfn [(add-ek-val? [making src] (if-let [ekv (:redex/ek-val src)] (assoc making :redex/ek-val ekv) making)) ; maintain :ek-val for sorting.
          (rkv [obj]
            (cond  (map? obj)     (cond (contains? obj :redex/more) ; :redex/more : continue 'this object'.
                                        (reduce (fn [m attr] (merge m (rkv attr)))
                                                (cond-> {} ; more without user-key is case for simple map, e.g. {'id': ?v1, 'aAttr': {'val': ?v2}}
                                                  (:redex/user-key obj) (assoc (:redex/user-key obj) (:redex/val+ obj))
                                                  true (add-ek-val? obj))
                                                (:redex/more obj)),

                                          (contains? obj :redex/obj) ; Then obj is a key with an object (:obj) value
                                          {(:redex/user-key obj) ; The :obj things are slots (ALWAYS, thus merge okay); their values can be anything.
                                           (reduce (fn [m attr] (merge m (rkv attr))) {} (:redex/obj obj))}

                                        ;; leaf attrs of a map have no attrs themselves; will be merged.
                                        (contains? obj :redex/user-key)
                                        (-> {(:redex/user-key obj) (-> obj :redex/val+ rkv)} (add-ek-val? obj))

                                        :else (-> obj :redex/val+ rkv))

                   (vector? obj)  (if (every? redex-keys? obj) ; a vector db redex objects. ToDo: Can I assume these are homogeneous?
                                    (if (-> obj first qvar-key?)
                                      (reduce (fn [res v] (merge res (rkv v))) {} obj),
                                      (mapv rkv obj))
                                    (mapv rkv obj))
                   :else  obj))]
    (let [data+  (redex-val+ data)]
      (cond (== 1 (count data+))                           (-> data+ first rkv),
            (merge-cond? data+ reduce-body)                (->> data+ (map rkv) (apply merge)),
            :else                                          (mapv rkv data+)))))

;;; ToDo: Do sort-by-body when *in-reduce* = false. Where to put it might not be straightforward!
(defn sort-by-body
  "Walk through redex-keys-values-processed output sorting
     (a) its vectors of express-keyed  maps by by their express-keys (:redex/ek-val) and
     (b) its maps by their keys." ; If nothing else, this make testing easier!
  [data key-order]
  (let [known-key? (set key-order)]
    (letfn [(compar [k1 k2]
              (if (and (known-key? k1) (known-key? k2))
                (< (.indexOf key-order k1) (.indexOf key-order k2))
                (compare (str k1) (str k2)))) ; ToDo: See notes 2023-01-09 about symbols as keys.
            (ek-vec? [obj]
              (and (vector? obj)
                   (every? #(contains? % :redex/ek-val) obj)))
            (sbek [obj]
              (cond (ek-vec? obj)     (->> (sort-by :redex/ek-val obj) (mapv #(dissoc % :redex/ek-val)) sbek),
                    (vector? obj)     (mapv sbek obj),
                    (map? obj)        (->> (reduce-kv (fn [m k v] (assoc m k (sbek v))) {} obj)
                                           (reduce-kv (fn [m k v] (if (= k :redex/ek-val) m (assoc m k v))) {})
                                           (into (sorted-map-by compar))),
                    :else             obj))]
      (sbek data))))

(defn redex-restore-values
  "Restoring values might sound like a paleocon objective, but here we are referring to user data
   that was originally not string-valued yet is forced into being a string because it needed to
   serve as :redex/user-key and is qvar-in-key-pos.
   :redex/user-key requires :db.type/string and such data, serving as :db.unique/identity can't be boxed.
   util/read-str is used to restore the value of :redex/user-key."
  [data schema]
  (let [restore-attr? (reduce-kv (fn [res k v]
                                   (if (contains? v :redex/original-key-type) ; Schema entry has this only if qvar-in-key-pos...
                                     (conj res k)                           ; ...and bsets indicate that data isn't :db.valueType/string.
                                     res))
                                 #{} schema)]
    (letfn [(rrv [obj]
              (cond (map? obj)     (if (and (some #(contains? obj %) restore-attr?)
                                            (contains? obj :redex/user-key))
                                     (-> (reduce-kv (fn [m k v] (assoc m k (rrv v))) {} obj)
                                         (update :redex/user-key util/read-str))
                                     (reduce-kv (fn [m k v] (assoc m k (rrv v))) {} obj))
                    (vector? obj)  (mapv rrv obj)
                    :else          obj))]
      (rrv data))))

(defn redex-data-cleanup
  "Return the evaluated express body in the form expected of output from $reduce on
   and express body. This entails:
     1) user express body keys replacing :_rm 'domain' keys,
     2) maps, vectors, and values replacing :redex/more :redex/vals and :redex/val respectively,
     3) boxed values unboxed,
     4) and (by default), things sorted nicely.
   schema argument should be 'full-schema' (containing :_rm entries).
   key-order argument is the order of keys found in the base body."
  [data key-order schema reduce-body]
  (-> data
      unbox
      (redex-restore-values schema)
      (redex-keys-values reduce-body)
      #_redex-rem-keys ; ToDo: I don't think this should be necessary.
      (sort-by-body key-order)))

(defn known-lookup?
  "Return true if the k and value is already present in the lookups.
   There are separate implementations for each database type."
  [lookup-refs k v db]
  (case db
    :datascript  (some #(let [ref (dissoc % :db/id)]
                          (and (= k (-> ref keys first))
                               (= v (-> ref vals first))))
                       lookup-refs)
    :datahike    (some #(let [[_ _ kk vv] %]
                          (and (= k kk) (= v vv)))
                       lookup-refs)))

(defn create-lookup-refs
  "Return a vector of datahike:    {:db/add -1 attr val}
                      datascript:  {:db/id   n attr val}, where n={1,2,3...}
   corresponding to entities to establish before sending data.
   From datahike api.cljc:
                      ;; create a new entity ('-1', as any other negative value, is a tempid
                      ;; that will be replaced by Datahike with the next unused eid)
                      (transact conn [[:db/add -1 :name \"Ivan\"]])"
  [schema data cljs?]
  (let [db (if cljs? :datascript :datahike)
        lookup-refs (atom #{})
        cnt (atom 0)
        ref-ident? (reduce-kv (fn [r k v] (if (contains? v :db/unique) (conj r k) r)) #{} schema)]
    (letfn [(dlr [obj]
              (cond (map? obj)    (doseq [[k v] (seq obj)] ; v should already be a string. ToDo: It isn't!
                                    (when (and (ref-ident? k) (not (known-lookup? @lookup-refs k v db)))
                                        (swap! lookup-refs conj (case db
                                                                  :datascript {:db/id (swap! cnt inc) k v}
                                                                  :datahike   [:db/add -1 k v])))
                                    (dlr v))
                    (vector? obj) (doall (map dlr obj))))]
      (dlr data)
      (-> @lookup-refs vec))))

(defn data-with-lookups
  "(1) Rewrite the data to use lookup refs.
   (2) Ensure that :redex/user-key is a string. If necessary, this will be undone later."
  [schema data]
  (let [ref-ident? (reduce-kv (fn [r k v] (if (contains? v :db/unique) (conj r k) r)) #{} schema)]
    (letfn [(dal [obj] (cond
                         (map? obj)         (reduce-kv (fn [m k v]
                                                         (if (ref-ident? k)
                                                           (assoc m :db/id [k v])
                                                           (assoc m k (dal v))))
                                                       {} obj)
                         (vector? obj)      (mapv dal obj)
                         :else obj))]
      (dal data))))

(defn bset-db-types
  "Return a map each qvar's db-type (e.g :db.type/string) except in cases where the bsets do not exhibit a homogeneous type.
   In that case the qvar's value in the map returned is set to :redex/must-be-boxed." ; _rm/must-be-boxed is not yet used.
  [bsets]
  (as-> (reduce-kv (fn [m k v] (assoc m k (util/db-type-of v))) {} (first bsets)) ?types
    (reduce (fn [res qvar]
              (if (every? (fn [val] (= (get ?types qvar) (util/db-type-of val)))
                          (map #(get % qvar) (rest bsets)))
                res
                (assoc res qvar :redex/must-be-boxed)))
            ?types
            (keys ?types))))

(defn update-schema-for-bsets
  "(1) The schema necessarily uses strings for :redex/user-key.
       However, it the user's data might use something else (particularly numbers).
       Here we check whether bset qvars that are also :redex/user-key qvar are in fact strings.
       Where they are not (e.g. when they are numbers), we add :redex/orignal-key-type to the schema entry."
  [schema bsets]
  (let [bset-type (bset-db-types bsets)]
    (reduce-kv (fn [m k v]
                 (let [v-type (bset-type (:redex/user-key v))]
                   (if (and (qvar? (:redex/user-key v))           ; :redex/user-key is used in two ways. If the schema indicates an express key...
                            (not   (= :db.type/string v-type))  ; ...then just use the qvar as-is. Otherwise, it is qvar-in-key-pos...
                            (not   (:redex/exp-key? v)))          ; ...and the original key type must be restored by util/read-str.
                     (assoc m k (assoc v :redex/original-key-type v-type))
                     (assoc m k v))))
               {}
               schema)))

(defn reduce-express
  "This function performs $reduce on an express function; *in-reduce?* = true.
   Prior to this call, query/schematic-express-body defined metadata (:bi/reduce-body and :bi/schema).
   These two are produced by calling bi/reduce-body-and-schema with the sole argument being the base-body.
   The data is built-up according to the schema, the reduce-body and the b-sets.
   The data is set to the value of :redex/ROOT.
   Lookup-refs are used, and the data is pushed into a database. The ROOT is pulled back, and cleaned up.
   The result is the value of the the express body reduced by the data."
  ([b-sets efn] (reduce-express b-sets efn {}))
  ([b-sets efn _init] ; ToDo: Don't know what to do with the init!
   (let [full-schema (update-schema-for-bsets (-> efn meta :bi/schema) b-sets)
         base-data   (->> (mapv efn b-sets) walk/keywordize-keys)
         lookup-refs (create-lookup-refs full-schema base-data (util/cljs?))
         data        (data-with-lookups full-schema base-data)
         #_[{:redex/user-key #:box{:string-val "abc"}, :db/id [:redex/abc--abc "abc"], :redex/vals {:box/number-val 1}}
          {:redex/user-key #:box{:string-val "abc"}, :db/id [:redex/abc--abc "abc"], :redex/vals {:box/number-val 2}}
          {:redex/user-key #:box{:string-val "abc"}, :db/id [:redex/abc--abc "abc"], :redex/vals {:box/number-val 3}}]

         db-schema   (qu/schema-for-db full-schema (if (util/cljs?) :datascript :datahike))
         #_#_zippy       (reset! diag {:schema      full-schema
                                   :efn         efn
                                   :lookup-refs lookup-refs
                                   :reduce-body (-> efn meta :bi/reduce-body)
                                   :base-body   (-> efn meta :bi/base-body)
                                   :db-schema   db-schema
                                   :b-sets      b-sets
                                   :base-data   base-data
                                   :data        data})
         db-atm      (qu/db-for! [] :known-schema db-schema :learn? false)]
     ;;(swap! diag #(assoc % :db-atm db-atm))
     ;; Inserting objects lookup-refs and data one at a time helps us catch errors.
     (if (util/cljs?)
       (d/transact db-atm lookup-refs)
       (doseq [obj lookup-refs] (d/transact db-atm [obj]))) ; lookup-refs MUST be one at a time in datahike!?!
     ;; There can be multiple roots.
     ;; That makes sense to me when the body can starts with a qvar-in-key-pos.
     (if (util/cljs?)
       (doseq [obj data] (d/transact db-atm [{:redex/ROOT obj}]))
       (doseq [obj data] (d/transact db-atm {:tx-data [{:redex/ROOT obj}]}))) ; This one good for debugging.
     ;; ToDo: There can also be multiple roots in other situations. There is duplicate data!
     ;;  {:db/id 16, :redex/ROOT [#:db{:id 13}]}
     ;;  {:db/id 18, :redex/ROOT [#:db{:id 13}]}
     ;; This implies one extra, useless entry per b-set. Can it be avoided? Maybe not;
     ;; What the data indicates is that "more than one thing can have a :redex/ROOT". Hard to argue with that!
     (let [root-eids (d/q '[:find [?ref ...] :where [?top :redex/ROOT ?ref]] @db-atm)
           pre-clean (mapv #(util/resolve-db-id {:db/id %} db-atm #{:db/id}) root-eids)]
       ;;(swap! diag #(-> % (assoc :root-eids root-eids) (assoc :db-atm db-atm) (assoc :pre-clean pre-clean)))
       (redex-data-cleanup pre-clean (-> efn meta :bi/key-order) full-schema (-> efn meta :bi/reduce-body))))))

;;;---- not express ------------
;;; ToDo: update the schema. This doesn't yet do what its doc-string says it does!
(defn update-db
  "DB is a DH database. Create a new one based on the existing one's content and the new data.
   This entails updating the schema."
  [db data]
  (d/transact db data))

(defn $addSchema
  "Add knowledge of schema to an existing DB.
   This can be a computational expensive operation when the DB is large."
  [mc schema-data db-name]
  (let [type (cond (-> mc :sources (contains? db-name)) :sources
                   (-> mc :targets (contains? db-name)) :targets
                   :else (throw (ex-info "No such database:" {:db-name db-name})))]
    (update-in mc [type db-name] #(update-db % schema-data))))

;;; ToDo: The Dataweave examples suggest to me that they assume that the order of keys in a map
;;;       are as they appear when you print. These are supposed to be maintained?
(defn $mapObject
  "Apply the argument function to each key/value pair (and optionally the index of the pair) of the argument object.
   Each call to the function (one each for each key/value pair) should return an object containing one key-value pair.
   These are integrated into the result object such that the key of the object returned by the function becomes
   a key of the accummulated object, and the value returned by the function becomes the associated value.

   The function specified shall provide 2 or 3 argument: f[key, value, (index)].
   When index is provided, it will be called in turn with the value 0,1,2... for each call.

   If the function returns multiple key/value pairs containing the same key, the resulting object shall contain
   the key/value pair from the last such pair processed."
  [obj fun] ; ToDo: Check arg types.
  (let [arg-cnt (-> fun meta :bi/params count)]
    (cond (== arg-cnt 2)  (reduce-kv (fn [m k v]
                                       (let [ret (fun k v)
                                             kk (-> ret keys first)
                                             vv (-> ret vals first)]
                                         (assoc m kk vv)))
                                     {}
                                     obj)
          (== arg-cnt 3)  (let [cnt (count obj)]
                            (reduce (fn [m [k v i]]
                                      (let [ret (fun k v i)
                                            kk (-> ret keys first)
                                            vv (-> ret vals first)]
                                        (assoc m kk vv)))
                                    {}
                                    (for [i (range cnt)
                                          k (keys obj)
                                          v (vals obj)]
                                      [k v i])))
          :else (throw (ex-info "Second argument to $mapObject should be a function of 2 or 3 arguments."
                                {:arg-cnt arg-cnt})))))

;;; $qIdent({'id' : [123, 456], 'aAttr' : {'val' : 'A-value'}}) ==> [[?e1 :id ?v1] [?e1 :aAttr ?e2] [?e2 :val ?v2]]
(defn $qIdent
  "Write a query that will capture all the data."
  [data]
  (let [ecnt   (atom 0)
        vcnt   (atom 0)
        res    (atom [])]
    (letfn [(qi [obj]
              (if (map? obj)
                (do (swap! ecnt inc)
                    (let [ename (-> "?e" (str @ecnt) symbol)]
                      (doseq [[k v] (seq obj)]
                        (if (map? v) ; ToDo: I don't need an estack e.g. (let {estack (atom '())...] do I?
                          (do (swap! res #(conj % (with-meta [ename (keyword k) (-> "?e" (str (inc @ecnt)) symbol)]
                                                    {:query-form? true})))
                              (qi v))
                          (do (swap! vcnt inc)
                              (let [vname (-> "?v" (str @vcnt) symbol)]
                                (swap! res #(conj % (with-meta [ename (keyword k) vname]
                                                      {:query-form? true})))))))))
                obj))]
      (qi data)
      (with-meta @res {:qident-forms? true})))) ; ToDo: Maybe qident forms should be first class data. Is it?

;;; BTW, I think the task of collecting vector content belongs to redex,
;;;      whereas the number of forms created by $map is equal to the number of bsets.
(defn $eIdent
  "Write an express form that, when provided with complete bsets, will express all the data."
  [data]
  (let [vcnt  (atom 0)]
    (letfn [(ei [obj]
              (cond (map? obj)      (reduce-kv (fn [m k v]
                                                 (if (map? v)
                                                   (assoc m k (ei v))
                                                   (do (swap! vcnt inc)
                                                       (assoc m k (-> "?v" (str @vcnt) symbol)))))
                                               {}
                                               obj)
                    (vector? obj)   (do (swap! vcnt inc) (-> "?v" (str @vcnt) symbol))
                    :else obj))]
      (ei data))))

(defn $identities
  "Return a map with 'query' and 'express' keys for which the values are strings of corresponding
   query that will capture all the data and return it as is."
  [data]
  {"query"   ($qIdent data)
   "express" ($eIdent data)})

;;; ===== These were conceived for the Dataweave example; they might not survive.
(defn $reduceKV
  "Reduce INIT by calling FUN with each key/value pair of OBJ.
   Signature: $reduceKV (obj, function [, init])"
  ([obj fn] ($reduceKV obj fn {}))
  ([obj fn init] (reduce-kv fn init obj)))

(defn $assoc
  [obj k v]
  (assoc obj k v))

(defn $update
  [m k f]
  (update m k f))

;;; https://platform.openai.com/docs/guides/chat/introduction
(def llm-match-instructions
  "A prompt for an LLM to produce a solution for reconciling information in two EDN structures.
   See (1) Wei et al. 'Chain-of-Thought Prompting Elicits Reasoning in Large Language Models'
           https://arxiv.org/abs/2201.11903
       (2) Shizhe Diao et al., 'Active Prompting with Chain-of-Thought for Large Language Models',
           http://arxiv.org/abs/2302.12246"

;;; 1416 tokens! Careful about trailing blanks!
  "Wherever you can, replace each <replace-me> in the target_form with similar information from the source_form.
Both source_form and target_form are Clojure maps.
Because data in source_form does not match data in target_form perfectly, you should do the following to make things work:

(1) If a target_form fields combines data from multiple source_form fields, give that target_form field a value consisting of one field :concat, the value of which is a vector of the field names.

For example, if source_form has specific data fields Company, StreetAddress and BuildingName, but the target_form has only has a general field :AddressLine1,
it might used to accommodate all three of those data fields from the source_form:
##
source_fields:
 :Company \"<company-name-data>\"
 :StreetAddress  \"<street-address-data>\"
 :BuildingName \"<building-name-data>\"

answer:
 :AddressLine1 {:concat [\"<company-name-data>\" \"<street-address-data>\" \"<building-name-data>\"]}
##
(2) Conversely, if source_form has a general field that might contain information for more specific target_form fields, give each of those target_form fields
values that represent extracting the specific data from the more general field as shown:
##
source_field:
 :AddressLine1 \"<address-line-1-data>\"

answer:
 :Company {:extract-from \"<address-line-1-data>\" :value :Company},
 :Street {:extract-from  \"<address-line-1-data>\" :value :Street},
 :BuildingName {:extract-from \"<address-line-1-data>\" :value :Building Name}
##
(3) If there is nothing in source_form that seems to match the needed information in target_form,
just leave the value \"<replace-me>\" in target_form.

Some examples:

source_form 1:
{:Invoice
 {:InvoiceLine
  {:Buyer
   {:Location
    {:Address
     {:CompanyName \"<company-name-data>\"
      :Street \"<street-data>\"
      :BuildingNumber \"<building-number-data>\"
      :City \"<city-data>\"
      :State \"<state-data>\"
      :ZipCode \"<zip-code-data>\"}}
    :TaxID \"<tax-id-data>\"}}}}

target_form 1:
{:Invoice
 {:InvoiceLine
  {:BuyerParty
   {:Location
    {:Address
     {:AddressLine1 \"<replace-me>\"
      :City \"<replace-me>\"
      :State \"<replace-me>\"
      :PostalCode \"<replace-me>\"}}}}}}

answer 1:
{:Invoice
 {:InvoiceLine
  {:BuyerParty
   {:Location
    {:Address
     {:AddressLine1 {:concat [\"<company-name-data>\" \"<street-data>\" \"<building-number-data>\"]}
      :City \"<city-data>\"
      :State \"<state-data>\"
      :PostalCode  \"<zip-code-data>\"}}}}}}
###
source_form 2:
{:Invoice
 {:InvoiceLine
  {:BuyerParty
   {:Location
    {:Address
     {:AddressLine1 \"<address-line-1-data>\"
      :City \"<city-data>\"
      :State \"<state-data>\"
      :PostalCode \"<postal-code-data>\"}}}}}}

target_form 2:
{:Invoice
 {:InvoiceLine
  {:Buyer
   {:Location
    {:Address
     {:CompanyName \"<replace-me>\"
      :Street \"<replace-me>\"
      :BuildingNumber \"<replace-me>\"
      :City \"<replace-me>\"
      :State \"<replace-me>\"
      :ZipCode \"<replace-me>\"}}
    :TaxID \"<replace-me>\"}}}}

answer 2:
{:Invoice
 {:InvoiceLine
  {:BuyerParty
   {:Location
    {:Address
     {:CompanyName {:extract-from \"<address-line-1-data>\" :value :CompanyName}
      :Street {:extract-from \"<address-line-1-data>\" :value :Street}
      :BuildingNumber {:extract-from \"<address-line-1-data>\" :value :BuildingNumber)
      :City \"<city-data>\"
      :State \"<state-data>\"
      :ZipCode  \"<postal-code-data>\"}}
    :TaxID \"<replace-me>\"}}}}
###")

;;; ToDo: Make keys go back two steps.
(defn llm-match-pre
  "Create a Clojure map like used in the $llmMatch prompt from the string thing
   typically created, it has the string '<data>' for values.
   '<data>' is replaced with '<replace-me>' if :replace-me?
   otherwise the kebab-case version of the key inside '<...-data>'."
  [obj replace-me?]
  (letfn [(smp [obj]
            (cond (map? obj)     (reduce-kv
                                  (fn [m k v] (assoc m (keyword k) (if (string? v)
                                                                     (if replace-me?
                                                                       "<replace-me>"
                                                                       (str "<" (csk/->kebab-case-string k) "-data>"))
                                                                     (smp v))))
                                  {} obj)
                  (vector? obj)  (mapv smp obj)
                  :else          obj))]
    (smp obj)))

(defn llm-match-string
  "Return the full string for matching."
  [src tar]
  (let [src3 (with-out-str (-> src (llm-match-pre false) pprint))
        tar3 (with-out-str (-> tar (llm-match-pre true)  pprint))]
    (str llm-match-instructions "\n\n"
         "source_form 3:\n" src3 "\n\n"
         "target_form 3:\n" tar3 "\n\n"
         "answer 3:\n")))

#?(:clj
(defn $llmMatch
  "Find closes match of terminology of keys in two 'object shapes' and thereby produce a mapping
   of the data at those keys. The prompt instructs how to indicate extraction and aggregation
   of source object fields to target object fields."
  [src tar]
  (log/info "$llmMatch on server")
  (let [q-str (llm-match-string src tar)]
       (if-let [key (util/get-api-key :llm)]
         (try (let [res (-> (openai/create-chat-completion {:model "gpt-3.5-turbo-0301"
                                                            :api-key key
                                                            :messages [{:role "user" :content q-str}]})
                            :choices first :message :content)]
                (-> res read-string))
              (catch Throwable e
                (throw (ex-info "OpenAI API call failed."
                                {:message (.getMessage e)
                                 :details (-> e .getData :body json/read-str)}))))
         (throw (ex-info "OPENAI_API_KEY environment variable value not found." {}))))))

 #?(:cljs
(defn $llmMatch
  "Call server for $llmMatch."
  [src tar]
  (log/info "$llmMatch on client")
  (let [prom (p/deferred)]
    (log/info "Call to $llmMatch") ; ToDo: For some reason, this is not printed in console.
    (util/start-clock 30000)
    (POST (str svr-prefix "/api/llm-match") ; ToDo: Use https://github.com/oliyh/martian
          {:params {:src src :tar tar}
           :timeout 30000
           :handler (fn [resp] (p/resolve! prom resp))
           :error-handler (fn [{:keys [status status-text]}]
                            (log/info (str "CLJS-AJAX $llmMatch error: status = " status " status-text= " status-text))
                            (p/rejected (ex-info "CLJS-AJAX error on /api/llm-match" {:status status :status-text status-text})))})
    (log/info "$llmMatch returns promise" prom)
    prom)))

(defn llm-extract-prompt
  "Create a few-shot prompt for extracting particular information from a text field."
  [src seek]
  (cl-format nil
  "The Clojure map provided contains two keys: :source provides a string of information, :seek provides some type of information sought in :source.
   Return only this Clojure map with two additional keys:
     (1) :found is the substring of the string at :source that contains the information sought.
     (2) :probability contains a probability that the value in :found is of the type sought.
   Example:
   {:source \"South Windsor, CT, 06074\" :seek \"city\"}
   {:source \"South Windsor, CT, 06074\" :seek \"city\" :found \"South Windsor\" :probability 0.98}
##
   Example:
   {:source \"NIST Bldg 220\" :seek \"building\"}
   {:source \"NIST Bldg 220\" :seek \"building\" :found \"Bldg 220\" :probability 0.95}
##
   {:source ~S :seek ~S}"
  src seek))

#?(:clj
(defn $llmExtract
  "Use LLM to extract seek argument, a descriptive string from source string.
   Token limit is 3000."
  [src seek]
  (log/info "$llmExtract on server")
  (let [q-str (llm-extract-prompt src seek)]
       (if-let [key (util/get-api-key :llm)]
         (try (let [res (-> (openai/create-completion {:model "text-davinci-003"
                                                       :api-key key
                                                       :max-tokens 3000
                                                       :temperature 0.1
                                                       :prompt q-str})
                            :choices first :text)]
                #_(reset! diag {:res res})
                (-> res read-string))
              (catch Throwable e
                #_(swap! diag #(assoc % :e e))
                (throw (ex-info "OpenAI API call failed."
                                {:message (.getMessage e)}))))
         (throw (ex-info "OPENAI_API_KEY environment variable value not found." {}))))))

 #?(:cljs
(defn $llmExtract
  "Call server for $llmExtract."
  [src seek]
  (log/info "$llmExtract on client")
  (let [prom (p/deferred)]
    (log/info "Call to $llmExtract") ; ToDo: For some reason, this is not printed in console.
    (util/start-clock 30000)
    (GET (str svr-prefix "/api/llm-extract") ; ToDo: Use https://github.com/oliyh/martian
          {:params {:source src :seek seek}
           :timeout 30000
           :handler (fn [resp] (p/resolve! prom resp))
           :error-handler (fn [{:keys [status status-text]}]
                            (log/info (str "CLJS-AJAX $llmExtract error: status = " status " status-text= " status-text))
                            (p/rejected (ex-info "CLJS-AJAX error on /api/llm-extract" {:status status :status-text status-text})))})
    (log/info "$llmExtract returns promise" prom)
    prom)))

;;;====================================================================================================================
;;; Evaluation
;;;====================================================================================================================
(defn pretty-form
  "Replace some namespaces with aliases for diagnostic legability."
  [form]
  (let [ns-alia {"rad-mapper.builtin"  "bi"
                 "bi"                  "bi"
                 "java.lang.Math"      "Math"}] ; ToDo: Make it more general. (Maybe "java.lang" since j.l.Exception too.)
    (letfn [(ni [form]
              (let [m (meta form)]
                (cond (vector? form) (-> (->> form (map ni) doall vec) (with-meta m)),
                      (seq? form)    (-> (->> form (map ni) doall) (with-meta m)),
                      (map? form)    (-> (reduce-kv (fn [m k v] (assoc m k (ni v))) {} form) (with-meta m)),
                      (symbol? form) (-> (if-let [ns (-> form namespace ns-alia)] (symbol ns (name form)) (symbol (name form)))
                                         (with-meta m)),
                      :else form)))]
      (ni form))))

(def macro-subs
  "When rewriting produces any of the symbols corresponding to the keys of the map,
   AND evaluation with Clojure eval is intended, use the corresponding value of the map."
  {"conditional"   'rad-mapper.builtin-macros/conditional-m,
   "init-step"     'rad-mapper.builtin-macros/init-step-m,
   "map-step"      'rad-mapper.builtin-macros/map-step-m,
   "primary"       'rad-mapper.builtin-macros/primary-m,
   #_#_"try-threading" 'rad-mapper.builtin-macros/try-threading-m, ; ToDo: Get SCI dynamic variable working so you don't need this!
   "value-step"    'rad-mapper.builtin-macros/value-step-m})

(defn macro?
  "Return a ns-qualified -macro symbol substituting for the argument symbol created from rewriting.
   This is used where a macro is intended (i.e. when using clojure eval, not SCI, for the evaluation)."
  [sym]
  (let [n (name sym)]
    (when (#{"rad-mapper.builtin" "bi"} (namespace sym))
      (get macro-subs n))))

(defn rad-form
  "Walk the form replacing the namespace alias 'bi' with 'rad-mapper.builtin' except where the var is an :sci/macro.
      - If it is an :sci/macro and running sci, drop the namespace altogether; sci doesn't like them.
      - If it is an :sci/macro and not running sci, replace it with the corresponding macro.
      - Wrap the form in code for multiple evaluation when it returns a primary fn."
  [form sci?]
  (let [ns-alia {"bi" "rad-mapper.builtin"}]
    (letfn [(ni [form]
              (cond (vector? form) (->> form (map ni) doall vec),
                    (seq? form)    (->> form (map ni) doall),
                    (map? form)    (->> (reduce-kv (fn [m k v] (assoc m k (ni v))) {} form) doall)
                    (symbol? form) (cond (and sci? (macro? form))        (-> form name symbol) ; SCI doesn't like ns-qualified.
                                         (macro? form)                   (macro? form)         ; Not sci; use a real macro named <x>-m.
                                         :else                           (if-let [nsa (-> form namespace ns-alia)]
                                                                           (->> form name (symbol nsa))
                                                                           form)),
                    :else form))]
      `(do
         (rad-mapper.builtin/reset-env)
         (rad-mapper.builtin/finalize ~(ni form))))))

;;; Having these here rather than rad-mapper.builtin allows this file to be required by builtin.
;;; Thus we can do eval from inside builtin. That's only needed in special cases like
;;; $get([['library/fn', 'schemaParentChild'],['fn/exe']])
(def value-step ^:sci/macro
  (fn [_&form _&env body]
      `(-> (fn [& ignore#] ~body)
           (with-meta {:bi/step-type :bi/value-step :body '~body}))))

(def primary ^:sci/macro
  (fn [_&form _&env body]
    `(-> (fn [& ignore#] ~body)
         (with-meta {:bi/step-type :bi/primary}))))

(def init-step ^:sci/macro
  (fn [_&form _&env body]
    `(-> (fn [_x#] ~body)
         (with-meta {:bi/step-type :bi/init-step :bi/body '~body}))))

(def map-step ^:sci/macro
  (fn [_&form _&env body]
    `(-> (fn [_x#] ~body)
         (with-meta {:bi/step-type :bi/map-step :body '~body}))))

;;; Implements the JSONata-like <test> ? <then-exp> <else-exp>."
(def conditional ^:sci/macro
  (fn [_&form _&env condition e1 e2]
    `(let [cond# ~condition
           answer# (if (fn? cond#) (cond#) cond#)]

       (cond (or (and (fn? cond#) (cond#))
                 (and (not (fn? cond#)) cond#))   (let [res# ~e1]
                                                    (if (fn? res#) (res#) res#))
             :else                                (let [res# ~e2]
                                                    (if (fn? res#) (res#) res#))))))
(def ctx (let [publics        (ns-publics 'rad-mapper.builtin)
               publics-m      (ns-publics 'rad-mapper.builtin-macros)
               bns            (sci/create-ns 'rad-mapper.builtin)
               bns-m          (sci/create-ns 'rad-mapper.builtin-macros)
               #_#_pns            (sci/create-ns 'pprint-ns)
               tns            (sci/create-ns 'timbre-ns)
               builtin-ns     (update-vals publics   #(sci/copy-var* % bns))
               builtin-m-ns   (update-vals publics-m #(sci/copy-var* % bns-m))
               #_#_pprint-ns      {'cl-format (sci/copy-var* #'clojure.pprint/cl-format pns)}
               timbre-ns      {'-log!     (sci/copy-var* #'taoensso.timbre/-log! tns)
                               '*config*  (sci/copy-var* #'taoensso.timbre/*config* tns)}
               nspaces        {'rad-mapper.builtin               builtin-ns,
                               'rad-mapper.builtin-macros        builtin-m-ns,
                               'taoensso.timbre                  timbre-ns,
                               #_#_'clojure-pprint               pprint-ns}]
           (sci/init
            {:namespaces #?(:clj nspaces :cljs (merge nspaces scip/namespaces))  ; for promesa.core and promesa.protocols.
             ;; ToDo: SCI doesn't seem to want namespaced entries for macros. <=== See https://github.com/babashka/sci.configs/blob/main/src/sci/configs/funcool/promesa.cljs
             :bindings  {'init-step    init-step
                         'map-step     map-step
                         'value-step   value-step
                         'primary      primary
                         'conditional  conditional}})))

;;; To remain safe and sandboxed, SCI programs do not have access to Clojure vars, unless you explicitly provide that access.
;;; SCI has its own var type, distinguished from Clojure vars.
(defn user-eval
  "Evaluate the argument form."
  [full-form opts]
  (let [min-level (util/default-min-log-level)
        run-sci?   (or (util/cljs?) (:sci? opts))]
    (when (or (:debug-eval? opts)  ; I'm suppressing this in run-sci?/cljs. Need to investigate taoensso.timbre/*config* #{"*"} :debug.
              (and (not run-sci?) (= min-level :debug)))
      (util/config-log :info) ; ToDo: :debug level doesn't work with cljs (including SCI sandbox).
      (log/info (cl-format nil "*****  Running ~S *****" (if run-sci? "SCI" "eval")))
      (-> full-form pretty-form pprint))
    (try
      ;;(s/check-asserts (:check-asserts? opts)) ; ToDo: Investigate why check-asserts? = true is a problem
      ;(sci/binding [bim/$ nil] ; <==== I get from SCI: "Error: Can't dynamically bind non-dynamic var [object Object]" yet this IS ^:dynamic.
        (sci/binding [sci/out *out*]
          (if run-sci?
            (sci/eval-form ctx full-form)
            #?(:clj (try (-> full-form str util/read-str eval) ; Once again (see notes), just eval doesn't work!
                         (catch Throwable e   ; Is this perhaps because I didn't have the alias for bi in builtin.cljc? No.
                           (ex-info "Failure in clojure.eval:" {:error e})))
               :cljs :never-happens)))
      (finally (util/config-log min-level)))))

(defn user-eval-devl
  "Evaluate the argument form. For use in REPL. Form is anything executable in ctx."
  [form]
  (let [min-level (util/default-min-log-level)]
    (util/config-log :info) ; ToDo: :debug level doesn't work with cljs (including SCI sandbox). Use println for now.
    (log/info (cl-format nil "*****  Running SCI *****"))
    (try
      (sci/binding [sci/out *out*] ; Would want bim/$ here too. (See above)
        (sci/eval-form ctx form))
      (finally (util/config-log min-level)))))

(declare processRM)

(defn combine-code-and-data
  "Return a new problem for use with processRM consisting of the argument data and code.
   This is typically used with the exerciser, where the user has the opportunity to
   specify data in a separate editor window."
  [code data]
  (let [pdata (as-> data ?d ; Remove last \; to test parsing of data. "(<assign>; <assign>; <assign>)"
                (str/trim ?d)
                (let [last-ix (-> ?d count dec)]
                  (if (= \; (get ?d last-ix))
                    (subs ?d 0 last-ix)
                    ?d)))
        [_ code-body] (re-matches #"(?s)\s*\((.+)\)\s*" code) ; Remove surrounding \( ... \) if any (could just be one expression).
        pcode (or code-body code)]
    (processRM :ptag/jvar-decls (cl-format nil "(~A)" pdata)) ; Will throw if not okay.
    (cl-format nil "(~A; ~A)" pdata pcode)))

(declare pprint-obj pprint-top)

(defn processRM
  "A top-level function for all phases of translation.
   parse-string, rewrite, and execute, but with controls to quit before doing all of these, debugging etc.
   With no opts it returns the parse structure without debug output."
  ([tag code] (processRM tag code {}))
  ([tag code opts]
   (assert (every? #(#{:user-data :rewrite? :executable? :execute? :sci? :debug-eval?
                       :debug-parse? :debug-rewrite? :pprint?} %)
                   (keys opts)))
   (let [code         (if-let [udata (-> opts :user-data not-empty)]
                       (combine-code-and-data code udata)
                       code)
         execute?    (or (:pprint? opts) (:execute? opts))
         rewrite?    (or (:pprint? opts) (:execute? opts) (:executable? opts) (:rewrite? opts))
         executable? (or (:pprint? opts) (:execute? opts) (:executable? opts))
         sci?        (or (:sci? opts) (util/cljs?))
         ps-atm (atom nil)] ; An atom just to deal with :clj with-open vs :cljs.
     (binding [rewm/*debugging?* (:debug-rewrite? opts)
               pm/*debugging?*   (:debug-parse? opts)]
       ;; Note that s/check-asserts = true can produce non-JSONata like behavior by means of
       ;; throwing an error where JSONata might just return ** No Match **
       (if (or rewm/*debugging?* pm/*debugging?*) (s/check-asserts true) (s/check-asserts false))
       #?(:clj  (with-open [rdr (-> code char-array clojure.java.io/reader)]
                  (as-> (par/make-pstate rdr) ?ps
                    (pm/parse tag ?ps)
                    (dissoc ?ps :line-seq) ; dissoc so you can print it.
                    (assoc ?ps :parse-status (if (-> ?ps :tokens empty?) :ok :premature-end))
                    (reset! ps-atm ?ps)))
          :cljs (as-> (par/make-pstate code) ?ps
                  (pm/parse tag ?ps)
                  (assoc ?ps :parse-status (if (-> ?ps :tokens empty?) :ok :premature-end))
                  (reset! ps-atm ?ps)))
       (case (:parse-status @ps-atm)
         :premature-end (log/error "Parse ended prematurely")
         :ok (let [res (cond-> {:typ :toplevel :top (:result @ps-atm)}
                         (not rewrite?)      (:top)
                         rewrite?            (rew/rewrite)
                         executable?         (rad-form sci?)
                         execute?            (user-eval opts))]
               (if (:pprint? opts) (pprint-top res) res)))))))

(defn indent-additional-lines
  "Indent every line but the first by the amount indicated by indent."
  [s indent]
  (let [spaces (util/nspaces indent)
        [one & others] (str/split-lines s)]
    (cl-format nil "~A~{~%~A~}"
               one
               (map #(str spaces %) others))))

(def name-order
  "The values here are listed in the order in which they should appear."
  {"schema"      ["name" "shortname" "type" "sdo" "spec" "version" "subversion" "topic" "pathname" "content"]
   "model"       ["name" "elementDef" "elementRef" "complexType" "sequence" "union" "extension"]
   "element"     ["name" "ref" "id" "sequence" "complexType" "simpleType"]
   "complexType" ["name" "id"]
   "codeList"    ["name" "id" "terms" "restriction" "union"]})

(def ns-order
  "This is the order that things should be presented when they have different namespaces.
   after these nspaces, it is alphabetical (so that 'xsd' is towards the end."
  ["schema" "element" "model" "codeList" "complexType" "component"  "cct" "attribute" "has"])

;;;==========  Pretty printing. This is unrelated to the above uses of the term pretty-print. ====================
;;; Maybe forget this and try json.stringify? / (json/pprint obj)
;;; The functions for pprint each return a string that may have line breaks.
;;; The string starts with no spaces, the spaces to place after a line-break are passed in as "ident"
;;; The map and vector functions add indentation to the values, which may themselves be multi-line.
(defn sort-map
  "Sort maps so that
    - '*/name' comes first, xsd/* comes last, */documentation comes next to last.
    - schema/* stuff is ordered by schema/name, schema/type ... with schema/content last."
  [m]
  (letfn [(compare-names [ns x y]
            (if-let [v (get name-order ns)]
              (let [xi (as-> (.indexOf v x) ?i (if (neg? ?i) nil ?i))
                    yi (as-> (.indexOf v y) ?i (if (neg? ?i) nil ?i))]
                (cond (and xi yi)  (if (< xi yi) -1 +1)
                      xi           -1
                      yi           +1
                      :else        (compare x y)))
              (compare x y)))
          (compare-namespaces [x y]
              (let [xi (as-> (.indexOf ns-order x) ?i (if (neg? ?i) nil ?i))
                    yi (as-> (.indexOf ns-order y) ?i (if (neg? ?i) nil ?i))]
                (cond (and xi yi)  (if (< xi yi) -1 +1)
                      xi           -1
                      yi           +1
                      :else        (compare x y))))
          (compar [x y]
            (let [nsp-x (if (or (symbol? x) (keyword? x)) (namespace x) "")
                  nsp-y (if (or (symbol? y) (keyword? y)) (namespace y) "")
                  name-x (name x)
                  name-y (name y)]
              (cond (and nsp-x nsp-y)           (if (= nsp-x nsp-y)
                                                  (compare-names nsp-x name-x name-y)
                                                  (compare-namespaces nsp-x nsp-y))
                    nsp-x                       -1
                    nsp-y                       +1
                    :else                       (compare name-x name-y))))]
    (into (sorted-map-by compar) m)))

(defn sort-obj
  "Recursively sort the maps in the given object.
   Everything else, of course, is left alone."
  [obj]
  (cond (map? obj)      (->> obj (reduce-kv (fn [m k v] (assoc m k (sort-obj v))) {}) sort-map)
        (vector? obj)   (mapv sort-obj obj)
        :else           obj))

;;; ToDo: Consider the idea of creating an intermediate object in which all the parts are serialized. THEN print that.
;;;       I think that gets around a lot of the weirdness and duplication here.
(defn pprint-map
  "Print a map:
     (1) If it all fits within width minus indent, print it that way.
     (2) If the longest key/value it fits within width minus indent, print it with keys and values on the same line,
     (3) If it does not fit, print the value on a second line, with additional indentation relative to its key.
  - ident is the column in which this object can start printing.
  - width is column beyond which print should not appear."
  [obj indent width]
  (cond (empty? obj) "{}",
        (= obj {"db_connection" "_rm_schema-db"}) "<<connection>>",
        :else (let [kv-pairs (reduce-kv (fn [m k v] ; Here we get the 'dense' size; later calculate a new rest-start
                                          (let [kk (pprint-obj k :width width)
                                                vv (pprint-obj v :width width)]
                                            (conj m {:k kk :v vv
                                                     :k-len (count kk)
                                                     :v-len (->> vv str/split-lines (map count) (apply max))})))
                                        []
                                        obj)
                    max-key (apply max (map :k-len kv-pairs)) ; We will line them all up with the widest.
                    max-val (apply max (map :v-len kv-pairs))]
                (if (>= (- width indent) (+ (apply + (map :k-len kv-pairs)) (apply + (map :v-len kv-pairs)) (* (count kv-pairs) 3)))
                  ;; (1) The map fits on one line.
                  (cl-format nil "{~{~A: ~A~^, ~}}" (interleave (map :k kv-pairs) (map :v kv-pairs)))
                  ;; It doesn't fit on one line.
                  (let [indent-spaces (util/nspaces indent)
                        key-strs (into (-> kv-pairs first :k str vector)
                                       (map #(str indent-spaces (:k %) (util/nspaces (- max-key (:k-len %)))) (rest kv-pairs)))
                        ;; Values may have line breaks, each line but the first needs the indent plus the max-key
                        val-strs (mapv #(indent-additional-lines (:v %) (+ indent 2)) kv-pairs)]
                    (if (<= (+ max-key max-val) (- width indent))
                      ;; (2) It fits. The values are made to line up with the entry with the longest key.
                      (cl-format nil "{~{~A: ~A~^,~% ~}}" ; one space after ~% because starts with a '{'
                                 (interleave key-strs val-strs))
                      ;; (3) Too wide; break into two lines for each k/v pair.
                      ;;     Now all the values need to be indented
                      ;;     The cl-format indents the values a few spaces relative to their key.
                      (cl-format nil "{~{~A:~%  ~A~^,~%  ~}}"
                                 (interleave
                                  key-strs
                                  (map #(str indent-spaces %) val-strs)))))))))

(defn pprint-vec
  "Return a string representing vector:
    If it all fits on one line within width minus indent, the returned string contains now newlines.
    Else each element is on a new-line. (In fact, the elements themselves can run multiple lines.
    The first element no indentation, other are preceded by indent spaces."
  [obj indent width]
  (let [query-form? (-> obj meta :query-form?)
        elem-objs (reduce (fn [res elem]
                            (let [s (pprint-obj elem :width width :indent indent)]
                              (conj res {:val s :len (->> s str/split-lines (map count) (apply max))})))
                          [] obj)]
    (if (>= (- width indent) (+ (apply + (map :len elem-objs)) (* (count elem-objs) 2))) ; All on one line?
      (if query-form?
        (cl-format nil "[~{~A~^ ~}]"  (map :val elem-objs))
        (cl-format nil "[~{~A~^, ~}]" (map :val elem-objs)))
      ;; Not printable on one line, so every line but the first need the indent.
      ;; And the values might be multi-line; every line but the first needs the indent.
      (let [spaces (util/nspaces indent)
            elems (->> elem-objs (map :val) (map #(indent-additional-lines % indent)))
            val-strs (into (-> elems first vector)
                           (->> elems rest (map #(str spaces %))))]
        (if query-form?
          (cl-format nil "[~{~A~^~% ~}]" val-strs)
          (cl-format nil "[~{~A~^,~% ~}]" val-strs))))))

(def print-width "Number of characters that can be comfortably fit on a line.
                  This is an atom so that it can be set by other libraries."
  (atom 120))

(defn pprint-obj
  "Pretty print the argument object.
   (This tries to print the content within the argument width, but because we assume
    that there is a horizontal scrollbar, it doesn't work too hard at it!)
     - width: the character length of the total area we have to work with.
     - indent: a number of space characters per nesting level,
     - depth: the number of nesting levels.
     - start: a number of characters to indent owing to where this object starts because of key for which it is a value."
  [obj & {:keys [indent width] :or {indent 0 width @print-width}}]
  ;;(log/info "pprint-obj: obj = " obj)
  (letfn [(pp [obj accum]
            (cond (string? obj)    (str accum  "'" obj "'")
                  (number? obj)    (str accum obj)
                  (map? obj)       (str accum (pprint-map obj indent width))
                  (vector? obj)    (str accum (pprint-vec obj indent width))
                  (keyword? obj)   (if-let [ns (namespace obj)]
                                     (str accum "'" ns "_" (name obj) "'")
                                     (str accum "'" (name obj) "'"))
                  (fn? obj)        (str accum "<<Function>>")
                  (exception? obj) (str accum (cl-format nil "<<Error message:  ~A~%           data:  ~S >>"
                                                         (ex-message obj) (ex-data obj))),
                  :else            (str accum obj)))]
    (as-> obj ?o
      (if (map? ?o)                 (sort-obj ?o) ?o)
      (if (vector? ?o)              (sort-obj ?o) ?o)
      (reset! diag (pp ?o "")))))

(defn resolved-obj
  "Replace the promises in an object with their resolution.
   This function expects an object where all the promises are fulfilled."
  [obj]
  (letfn [(ro [x]
            (cond (map? x)       (reduce-kv (fn [m k v] (assoc m (ro k) (ro v))) {} x)
                  (vector? x)    (mapv #(ro %) x)
                  (p/promise? x) (p/extract x)
                  :else          x))]
    (ro obj)))

(defn pprint-top
  "Top-level of pretty printing. Collect all the promises in the object.
   when they are all resolved (p/all), call pprint-top again with all those objects resolved.
   Of course, promises might resolve to promises, so print-top recursively calls itself
   until no promises left."
  [obj]
  (let [proms (atom [])]
    (letfn [(cproms [x] (cond
                          (map? x)       (doseq [[k v] (seq x)]
                                           (if (p/promise? k) (swap! proms conj k) (cproms k))
                                           (if (p/promise? v) (swap! proms conj v) (cproms v)))
                          (vector? x)    (doseq [elem x] (if (p/promise? elem) (swap! proms conj elem) (cproms elem)))
                          (p/promise? x) (swap! proms conj x)))]
      (cproms obj)
      (if (empty? @proms)
        (pprint-obj obj)
        (-> (p/all @proms)
            (p/then (fn [_] (-> obj resolved-obj pprint-top)))
            (p/catch #(log/info "Error in evaluation:" %)))))))
