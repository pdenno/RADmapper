(ns rad-mapper.builtin
  "Built-in functions implementing the expression language of the mapping language.
   Functions with names beginning with a '$' are available to the user (e.g. $filter).
   Others (such as bi/key and bi/strcat) implement other parts of the expression language
   but are not available directly to the user except through the operators (e.g. the dot, and &
   respectively for navigation to a property and concatenation)."
  (:require
   [cemerick.url                      :as url]
   #?(:clj [clojure.data.json         :as json])
   #?(:clj [clojure.data.codec.base64 :as b64])
   [clojure.spec.alpha                :as s]
   [clojure.pprint                             :refer [cl-format]]
   [clojure.string                    :as str  :refer [index-of]]
   [clojure.walk                      :as walk :refer [keywordize-keys]]
   #?(:clj  [dk.ative.docjure.spreadsheet :as ss])
   #?(:clj  [datahike.api                 :as d]
      :cljs [datascript.core              :as d])
   #?(:clj  [datahike.pull-api    :as dp]
      :cljs [datascript.pull-api  :as dp])
   #?(:cljs ["nata-borrowed"  :as nb])
   #?(:cljs [goog.crypt.base64 :as jsb64])
   [rad-mapper.db-util            :as du :refer [box unbox]]
   [rad-mapper.query              :as qu]
   [rad-mapper.util               :as util :refer [qvar?]]
   [taoensso.timbre               :as log :refer-macros[error debug info log!]]
   [rad-mapper.builtin-macros
    :refer [$ $$ set-context! defn* defn$ thread-m value-step-m primary-m init-step-m map-step-m
            jflatten containerize containerize? container? flatten-except-json]])
  #?(:cljs (:require-macros [rad-mapper.builtin-macros]))
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

#?(:clj (alias 'bi 'rad-mapper.builtin))

;;; ToDo:
;;;  - Make sure meta isn't used where a closure would work.
;;;  - Clojure uses earmuffs as a clue to whether a var is dynamic. Should I use *$*?

(declare aref)

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
(s/def ::sbind #(-> % meta :sbind?))

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

(defn again? ; ToDo: Use the rewriter to eliminate this.
  "Primary does double duty:
      (1) wrapping a whole body like ($foo), and
      (2) function for mapping like $x.($y + 1).
   The former is top-level, but returning a function is not what is intended.
   All top-level expressions from rewrite  are wrapped in this to ensure that
   the top-level, when a :bi/primary, evaluates it before returning."
   [res]
  (if (and (fn? res) (= :bi/primary (-> res meta :bi/step-type)))
    (jflatten (res))
    (jflatten res)))

;;; It exists so that rew/wrap-non-path won't wrap it
(defn deref$
  "Expressions such as [[1,2,3], [1]].$ will translate to (bi/run-steps [[1 2 3] [1]] (deref bi/$))
   making it advantageous to have a deref that sets the value and returns it."
  ([] (containerize? @$))
  ([val] ; ToDo: Is this one ever used?
   (set-context! (containerize? val))))

;;;========================= JSONata built-ins  =========================================
(defn* add      "plus"   [x y]   (+ x y))
(defn* subtract "minus"  [x y]   (- x y))
(defn* multiply "times"  [x y]   (* x y))
(defn* div      "divide" [x y]   (s/assert ::non-zero y) (double (/ x y))) ; cljs.core/divide
(defn* gt       "greater-than"          [x y] (>  x y))
(defn* lt       "less-than "            [x y] (<  x y))
(defn* gteq     "greater-than-or-equal" [x y] (>= x y))
(defn* lteq     "less-than-or-equal"    [x y] (<= x y))

(defn eq
  "equal, need not be numbers"
  [x y]
  (= (jflatten x) (jflatten y)))

(defn !=
  "not equal, need not be numbers"
  [x y]
  (= (jflatten x) (jflatten y)))

(def thread ^:sci/macro
  (fn [_&form _&env x y]
    `(let [xarg# ~x]
       (do (set-context! xarg#)
           (let [yarg# ~y]
             (if (fn? yarg#)
               (yarg# xarg#)
               (throw (ex-info "The RHS argument to the threading operator is not a function."
                               {:rhs-operator yarg#}))))))))

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
    (loop [steps steps
           res   @$]
      (if (empty? steps) res
          (let [styp (step-type steps)
                sfn  (first steps)
                new-res (case styp ; init-step, value-step, map-step, thread, and primary use SCI's notion of macros.
                          :bi/init-step    (-> (sfn @$) containerize?),
                          :bi/get-filter   ((second steps) res {:bi/prior-step-type :bi/get-step
                                                                :bi/attr (-> sfn meta :bi/arg)}),
                          :bi/filter-step  (sfn res nil), ; containerizes arg; will do (-> (cmap aref) jflatten) | filterv
                          :bi/get-step     (sfn res nil), ; containerizes arg if not map; will do cmap or map get.
                          :bi/value-step   (do (log/info (cl-format nil "Run step -- value-step vec? = ~S fn = ~S res = ~S"
                                                                  (vector? res) sfn res))
                                               #_(sfn res)
                                               (if (vector? res)
                                                 (mapv #(binding [$ (atom %)] (sfn $)) res)
                                                 (sfn res))),
                          :bi/primary      (if (vector? res) (cmap sfn (containerize? res)) (sfn res)),
                          :bi/map-step     (cmap sfn (containerize? res)),
                          (throw (ex-info "Invalid step" {:sfn  sfn})))]
            ;; ToDo: Wrap next in dynamic *debug-eval?* or some such thing.
            ;;(log/info (cl-format nil "    styp = ~S meta = ~S res = ~S" styp (-> sfn meta (dissoc :bi/step-type)) new-res))
            (recur (if (= styp :bi/get-filter) (-> steps rest rest) (rest steps))
                   (set-context! new-res)))))))

;;; The spec's viewpoint on 'non-compositionality': The Filter operator binds tighter than the Map operator.
;;; This means, for example, that books.authors[0] will select the all of the first authors from each book
;;; rather than the first author from all of the books. http://docs.jsonata.org/processing

;;; ToDo: You can specify multiple conditions (a disjunction)  in filtering http://docs.jsonata.org/processing
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
        (let [obj (-> (if (-> args first empty?) @$ (first args)))]
          (cond (map? obj)      (get obj k)
                (vector? obj)   (->> obj
                                     containerize
                                     (cmap #(get % k))
                                     ;; lightweight flatten
                                     (reduce (fn [res x] (if (vector? x) (into res x) (conj res x))) [])
                                     containerize)
                :else           nil)))
      (with-meta {:bi/step-type :bi/get-step :bi/arg k})))

(def value-step ^:sci/macro
  (fn [_&form _&env body]
      `(-> (fn [& ignore#] ~body)
       (with-meta {:bi/step-type :bi/value-step :body '~body}))))

(defn get-scoped
  "Access map key like clj/get, but with arity overloading for $.

   Don't do a set-context! here; it will mess up 'distribution'.
   For example, setting $ to the value of 'c' in a.b.(c + f) is wrong."
  ([k] (get-scoped @$ k))
  ([obj k] (get obj k)))

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
(defn fncall
  [{:keys [func args]}]
  (if-let [cnt (-> func meta :bi/expected-arg-cnt)]
    (if (== cnt (count args))
      (apply func args)
      (throw (ex-info (cl-format nil "A function of type ~A expected ~A args; it got ~A."
                                 (-> func meta :bi/fn-type) cnt (count args)) {})))
    (apply func args)))

;;;--------------------------- JSONata built-in functions ------------------------------------

;;;------------- String --------------
(defn$ concat-op
  "JSONata & operator."
  [s1_ s2]
  (s/assert ::string s1_)
  (s/assert ::string s2)
  (str s1_ s2))

;;; $base64decode
(defn $base64decode
  "Converts base 64 encoded bytes to a string, using a UTF-8 Unicode codepage."
  [c]
#?(:clj  (-> c .getBytes b64/decode String.)
   :cljs (jsb64/decodeString c)))

;;; $base64encode
(defn $base64encode
  "Converts an ASCII string to a base 64 representation.
   Each each character in the string is treated as a byte of binary data.
   This requires that all characters in the string are in the 0x00 to 0xFF range,
   which includes all characters in URI encoded strings.
   Unicode characters outside of that range are not supported."
  [s]
  #?(:clj  (-> s .getBytes b64/encode String.)
     :cljs (jsb64/encodeString s)))

;;;  ToDo: (generally) when arguments do not pas s/assert, need to throw an error.
;;;        For example, "Argument 1 of function "contains" does not match function signature."
;;;        This could be built into defn$ and defn$ could be generalized and used more widely.
;;; $contains
(defn$ $contains
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
(defn $decodeUrl
  [url-string]
  (s/assert ::string url-string)
  (url/url-decode url-string))

;;; $decodeUrlComponent
(defn $decodeUrlComponent
  [s]
  (s/assert ::string s)
  (url/url-decode s))

;;; $encodeUrl
(defn $encodeUrl
  [s]
  (s/assert ::string s)
  (-> s url/url str))

;;; $encodeUrlComponent
(defn $encodeUrlComponent
  [s]
  (s/assert ::string s)
  (url/url-encode s))

;;; $eval
#?(:clj
(defn $eval
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
(defn $length [s] (s/assert ::string s) (count s))

;;; $lowercase
(defn $lowercase
  "Returns a string with all the characters of str converted to lowercase.
   If str is not specified (i.e. this function is invoked with no arguments),
   then the context value is used as the value of str. An error is thrown if str is not a string."
  ([]  ($lowercase @$))
  ([s] (s/assert ::string s) (str/lower-case s)))

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
         result (loop [res []
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
;;; ToDo: It seems like there are lots of opportunities to use defn$ where I'm not using it. Should I care?
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
(defn $string
  "Return the argument as a string."
  [s] (str s))

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

;;; $trim
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
(defn $number
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
(defn $max
  "Return the largest the numeric argument (an array or singleton)."
  [v_]
  (let [v (singlize v_)]
    (s/assert ::numbers v)
    (apply max v)))

;;; $min
(defn $min
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

;;; $sum
(defn $sum
  "Take one number of a vector of numbers; return the sum."
  [nums]
  (let [v (singlize nums)]
    (s/assert ::numbers v)
     (apply + v)))

;;; $sqrt
(defn $sqrt
  "Returns the square root of the argument."
  [v_]
  (s/assert ::number v_)
  (Math/sqrt v_))

;;;--------------- Boolean ------------
;;; $boolean
(defn $boolean
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
  [arg]
  (cond (map? arg)     (if (empty? arg) false true)
        (vector? arg)  (if (some $boolean arg) true false)
        (string? arg)  (if (empty? arg) false true)
        (number? arg)  (if (zero? arg) false true)
        (fn? arg)      false
        (nil? arg)     false
        (boolean? arg) arg
        (keyword? arg) true)) ; query role.

;;; ToDo: I'll need more information than just the comment line here to understand what this
;;;       is suppose to do. It might require that I have better established how try/catch is handled.
;;; $exists
(defn $exists
  "Returns Boolean true if the arg expression evaluates to a value, or false if the expression
   does not match anything (e.g. a path to a non-existent field reference)."
  [arg]
  ($boolean arg))

;;; $not
(defn $not
  "Returns Boolean NOT on the argument. arg is first cast to a boolean."
  [arg]
  (-> arg $boolean not))

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

;;; $distinct
(defn $distinct
  "Returns an array containing all the values from the array parameter, but with any duplicates removed.
   Values are tested for deep equality as if by using the equality operator."
  ([] ($distinct @$))
  ([arr]
   (s/assert ::vector arr)
   (distinct arr)))

;;; $reverse
(defn$ $reverse
  "Returns an array containing all the values from the array parameter, but in reverse order."
  [arr_]
  (s/assert ::vector arr_)
  (-> arr_ reverse vec))

;;; $shuffle
(defn $shuffle
  "Returns an array containing all the values from the array parameter, but shuffled into random order."
  [arr]
  (s/assert ::vector arr)
  (loop [res []
         size (count arr)
         rem arr]
    (if (empty? rem) res
        (let [ix (rand-int size)]
          (recur (conj res (nth rem ix))
                 (dec size)
                 (let [[f b] (split-at ix rem)]
                   (into f (rest b))))))))

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
  ([arr] ($sort arr compare))
  ([arr fun]
   (s/assert ::vector arr)
   (s/assert ::fn fun)
   (vec (sort fun arr))))

;;; $zip
(defn $zip
  "Returns a convolved (zipped) array containing grouped arrays of values from the array1 ... arrayN
   arguments from index 0, 1, 2, etc.
   This function accepts a variable number of arguments. The length of the returned array is equal to
   the length of the shortest array in the arguments."
  [& arrays]
  (s/assert ::vectors arrays)
  (let [size (->> arrays (map count) (apply min))]
    (loop [res []
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
(defn $each
  "Returns an array containing the values return by the function when applied to each key/value pair in the object.
   The function parameter will get invoked with two arguments:
   function(value, name)
   where the value parameter is the value of each name/value pair in the object and name is its name.
   The name parameter is optional."
  [obj func]
  (s/assert ::map obj)
  (s/assert ::fn func)
  (if (== 2 (-> func meta :bi/params count))
    (reduce-kv (fn [r  k v] (conj r (func v k))) [] obj)
    (reduce-kv (fn [r _k v] (conj r (func v  ))) [] obj)))

;;; $error
(defn $error
  "Deliberately throws an error with an optional message"
  [msg]
  (s/assert ::string msg)
  (throw (ex-info msg {})))

;;; $keys
(defn $keys
  "Returns an array containing the keys in the object.
   If the argument is an array of objects, then the array returned contains a
   de-duplicated list of all the keys in all of the objects."
  [obj] ; ToDo (use s/assert?)
  (cond (map? obj) (-> obj keys vec)
        (vector? obj) (->> obj (map keys) distinct)
        :else (throw (ex-info "The argument to $keys must be an object or array:" {:obj obj}))))

;;; $lookup
(defn $lookup
  "Returns the value associated with key in object.
   If the first argument is an array of objects, then all of the objects in the array are searched,
   and the values associated with all occurrences of key are returned."
  [obj k]
  (cond (map? obj) (get obj k)
        (vector? obj) (mapv #(get % k) obj)
        :else (throw (ex-info "The argument to $keys must be an object or array." {:arg obj}))))

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
(defn $spread
  "Splits an object containing key/value pairs into an array of objects,
   each of which has a single key/value pair from the input object.
   If the parameter is an array of objects, then the resultant array contains an object
   for every key/value pair in every object in the supplied array."
  [obj]
  (when-not (or (map? obj)
                (and (vector? obj) (every? map? obj)))
    (throw (ex-info "The argument to $spread must be an object or array of objects:" {:obj obj})))
  (if (map? obj)
                                (reduce-kv (fn [r k v] (conj r {k v})) [] obj)
      (reduce (fn [r o] (into r (reduce-kv (fn [r k v] (conj r {k v})) []  o)))
              []
              obj)))

;;; $type
(defn $type
  "Evaluates the type of value and returns one of the following strings:
   'null', 'number', 'string', 'boolean', 'array', 'object', 'function'.
   Returns (non-string) undefined when value is undefined."
  [arg]
  (cond (nil? arg) "null"
        (number? arg) "number"
        (string? arg) "string"
        (boolean? arg) "boolean"
        (vector? arg) "array"
        (map? arg) "object"
        (fn? arg) "function"))

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
         (loop [str pic
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
                (loop [c coll_, i 0, r []]
                  (if (empty? c)
                    r
                    (let [val (when (func (first c) i coll_) (first c))]
                      (recur (rest c), (inc i), (if val (conj r val) r)))))
                (== nvars 2)
                (loop [c coll_, i 0, r []]
                  (if (empty? c)
                    r
                    (let [val (when (func (first c) i) (first c))]
                      (recur (rest c), (inc i) (conj (if val (conj r val) r))))))
                (== nvars 1)
                (loop [c coll_, r []]
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
      (loop [c coll, i 0, r []]
        (if (empty? c)
            r
            (recur (rest c) (inc i) (conj r (func (first c) i coll)))))
      (== nvars 2)
      (loop [c coll, i 0, r []]
        (if (empty? c)
          r
          (recur (rest c) (inc i) (conj r (func (first c) i)))))
      (== nvars 1)
      (loop [c coll, r []]
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
    (loop [c (rest coll),
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
(defn $single
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
  [coll func]
  (let [nvars  (-> func meta :bi/params count)]
    (cond
      (== nvars 3)
      (loop [c coll, i 0, r false]
        (cond r r
              (empty? c) false
              :else (recur (rest c) (inc i) (func (first c) i coll))))
      (== nvars 2)
      (loop [c coll, i 0, r false]
        (cond r r
              (empty? c) false
              :else (recur (rest c) (inc i) (func (first c) i))))
      (== nvars 1)
      (loop [c coll, r false]
        (cond r r
              (empty? c) false
              :else (recur (rest c) (func (first c))))),
      :else (throw (ex-info "$single expects a function of 1 to 3 parameters:"
                            {:params (-> func meta :bi/params)})))))

;;;==========================================================================
;;;=================== Non-JSONata functions ================================
#?(:clj
(defn $read
  "Read a file of JSON or XML, creating a map."
  ([fname] ($read fname {})) ; For Javascript-style optional params; see https://tinyurl.com/3sdwysjs
  ([fname opts]
   (let [type (-> (re-matches #"^.*\.([a-z,A-Z,0-9]{1,5})$" fname) second)]
     (-> (case (or (get opts "type") type "xml")
           #?(:clj "json") #?(:clj (-> fname slurp json/read-str))
           "xml"  (-> fname util/read-xml :xml/content first :xml/content util/simplify-xml)
           "edn"  (-> fname slurp util/read-str util/json-like))
         set-context!)))))

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

;;; ToDo: make this part of $read.
;;; $readSpreadsheet
#?(:clj
(defn $readSpreadsheet
  "Read the .xlsx and make a clojure map for each row. No fancy names, just :A,:B,:C,...!"
  ([filename sheet-name] ($readSpreadsheet filename sheet-name false))
  ([filename sheet-name invert?]
   (reset! $$ (when-let [sheet (->> (ss/load-workbook filename) (ss/select-sheet sheet-name))]
               (let [row1 (mapv ss/read-cell (-> sheet ss/row-seq first ss/cell-seq ss/into-seq))
                     len  (loop [n (dec (-> sheet ss/row-seq first .getLastCellNum))]
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
#_(defn unkeywordize-bsets
  "Data pushed into the DB had to have keywords for map keys. This undoes that."
  [bsets]
  (mapv #(reduce-kv (fn [m k v] (if (keyword? v)
                                  (if (namespace v)
                                    (assoc m k (str (namespace v) "/" (name v)))
                                    (assoc m k (name v)))
                                  (assoc m k v)))
                    {} %)
        bsets))

;;; ToDo: Did I make a mistake in the User's Guide by calling the entire returned value of query (a vector) a binding set?
;;;       According to this code, I should have called it a "collection of bsets".
(defn query-fn-aux
  "The function that returns a vector binding sets.
   Note that it attaches meta for the DB and body."
  [db-atms body in pred-args param-subs options]
  (let [dbs (mapv deref db-atms)
        e-qvar? (entity-qvars body)
        qform (final-query-form body in param-subs)]
    (as-> (apply d/q qform (into dbs pred-args)) ?bsets ; This is possible because body is data to d/q.
      ;; Remove binding sets that involve a schema entity.
      (remove (fn [bset] (some (fn [bval]
                                 (and (keyword? bval)
                                      (= "db" (namespace bval))))
                               (vals bset))) ?bsets)
      (cond->> ?bsets
        (-> options :keepDBid not) (map (fn [bset] bset
                                          (reduce-kv (fn [m k v]
                                                       (if (e-qvar? k) m (assoc m k v)))
                                                     {}
                                                     bset)))
        #_#_false                      unkeywordize-bsets
        true                       (vec))
      (with-meta ?bsets {:bi/b-set? true}))))

(def diag (atom nil))

(defn immediate-query-fn
  "Return a function that can be used immediately to make the query defined in body."
  [body in pred-args options]
  (fn [& data|dbs]
    (let [db-atms (map #(if (util/db-atm? %) % (-> % keywordize-keys qu/db-for!)) data|dbs)]
      (query-fn-aux db-atms body in pred-args {} options))))

(defn higher-order-query-fn
  "Return a function that can be called with parameters to return a function to m
   the parameterizes query defined by body and params. (It's just a closure...)"
  [body in pred-args options params]
  (-> (fn [& args]
        (let [param-subs (zipmap params args)] ; the closure.
          (-> (fn [& data|dbs]
                (let [db-atms (map #(if (util/db-atm? %) % (-> % keywordize-keys qu/db-for!)) data|dbs)]
                  (swap! diag #(-> % (assoc :body body) (assoc :pred-args pred-args) (assoc :param-subs param-subs)))
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

  ( $data := $newContext() ~> $addSource($read('data/testing/owl-example.edn'));
    $q := query($type){[?class :rdf/type            $type]
                       [?class :resource/iri        ?class-iri]
                       [?class :resource/namespace  ?class-ns]
                       [?class :resource/name       ?class-name]};
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
;;;      (4) the avoidance of constant keys in the DB and :_rm/constant-parent (RENAME IT LIKE THIS???)
(declare evaluate-express-body express-sub)

(defn express
  "Return an function that either
    (1) has no template variable and can be used directly with a binding set, or
    (2) has template variables and returns a parameterized version of (1) that
        can be called with parameter values to get the a function like (1) to
        be used with binding sets.
     The function returned has meta {express? true} so that, for example, $reduce
     knows that there should be a database involved."
  [& {:keys [params options base-body reduce-body schema key-order]}]
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
          (with-meta {:bi/express-template? true}))))


(defn box-vals
  "Walk through the form replacing non-map :_rm/val and :_rm/ek-val with boxed data."
  [obj]
  (cond (map? obj)       (reduce-kv (fn [m k v]
                                      (if (and (#{:_rm/val :_rm/ek-val} k)
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
    (letfn [(sub-bset [key-parts bset] ; Generate a value for a :rm/express-key expression.
              (if *in-reduce?*
                (->> (map #(if (qvar? %) (get bset %) %) key-parts) ; There can be constants in catkey.
                     (map str)
                     (interpose "|")
                     (apply str))
                (get bset (first key-parts))))
            (eeb-aux [obj]
              (cond (map? obj)         (reduce-kv (fn [m k v]
                                                    (if (= :_rm/user-key k)
                                                      (assoc m k (-> (if (qvar? v) (get bset v) v) box))
                                                      (assoc m (eeb-aux k) (eeb-aux v)))) {} obj)
                    (qu/exp-key? obj)  (sub-bset (rest obj) bset) ; rest: strip off :rm/express-key.
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

;;; "redex" is reduce on express body.
(defn redex-keys-values
  "Rewrite the :_rm/ROOT object retrieved from the database so that it matches the shape that
   was (1) encoded in the schema, and (2) built-up through objects using the reduce body.
   Since all the work was done in those two tasks, what remains here is just to make ordinary
   maps, vectors, and values from the retrieved content. There are three kinds of maps to deal with:
     - The ones with express keys (have :_rm/ek-val)  get their express key processed first and then their :_rm/attrs.
     - The ones with just :_rm/attrs are similar but the value of the :_rm/user-key is the reduced :_rm/attrs.
     - The ones without :_rm/attrs are maps of just one key."
  [data]
  ;(swap! diag #(assoc % :redex-data data))
  (letfn [(rkv [obj]
            (cond  (map? obj) ; 1st cond is any map, but :_rm/attrs may be missing because empty (just map with express key).
                   (cond (contains? obj :_rm/ek-val) ; express key
                         (reduce (fn [m attr] (merge m (rkv attr)))
                                 (-> {(:_rm/user-key obj) (:_rm/ek-val obj)}
                                     (assoc :_rm/ek-val   (:_rm/ek-val obj))) ; For subsequent sorting
                                 (:_rm/attrs obj)) ; Could be empty; then just express key and :_rm/ek-val

                         (contains? obj :_rm/attrs) ; ordinary or qvar key. Each attr defines one key and value.
                         {(:_rm/user-key obj) (reduce (fn [m attr]
                                                        (let [[k v] (-> attr rkv seq first vec)] ; It is (and map? count==1)
                                                          (assoc m k v)))
                                                      {}
                                                      (:_rm/attrs obj))}
                         ;; leaf attrs of a map (no attrs themselves)
                         (contains? obj :_rm/vals) {(:_rm/user-key obj) (-> obj :_rm/vals rkv)}
                         (contains? obj :_rm/val ) {(:_rm/user-key obj) (-> obj :_rm/val  rkv)})
                   (vector? obj) (mapv rkv obj)
                   :else         obj))]
    (if (== 1 (count data))
      (-> data first rkv)
      ;; This in the case that the body has a qvar-in-key-pos at the top level.
      (->> data (map rkv) (apply merge)))))

;;; ToDo: Do sort-by-body when *in-reduce* = false. Where to put it might not be straightforward!
(defn sort-by-body
  "Walk through redex-keys-values-processed output sorting
     (a) its vectors of express-keyed  maps by by their express-keys (_:rm/ek-val) and
     (b) its maps by their keys." ; If nothing else, this make testing easier!
  [data key-order]
  (let [known-key? (set key-order)]
    (letfn [(compar [k1 k2]
              (if (and (known-key? k1) (known-key? k2))
                (< (.indexOf key-order k1) (.indexOf key-order k2))
                (compare (str k1) (str k2)))) ; ToDo: See notes 2023-01-09 about symbols as keys.
            (ek-vec? [obj]
              (and (vector? obj)
                   (every? #(contains? % :_rm/ek-val) obj)))
            (sbek [obj]
              (cond (ek-vec? obj)     (->> (sort-by :_rm/ek-val obj) (mapv #(dissoc % :_rm/ek-val)) sbek),
                    (vector? obj)     (mapv sbek obj),
                    (map? obj)        (->> (reduce-kv (fn [m k v] (assoc m k (sbek v))) {} obj)
                                           (reduce-kv (fn [m k v] (if (= k :_rm/ek-val) m (assoc m k v))) {})
                                           (into (sorted-map-by compar))),
                    :else             obj))]
      (sbek data))))

(defn redex-restore-values
  "Restoring values might sound like a paleocon objective, but here we are referring to user data
   that was originally not string-valued yet is forced into being a string because it needed to
   serve as :_rm/user-key and is qvar-in-key-pos.
   :_rm/user-key requires :db.type/string and such data, serving as :db.unique/identity can't be boxed.
   util/read-str, (that is  ?(:clj read-string, :cljs cljs.reader/read-string) is used to
   restore the value of :_rm/user-key."
  [data schema]
  (let [restore-attr? (reduce-kv (fn [res k v]
                                   (if (contains? v :_rm/original-key-type) ; Schema entry has this only if qvar-in-key-pos...
                                     (conj res k)                           ; ...and bsets indicate that data isn't :db.valueType/string.
                                     res))
                                 #{} schema)]
    (letfn [(rrv [obj]
              (cond (map? obj)     (if (and (some #(contains? obj %) restore-attr?)
                                            (contains? obj :_rm/user-key))
                                     (-> (reduce-kv (fn [m k v] (assoc m k (rrv v))) {} obj)
                                         (update :_rm/user-key util/read-str))
                                     (reduce-kv (fn [m k v] (assoc m k (rrv v))) {} obj))
                    (vector? obj)  (mapv rrv obj)
                    :else          obj))]
      (rrv data))))

(defn redex-data-cleanup
  "Return the evaluated express body in the form expected of output from $reduce on
   and express body. This entails:
     1) user express body keys replacing :_rm 'domain' keys,
     2) maps, vectors, and values replacing :_rm/attrs :_rm/vals and :_rm/val respectively,
     3) boxed values unboxed,
     4) and (by default), things sorted nicely.
   schema argument should be 'full-schema' (containing :_rm entries).
   key-order argument is the order of keys found in the base body."
  [data key-order schema]
  (-> data
      unbox
      (redex-restore-values schema)
      redex-keys-values
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
              (cond (map? obj)    (doseq [[k v] (seq obj)] ; v should already be a string. ToDo: It isnt!
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
   (2) Ensure that :_rm/user-key is a string. If necessary, this will be undone later."
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
   In that case the qvar's value in the map returned is set to :_rm/must-be-boxed." ; _rm/must-be-boxed is not yet used.
  [bsets]
  (as-> (reduce-kv (fn [m k v] (assoc m k (du/db-type-of v))) {} (first bsets)) ?types
    (reduce (fn [res qvar]
              (if (every? (fn [val] (= (get ?types qvar) (du/db-type-of val)))
                          (map #(get % qvar) (rest bsets)))
                res
                (assoc res qvar :_rm/must-be-boxed)))
            ?types
            (keys ?types))))

(defn update-schema-for-bsets
  "The schema necessarily uses strings for :_rm/user-key.
   However, it the user's data might use something else (particularly numbers).
   Here we check whether bset qvars that are also :_rm/user-key qvar are in fact strings.
   Where they are not (e.g. when they are numbers), we add :_rm/orignal-key-type to the schema entry."
  [schema bsets]
  (let [bset-type (bset-db-types bsets)]
    (reduce-kv (fn [m k v]
                 (let [v-type (bset-type (:_rm/user-key v))]
                   (if (and (qvar? (:_rm/user-key v))           ; :_rm/user-key is used in two ways. If the schema indicates an express key...
                            (not   (= :db.type/string v-type))  ; ...then just use the qvar as-is. Otherwise, it is qvar-in-key-pos...
                            (not   (:_rm/exp-key? v)))          ; ...and the original key type must be restored by read-string.
                     (assoc m k (assoc v :_rm/original-key-type v-type))
                     (assoc m k v))))
               {}
               schema)))

(defn reduce-express
  "This function performs $reduce on an express function; *in-reduce?* = true.
   Prior to this call, query/schematic-express-body defined metadata (:bi/reduce-body and :bi/schema).
   The data is built-up according to this schema and the b-sets. The data is set to the value of :_rm/ROOT.
   Lookup-refs are used, and the data is pushed into a database. The ROOT is pulled back, and cleaned up.
   The result is the value of the the express body reduced by the data."
  ([b-sets efn] (reduce-express b-sets efn {}))
  ([b-sets efn _init] ; ToDo: Don't know what to do with the init!
   (let [full-schema (-> efn meta :bi/schema (update-schema-for-bsets b-sets))
         base-data   (->> (mapv efn b-sets) walk/keywordize-keys)
         lookup-refs (create-lookup-refs full-schema base-data (util/cljs?))
         data        (data-with-lookups full-schema base-data)
         db-schema   (qu/schema-for-db full-schema (if (util/cljs?) :datascript :datahike))
         zippy       (reset! diag {:schema      full-schema
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
       (doseq [obj data] (d/transact db-atm [{:_rm/ROOT obj}]))
       (doseq [obj data] (d/transact db-atm {:tx-data [{:_rm/ROOT obj}]}))) ; This one good for debugging.
     ;; ToDo: There can also be multiple roots in other situations. There is duplicate data!
     ;;  {:db/id 16, :_rm/ROOT [#:db{:id 13}]}
     ;;  {:db/id 18, :_rm/ROOT [#:db{:id 13}]}
     ;; This implies one extra, useless entry per b-set. Can it be avoided? Maybe not;
     ;; What the data indicates is that "more than one thing can have a :_rm/ROOT". Hard to argue with that!
     (let [root-eids (d/q '[:find [?ref ...] :where [?top :_rm/ROOT ?ref]] @db-atm)
           pre-clean (mapv #(du/resolve-db-id {:db/id %} db-atm #{:db/id}) root-eids)]
       (swap! diag #(-> % (assoc :root-eids root-eids) (assoc :pre-clean pre-clean)))
       (redex-data-cleanup pre-clean (-> efn meta :bi/key-order) full-schema)))))

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
          :else (throw (ex-info "Second argument to $mapObject shoud be a function of 2 or 3 arguments."
                                {:arg-cnt arg-cnt})))))

(defn $reduceKV
  "Reduce INIT by calling FUN with each key/value pair of OBJ."
  [fn init coll]
  (reduce-kv fn init coll))

(defn $assoc
  [obj k v]
  (assoc obj k v))
