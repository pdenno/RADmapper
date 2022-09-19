(ns rad-mapper.builtins
  "Built-in functions implementing the expression language of the mapping language.
   Functions with names beginning with a '$' are available to the user (e.g. $filter).
   Others (such as bi/key and bi/strcat) implement other parts of the expression language
   but are not available directly to the user except through the operators (e.g. the dot, and &
   respectively for navigation to a property and concatenation)."
  (:refer-clojure :exclude [+ - * / < > <= >= =]) ; So be careful!
  (:require
   [cemerick.url                 :as url]
   [clojure.core                 :as core]
   #?(:clj [clojure.data.json         :as json])
   #?(:clj [clojure.data.codec.base64 :as b64])
   [clojure.spec.alpha           :as s]
   [clojure.pprint               :refer [cl-format]]
   [clojure.string               :as str :refer [index-of]]
   [clojure.walk                 :as walk :refer [keywordize-keys]]
   #?(:clj  [dk.ative.docjure.spreadsheet :as ss])
   #?(:clj  [datahike.api                 :as d]
      :cljs [datascript.core              :as d])
   [failjure.core                :as fj]
   [rad-mapper.query             :as qu]
   [rad-mapper.util              :as util]
   [taoensso.timbre              :as log])
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
              java.time.ZoneOffset))
  #?(:cljs (:require-macros [rad-mapper.builtins :refer [defn* defn$ thread value-step primary init-step map-step]])))

;;; ToDo:
;;;  * Consider Small Clojure Interpreter (SCI) for Clojure version.
;;;  * Make sure meta isn't used where a closure would work.

(def ^:dynamic $  "JSONata context variable" (atom nil))
(def           $$ "JSONata root context."    (atom :bi/unset))

(declare aref)

(defmacro with-context
  "Dynamically bind $ to the value provided."
  [val & body]
  `(binding [$ (atom ~val)] ~@body))

(defn set-context!
  "Set the JSONata context variable to the argument. If the root context has not
   yet been set, set that too."
  [val]
  (reset! $ val)
  (when (core/= @$$ :bi/unset) (reset! $$ val))
  val)

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
(s/def ::radix (s/and number? #(core/<= 2 % 36)))
(s/def ::fn fn?)
(s/def ::sbind #(-> % meta :sbind?))

(defn handle-builtin
  "Generic handling of errors for built-ins"
  [e]
  (log/error (:message e))
  e)

(defn reset-env
  "Clean things up just prior to running user code."
  ([] (reset-env nil))
  ([context]
   (set-context! context)
   (reset! $$ :bi/unset)))

(defn flatten-except-json
  "Adapted from core/flatten:
   Takes any nested combination of sequential things (lists, vectors, etc.)
   and returns their contents as a single, flat lazy sequence.
   (flatten nil) returns an empty sequence.
   EXCEPTION: If the thing is a vector with metadata :type :bi/json-array, it isn't flattened. "
  [x]
  (let [m (meta x)]
    (letfn [(seq-except? [o]
              (and (sequential? o)
                   (not (-> o meta :bi/json-array?))))]
      (-> (remove seq-except? (tree-seq seq-except? seq x))
          vec
          (with-meta m)))))

(defn container?   [obj] (-> obj meta :bi/container?))
(defn containerize [obj] (-> obj (with-meta (merge (meta obj) {:bi/container? true}))))
(defn containerize?
  "If obj is a vector, set  metadata :bi/container?."
  [obj]
  (if (vector? obj)
    (-> obj (with-meta (merge (meta obj) {:bi/container? true})))
    obj))

(defn cmap
  "If the object isn't a container, run the function on it,
   otherwise, mapv over the argument and containerize the result."
  [f arg]
  (if (container? arg)
     (->> (mapv f arg) containerize)
    (f arg)))

(defn jflatten
  "Accommodate JSONata's quirky equivalence in behavior of scalars and arrays containing one object.
   Note that this is only applied for results of mapping (called 'containers'), not ordinary access.
   - ordinary access            : {'nums'   : [[1], 2, 3]}.nums[0]
   - container (because using $): [[1, 2, 3] 4].$

   See http://docs.jsonata.org/processing section 'Sequences', which currenlty reads as follows:
       The sequence flattening rules are as follows:

    1) An empty sequence is a sequence with no values and is considered to be 'nothing' or 'no match'.
       It won't appear in the output of any expression.
       If it is associated with an object property (key/value) pair in a result object, then that object will not have that property.

    2) A singleton sequence is a sequence containing a single value.
       It is considered equivalent to that value itself, and the output from any expression, or sub-expression will be
       that value without any surrounding structure.

    3) A sequence containing more than one value is represented in the output as a JSON array.
       This is still internally flagged as a sequence and subject to the next rule.
       Note that if an expression matches an array from the input JSON, or a JSON array is explicitly constructed in
       the query using the array constructor, then this remains an array of values rather than a sequence of values and
       will not be subject to the sequence flattening rules.
       However, if this array becomes the context of a subsequent expression, then the result of that will be a sequence.

    4) If a sequence contains one or more (sub-)sequences, then the values from the sub-sequence are pulled up to
       the level of the outer sequence. A result sequence will never contain child sequences (they are flattened)."
  [obj]
  (letfn [(elim-empty [o] ; rule-1
            (let [m (meta o)]
              (cond (map? o) (-> (reduce-kv
                                  (fn [m k v]
                                    (if (or (nil? v) (and (coll? v) (empty? v))) m (assoc m k (elim-empty v))))
                                  {}
                                  o)
                                 (with-meta m)),
                    (vector? o) (-> (->> o (remove nil?) (mapv elim-empty)) (with-meta m))
                    :else o)))]
    (cond (container? obj)
          (let [len (count obj)]
            (cond (== 0 len) nil ; Or should I call it ::no-match ? Rule 1
                  (== 1 len) (-> obj first elim-empty) ; (-> Rule 2, Rule 1)
                  :else (-> (elim-empty obj) flatten-except-json containerize))) ; Rule 1, Rule 3 JSON array.

          (vector? obj) (let [m   (meta obj)
                              obj (-> (->> obj (remove nil?) vec) (with-meta m))
                              len (count obj)]
                          (cond (== 0 len) nil ; Or should I call it ::no-match ? Rule 1
                                ;;(== 1 len) (first obj)
                                :else obj))
          :else obj)))

(defn singlize [v] (if (vector? v) v (vector v)))

;;; It exists so that rew/wrap-non-path won't wrap it
(defn deref$
  "Expressions such as [[1,2,3], [1]].$ will translate to (bi/run-steps [[1 2 3] [1]] (deref bi/$))
   making it advantageous to have a deref that sets the value and returns it."
  ([] (containerize? @$))
  ([val] ; ToDo: Is this one ever used?
   (set-context! (containerize? val))))

(defmacro defn*
  "Convenience macro for numerical operators. They can be passed functions."
  [fn-name doc-string [& args] & body]
  `(def ~fn-name
     ~doc-string
     (fn [~@args]
       (let [~@(mapcat #(list % `(-> (if (fn? ~%) (~%) ~%) jflatten)) args)]
         ~@(map #(list 'clojure.spec.alpha/assert ::number %) args)
         ~@body))))

(defmacro defn$
  "Define two function arities using the body:
     (1) the ordinary one, that has the usual arguments for the built-in, and,
     (2) a function where the missing argument will be assumed to be the context variable, $.
   The parameter ending in a \\_ is the one elided in (2). (There must be such a parameter.)
   doc-string is required."
  [fn-name doc-string [& params] & body]
  (let [param-map (zipmap params (map #(symbol nil (name %)) params))
        abbrv-params (vec (remove #(str/ends-with? (str %) "_") (vals param-map)))
        abbrv-args (mapv #(if (str/ends-with? (str %) "_") '@$ %) (vals param-map))
        fn-name (symbol nil (name fn-name))]
    (letfn [(rewrite [x]
              (cond (seq? x)    (map  rewrite x)
                    (vector? x) (mapv rewrite x)
                    (map? x)    (reduce-kv (fn [m k v] (assoc m (rewrite k) (rewrite v))) {} x)
                    :else (get param-map x x)))]
    `(defn ~fn-name ~doc-string
       (~abbrv-params (~fn-name ~@abbrv-args))
       ([~@(vals param-map)]
        (let [res# (do ~@(rewrite body))] (if (seq? res#) (doall res#) res#)))))))

;;;========================= JSONata built-ins  =========================================
(defn* +  "plus"   [x y]   (core/+ x y))
(defn* -  "minus"  [x y]   (core/- x y))
(defn* *  "times"  [x y]   (core/* x y))
(defn* /  "divide" [x y]   (s/assert ::non-zero y) (double (core// x y)))
(defn* >  "greater-than"          [x y] (core/>  x y))
(defn* <  "less-than "            [x y] (core/<  x y))
(defn* >= "greater-than-or-equal" [x y] (core/>= x y))
(defn* <= "less-than-or-equal"    [x y] (core/<= x y))

(defn =
  "equal, need not be numbers"
  [x y]
  (core/= (jflatten x) (jflatten y)))

(defn !=
  "not equal, need not be numbers"
  [x y]
  (core/= (jflatten x) (jflatten y)))

(defmacro thread
  "The function chaining operator is used in the situations where multiple nested functions need to
   be applied to a value, while making it easy to read. The value on the LHS is evaluated,
   then passed into the function on the RHS as its first argument. If the function has any other arguments,
   then these are passed to the function in parenthesis as usual. It is an error if the RHS is not a
   function, or an expression that evaluates to a function."
  [x y]
  `(let [xarg# ~x]
     (do (set-context! xarg#)
         (let [yarg# ~y]
           (if (fn? yarg#)
             (yarg# xarg#)
             (throw (ex-info "The RHS argument to the threading operator is not a function."
                             {:rhs-operator yarg#})))))))

;;; ------------------ Path implementation ---------------------------------
(defn run-steps
  "Run or map over each path step function, passing the result to the next step."
  [& steps]
  (binding [$ (atom @$)] ; Make a new temporary context that can be reset in the steps.
    (let [init-step? (core/= :bi/init-step (-> steps first meta :bi/step-type))]
      (loop [res   (if init-step? (set-context! (-> ((first steps) @$) containerize?)) @$)
             steps (if init-step? (rest steps) steps)]
      (cond (empty? steps) res

            ;; bi/get-step followed by bi/filter-step ("non-compositional" ???)
            (and (core/= :bi/get-step (-> steps first meta :bi/step-type))
                 (core/= :bi/filter-step (-> steps second meta :bi/step-type))) ; Then don't do next step. Steal its argument.
            (let [sfn (second steps)              ; Run the filter specifying the previous step keyword.
                  new-res (set-context! (sfn res {:bi/call-type :bi/get-step
                                                  :bi/attr (-> steps first meta :bi/arg)}))]
              (recur new-res (-> steps rest rest))),

            :else ; All other are "compositional".
            (let [sfn (first steps)
                  new-res (set-context!
                           (case (-> sfn meta :bi/step-type)
                             :bi/filter-step (sfn res nil) ; containerizes arg; will do (-> (cmap aref) jflatten) | filterv
                             :bi/get-step    (sfn res nil) ; containerizes arg if not map; will do cmap or map get.
                             :bi/value-step  (sfn res)     ; When res is map it containerizes.
                             :bi/primary     (if (vector? res) (cmap sfn (containerize? res)) (sfn res))
                             :bi/map-step    (cmap #(binding [$ (atom %)] (sfn %)) (containerize? res))
                             (fj/fail "Huh? Meta = %s" (-> sfn meta :bi/step-type))))]
              (recur new-res (rest steps))))))))

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
        (let [prix (fj/try* (pred|ix-fn @$))
              call-type (:bi/call-type prior-step)
              ob (if (core/= :bi/get-step call-type)
                   (let [k (:bi/attr prior-step)] ; non-compositional semantics
                     (if (vector? obj)
                       (cmap #(get % k) (containerize obj))
                       (get obj k)))
                   obj)]
          (if (number? prix)   ; Array behavior. Caller will map over it.
            (let [ix (-> prix Math/floor int)] ; Really! I checked!
              (if (-> ob meta :bi/json-array?)
                (aref ob ix)
                (-> (cmap #(aref % ix) ob) jflatten)))
            (as-> ob ?o          ; Filter behavior.
              (singlize ?o)
              (-> (filterv pred|ix-fn ?o) containerize?)))))
      (with-meta {:bi/step-type :bi/filter-step})))

(defn get-step
  "Perform the mapping activity of the 'a' in $.a, for example.
   This function is called with the state object.
   It returns the state object with :sys/$ updated."
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

(defmacro value-step
  "Return a function that evaluates what is in the the []  'hello' in [1,2,3].['hello']
   and the truth values [[false] [true] [true]] of [1,2,3].[$ = 2]."
  [body]
  `(->
     (fn ~'value-step [x#]
       (let [func# (fn [y#] (binding [$ (atom y#)] ~body))]
         (if (vector? x#)
           (->> (mapv func# x#) (mapv vector))
           (vector (func# x#)))))
     (with-meta {:bi/step-type :bi/value-step})))

(defn get-scoped
  "Access map key like clj/get, but with arity overloading for $."
  ([k] (get-scoped @$ k))
  ([obj k] (get obj k)))

(defmacro primary
  "Return a function with meta {:bi/step-type :bi/primary} that optionally takes
   the context atom and runs the body."
  [body]
  `(-> (fn ~'primary [& arg#] (binding [bi/$ (if (empty? arg#) bi/$ (-> arg# first atom))] ~body))
       (with-meta {:bi/step-type :bi/primary})))

(defmacro init-step
  "All the arguments of bi/run-steps are functions. This one just runs the argument body,
   which might construct a literal value, be a literal value, or call a function."
  [body]
  `(-> (fn [_x#] ~body)
       (with-meta {:bi/step-type :bi/init-step})))

(defmacro map-step
  "All the arguments of bi/run-steps are functions. This one maps $ over the argument body."
  [body]
  `(-> (fn [_x#] ~body)
       (with-meta {:bi/step-type :bi/map-step})))

(defn aref
  "Negative indexes count from the end of the array, for example, arr[-1] will select the last value,
   arr[-2] the second to last, etc.
   If an index is specified that exceeds the size of the array, then nothing is selected."
  [obj ix]
  (let [len (if (vector? obj) (count obj) 1)
        ix  (if (neg? ix) (core/+ len ix) ix)]
    (if (or (and (pos? ix) (core/>= ix len))
            (and (neg? ix) (core/> (Math/abs ix) len)))
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
                        (seq? form) (cond (and (core/= (first form) 'bi/key) (== 2 (count form)))
                                          (list 'bi/key sym (second form))
                                          (core/= form '(core/deref rad-mapper.builtins/$)) sym ; 2022-05-29, added.
                                          :else (map sba-aux form))
                        :else form)
                  (with-meta m))))]
    (sba-aux form)))

;;;--------------------------- JSONata built-in functions ------------------------------------

;;;------------- String --------------
;;; $base64decode
#?(:clj
(defn $base64decode
  "Converts base 64 encoded bytes to a string, using a UTF-8 Unicode codepage."
  [c]
  (-> c .getBytes b64/decode String.)))

;;; $base64encode
#?(:clj
(defn $base64encode
  "Converts an ASCII string to a base 64 representation.
   Each each character in the string is treated as a byte of binary data.
   This requires that all characters in the string are in the 0x00 to 0xFF range,
   which includes all characters in URI encoded strings.
   Unicode characters outside of that range are not supported."
  [s]
  (-> s .getBytes b64/encode String.)))

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
#?(:clj ; ToDo: CLJS doesn't have ns-resolve.
(defn $eval
  ([s] ($eval s @$))
  ([s context]
   (s/assert ::string s)
   (let [rewrite (ns-resolve 'rad-mapper.rewrite 'rewrite*)
         form (rewrite :ptag/exp s :rewrite? true)]
     (binding [*ns* (find-ns 'user)]
       (fj/try*
         (reset-env context)
         (let [res (eval form)]
           (if (and (fn? res) (core/= :bi/primary (-> res meta :bi/step-type)))
             (jflatten (res))
             (jflatten res)))))))))

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

;;; $match
#?(
:cljs
(defn $match [& args] false) ; ToDo: CLJS doesn't have re-matcher.
:clj
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
                          (let [ix (core/+ adv (or (index-of (subs s adv) match) 0))]
                            (recur
                             (conj res (-> {"match" match, "index" ix, "groups" (vec groups)}
                                           (with-meta {:bi/regex-result? true})))
                             (core/+ adv (count match))
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
              "end"   (core/+ ix (count match))
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
       (let [extra (->> (repeat (core/- awidth len) char) (apply str))]
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
   If it is a regex, its is used to find .

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
   The remainder of the input beyond this limit will be copied to the output unchanged."
  ([s pattern replacement] ($replace s pattern replacement :unlimited))
  ([s pattern replacement limit]
   (s/assert ::string s)
   (s/assert ::str|regex pattern)
   (s/assert ::pos-limit limit)
   (let [lim  (if (number? limit) (-> limit Math/floor int) limit)]
     (cond (string? replacement)
           (let [repl (str/replace replacement "$$" "\\$")]
             (reduce (fn [res _i] (str/replace-first res pattern repl))
                     s
                     (if (core/= :unlimited lim) (-> s count range) (range lim)))),
           (fn? replacement)
           (reduce (fn [res _i]
                      (try (let [repl (-> ($match res pattern) replacement)]
                            (str/replace-first res pattern repl))
                          ;; ToDo: Need a better way! See last test of $replace in builtins_test.clj
                          (catch #?(:clj Exception :cljs :default) _e res)))
                   s
                   (if (core/= :unlimited lim) (-> s count range) (range lim))), ; ToDo: (-> s count range) is a guess.
           :else (fj/fail "Replacement pattern must be a string or function: %s" replacement)))))

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
     (if (core/= lim :unlimited)
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
                (subs str (core/+ len start))
                (subs str start))]
      (if (or (core/= :unlimited length) (> length len))
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
  (core// (apply core/+ nums)
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
#?(:clj
(defn $formatNumber
  "Casts the number to a string and formats it to a decimal representation as specified by the picture string.

   The behaviour of this function is consistent with the XPath/XQuery function fn:format-number as defined in
   the XPath F&O 3.1 specification. The picture string parameter defines how the number is formatted and has
   the same syntax as fn:format-number.

   The optional third argument options is used to override the default locale specific formatting characters
   such as the decimal separator. If supplied, this argument must be an object containing name/value pairs
   specified in the decimal format section of the XPath F&O 3.1 specification."
  [number picture & options]
  (let [pic (str/replace picture "e" "E")
        opts (-> options keywordize-keys first)
        symbols (DecimalFormatSymbols.)]
    (.setExponentSeparator symbols "e")
    (doseq [[k v] opts]
      (case k ; ToDo: More of these.
        :zero-digit (.setZeroDigit symbols (nth v 0))
        :minus-sign (.setMinusSign symbols (nth v 0))
        nil))
    (let [df (DecimalFormat. pic symbols)]
      (doseq [[k _v] opts]
        (case k ; ToDo: More of these.
          :per-mille (.setMultiplier df 1000)
          nil))
    (.format df number)))))

;;; https://www.altova.com/xpath-xquery-reference/fn-format-integer

;;; $formatInteger
#?(:clj   ; ToDo: Calls $formatNumber, which is :clj-only so far.
(defn $formatInteger
  "Casts the number to a string and formats it to an integer representation as specified by the picture string.
   The behaviour of this function is consistent with the two-argument version of the XPath/XQuery function
   fn:format-integer as defined in the XPath F&O 3.1 specification. The picture string parameter defines how
   the number is formatted and has the same syntax as fn:format-integer."
  [num pic]
  (s/assert num ::number)
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
          :else ($formatNumber num pic)))))

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
                        (fj/fail "Cannot be cast to a number: %s" v_)))
        (boolean? v_) (if v_ 1 0)
        :else (fj/fail "Cannot be cast to a number: %s" v_)))

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
#?(:clj
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
   (let [[left _right] (-> num double str (str/split #"\."))
         rnum (BigDecimal.
               num
               (MathContext.
                (if (zero? precision)
                  (count left)
                  (core/+ (count left) precision))
                RoundingMode/HALF_EVEN))]
     (if (pos? precision) (double rnum) (long rnum))))))

;;; $sum
(defn $sum
  "Take one number of a vector of numbers; return the sum."
  [nums]
  (let [v (singlize nums)]
    (s/assert ::numbers v)
     (apply core/+ v)))

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
  (fj/fail msg))

;;; $keys
(defn $keys
  "Returns an array containing the keys in the object.
   If the argument is an array of objects, then the array returned contains a
   de-duplicated list of all the keys in all of the objects."
  [obj] ; ToDo (use s/assert?)
  (cond (map? obj) (-> obj keys vec)
        (vector? obj) (->> obj (map keys) distinct)
        :else (fj/fail "The argument to $keys must be an object or array: %s." obj)))

;;; $lookup
(defn $lookup
  "Returns the value associated with key in object.
   If the first argument is an array of objects, then all of the objects in the array are searched,
   and the values associated with all occurrences of key are returned."
  [obj k]
  (cond (map? obj) (get obj k)
        (vector? obj) (mapv #(get % k) obj)
        :else (fj/fail "The argument to $keys must be an object or array." :arg obj)))

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
           :else (fj/fail "The function provided to $sift must specify 1 to 3 arguments: nargs= %." nargs)))))

;;; $spread
(defn $spread
  "Splits an object containing key/value pairs into an array of objects,
   each of which has a single key/value pair from the input object.
   If the parameter is an array of objects, then the resultant array contains an object
   for every key/value pair in every object in the supplied array."
  [obj]
  (when-not (or (map? obj)
                (and (vector? obj) (every? map? obj)))
    (fj/fail "The argument to $spread must be an object or array of objects: %s." obj))
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
#?(:clj
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
  ([millis] ($fromMillis millis nil nil))
  ([millis pic] ($fromMillis millis pic nil))
  ([millis pic tzone]
   (let [zone-offset (if tzone (ZoneId/of tzone) ZoneOffset/UTC)
         java-tstamp (ZonedDateTime/ofInstant (Instant/ofEpochMilli (long millis)) zone-offset)]
     (format-time java-tstamp pic)))))

;;; ToDo: What does this mean?:
;;;   "All invocations of $millis() within an evaluation of an expression will all return the same timestamp value."
;;;    It says the same thing on $now().
;;; $millis
#?(:clj
(defn $millis
  "Returns the number of milliseconds since the Unix Epoch (1 January, 1970 UTC) as a number.
   All invocations of $millis() within an evaluation of an expression will all return the same value."
  []
  (.toEpochMilli (.toInstant (ZonedDateTime/now))))) ; Local doesn't work here.

;;; $now
#?(:clj
(defn $now
  "Generates a UTC timestamp in ISO 8601 compatible format and returns it as a string.
   All invocations of $now() within an evaluation of an expression will all return the same timestamp value.
   If the optional picture and timezone parameters are supplied, then the current timestamp is formatted
   as described by the $fromMillis() function."
  ([] (str (LocalDateTime/now))) ; Prints as e.g. "2022-08-09T14:01:25.849575"
  ([pic] ($now pic nil))
  ([pic tzone]
   (let [zone-offset (if tzone (ZoneId/of tzone) ZoneOffset/UTC)]
     (format-time (ZonedDateTime/now zone-offset) pic)))))

;;; $toMillis
#?(:clj
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
   (LocalDateTime/parse str)))) ; ZonedDateTime will not work.

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
                :else (fj/fail "$filter expects a function of 1 to 3 parameters: %s" (-> func meta :bi/params)))))

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
      :else (fj/fail "$map expects a function of 1 to 3 parameters:" (-> func meta :bi/params)))))

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
   (let [met (meta func)]
     (cond (:bi/express-template? met)                          (fj/fail "Reducing called on express template fn." ),
           (:bi/express? met)                                   (reduce-express coll func init),
           (not (core/<= 2 (-> met :bi/params count) 4))        (fj/fail "Reduce function must take 2 to 4 args.")
           (not init)                                           (reduce-typical coll func)
           :else                                                (reduce-typical (into (vector init) coll) func)))))

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
      :else (fj/fail "$single expects a function of 1 to 3 parameters: %s" (-> func meta :bi/params)))))

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
           "edn"  (-> fname slurp util/read-str qu/json-like))
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
                            (cond (core/= n 0) 0,
                                  (not (nth row1 n)) (recur (dec n)),
                                  :else (inc n)))
                     keys (map keyword (take len (util/string-permute)))
                     raw (ss/select-columns (zipmap keys keys) sheet)]
                 (if invert?
                   (transpose-sheet raw)
                   ;; ToDo This is all sort of silly. Can we access cells a better way?
                   (rewrite-sheet-for-mapper raw))))))))

;;;============================= Mapping Context, query, express ======================

;;; Thoughts on schema
;;;   - Learned schema are sufficient for source data (uses qu/db-for!)
;;;   - What is provided as argument overrides what is learned in $query.
;;;   - $express could be with an argument schema.
(defn $schemaFor
  "Study the argument data and heuristically suggest the types and multiplicity of data.
   Note that this function does not make a guess at what the keys (db/key) are."
  [data]
  (qu/learn-schema data))

;;;---------- query ---------------------------------------
(defn substitute-in-form
  "Return a map for a datalog queryy [:find...] form that substitutes values as
   though syntax quote were being used."
  [body param-val-map]
  (letfn [(tp-aux [x]
            (cond (vector? x)                  (mapv tp-aux x),
                  (seq? x)                     (map  tp-aux x),
                  (contains? param-val-map x)  (param-val-map x),
                  :else x))]
    (let [vars (->> (reduce (fn [r x] (into r x)) body)
                    (filter #(str/starts-with? % "?"))
                    distinct
                    vec)]
    `{:vars  ~vars
      :keys  ~(mapv symbol vars)
      :where ~(tp-aux body)})))

(defn rewrite-qform
  "Rewrite a query map such as returned by subsitute-in-form into a form acceptable as a datalog query.
   Accepts map keys :find :keys :in and :where."
  [qmap]
  `[:find ~@(:vars qmap)
    :keys ~@(:keys qmap)
    :in ~@(into '[$] (:in qmap)) ; ToDo: do we really want the :in like this?
    :where ~@(:where qmap)])

;;; ToDo: This isn't working.
#_(defn qvar? [obj]  (-> obj meta :qvar?))
(defn qvar? [obj]  (and (symbol? obj) (= "?" (-> obj str (subs 0 1)))))

(defn entity-qvars
  "Return the set of qvars in entity position of the argument body"
  [body]
  (->> body
       (filter #(and (== 3 (count %)) (-> % first qvar?)))
       (map first)
       (map keyword)
       set))

(defn query-fn-aux
  "The function that returns a binding set.
   Note that it attaches meta for the DB and body."
  [db-atm body param-subs]
  (let [e-qvar? (entity-qvars body)
        qmap (substitute-in-form body param-subs)]
    (-> (->> (d/q (-> qmap rewrite-qform) @db-atm) ; This is possible because body is data to d/q.
             ;; Remove binding sets that involve a schema entity.
             (remove (fn [bset] (some (fn [bval]
                                        (and (keyword? bval)
                                             (core/= "db" (namespace bval))))
                                      (vals bset))))
             ;; ToDo: Add an option to query keepDBids. This should be the default, however.
             (mapv (fn [bset] (reduce-kv (fn [m k v] (if (e-qvar? k) m (assoc m k v))) {} bset)))
             vec) ; ToDo: The commented metadata might become useful?
        (with-meta {:bi/b-set? true #_#_#_#_:bi/db-atm db-atm :bi/query-map qmap}))))

(defn immediate-query-fn
  "Return a function that can be used immediately to make the query defined in body."
  [body]
  (fn [data|db] ; ToDo: Could I make this use $ if no data supplied?
    (let [db-atm (if (util/db-atm? data|db)
                   data|db
                   (-> data|db keywordize-keys qu/db-for!))]
      (query-fn-aux db-atm body {}))))

(defn higher-order-query-fn
  "Return a function that can be called with parameters to return a function to m
   the parameterizes query defined by body and params. (It's just a closure...)"
  [body params]
  (fn [& args]
    (let [param-subs (zipmap params args)] ; the closure.
      (fn [data|db]
        (let [db-atm (if (util/db-atm? data|db)
                       data|db
                       (-> data|db keywordize-keys qu/db-for!))]
          (query-fn-aux db-atm body param-subs))))))

(defn query
  "There are two uses scenarios for query:
      (1) Calls to query where no parameters are specified return a function
          that takes data and returns binding sets.
      (2) Calls to query that provide parameters return a function that takes
          values for those parameters and return a function of type (1).

  'params' is an ordered vector parameters (jvars) that will be matched to 'args' used
   to parameterized the query form, thus producing a 'customized' query function.

   Example usage (of the second sort):

  ( $data := $newContext() ~> $addSource($read('data/testing/owl-example.edn'));
    $q := query($type){[?class :rdf/type            $type]
                       [?class :resource/iri        ?class-iri]
                       [?class :resource/namespace  ?class-ns]
                       [?class :resource/name       ?class-name]};
    $q($data,'owl/Class') )"
  [params body]
  (if (empty? params)
    (immediate-query-fn body)
    (higher-order-query-fn body params)))

;;;------------------ Express --------------------------------------------
(declare sbind-body body&bset-ai-maps express-sub)
(defn express
  "Return an function that either
    (1) has no template variable and can be used directly with a binding set, or
    (2) has template variables and returns a parameterized version of (1) that
        can be called with parameter values to get the a function like (1) to
        be used with binding sets.
     The function returned has meta {express? true} so that, for example, $reduce
     knows that there should be a database involved."
  [& {:keys [params options body]}]
  (if (empty? params)
    ;; The immediate function:
    (let [bbody (sbind-body body)]
      (-> (fn [bset]
            (let [assoc-in-map (apply merge {} (body&bset-ai-maps bbody bset))]
              (-> (reduce-kv (fn [m k v] (assoc-in m k v)) {} assoc-in-map)
                  (with-meta {:bi/ai-map assoc-in-map})))) ; used by $reduce
          (with-meta {:bi/params '[b-set], :bi/express? true, :bi/options options})))
    ;; body has template params that must be replaced; new-body.
    (-> (fn [& psubs]
          (let [new-body (express-sub body (zipmap params psubs))
                bbody (sbind-body new-body)]
            (-> (fn [bset]
                  (let [assoc-in-map (apply merge {} (body&bset-ai-maps bbody bset))]
                    (-> (reduce-kv (fn [m k v] (assoc-in m k v)) {} assoc-in-map)
                        (with-meta {:bi/ai-map assoc-in-map})))) ; used by $reduce
                (with-meta {:bi/params '[b-set], :bi/express? true, :bi/options options}))))
        (with-meta {:bi/express-template? true}))))

(defn express-sub
  "Walk through form replacing template parameters in sub-map with their values."
  [form sub-map]
  (letfn [(es-aux [x]
            (cond (map? x)    (reduce-kv (fn [m k v] (assoc m (es-aux k) (es-aux v))) {} x)
                  (vector? x) (mapv es-aux x)
                  (symbol? x) (or (get sub-map x) x)
                  :else x))]
    (es-aux form)))

(defn sbind? [obj] (-> obj meta :sbind?))

(defn sbind-body
  "Return a structure topologically identical to the argument body, but with sbinds replacing
   all the simple elements except map keys (which remain as strings or numbers).
   The sbind is a map with meta {:sbind true}. The map contains as least the key :pos, an ordinal
   indicating the position of the sbind in depth-first traveral. It may also include:
     - :sym, the qvar if the sbind represents one.
     - :constant, a string or number, if the sbind represents one.
     - :binds, having value :key if the original element was a map key; ; :val if the it was a map value."
  [body]
  (let [order (atom 0)]
    (letfn [(sbind [m] (with-meta m {:sbind? true}))
            (bb-aux [x]
              (cond (sbind? x) (assoc x :pos (swap! order inc)),
                    (map? x)   (reduce-kv (fn [m k v]
                                           (assoc m
                                                  (if (qvar? k) (-> {:sym k :binds :key} sbind bb-aux)
                                                      (bb-aux k))
                                                  (cond (or (string? v) (number? v))
                                                        (-> {:constant? v :binds :val} sbind bb-aux),
                                                        (qvar? v) (-> {:sym v :binds :val} sbind bb-aux)
                                                        :else (bb-aux v))))
                                         {} x),
                    (qvar? x)   (-> {:sym x} sbind bb-aux), ; From vector, I suppose.
                    (vector? x) (mapv bb-aux x),
                    :else x))]
      (bb-aux body))))

(def path   "Atom tracking the path navigated in express body, shared only be sbind-path-to! and its caller." (atom []))
(def found? "Atom required because recursively throwing out of sbind-path-to!." (atom nil))
(defn sbind-eq
  "Return true if the two arguments are the same sbind, disregarding value."
  [x y]
  (and (sbind? x) (sbind? y) (== (:pos x) (:pos y))))

(defn sbind-path-to!
  "Navigate the body to the argument sbind, tracking the path in the path atom.
   Executed for its side-effect; returns nil."
  [vsb body]
  (letfn [(ppop! []      (swap! path #(if @found? % (-> % butlast vec))))
          (ppush! [step] (swap! path #(if @found? %      (conj % step))))
          (path-to-aux [vsb bod]
            (try (cond @found?               (throw #?(:clj (Throwable.) :cljs js/Error)) ; unwind.
                       (sbind-eq vsb bod)    (do (reset! found? true) (throw #?(:clj (Throwable.) :cljs js/Error)))
                       (map? bod)            (doseq [[k v] bod]
                                               (ppush! k) (path-to-aux vsb v) (ppop!)),
                       (vector? bod)         (doseq [elem bod] (path-to-aux vsb elem)))
                 (catch #?(:clj Throwable :cljs :default) _e nil)))]
    (path-to-aux vsb body)))

(defn body&bset-ai-maps
  "Process the bset and body together, returning the assoc-in maps (ai-maps) it defines.
   A ai-map describes a :val that is to be assoc-in'd to a :key
   The form of the  ai-map is:
   {:key - some vector of keys constructed from the body and bset;
           the elements of the vector are either sbinds or strings.
    :val - the value associated with the bset's qvar at the end of this path.}
   The body argument has sbinds wherever the user's express body has a qvar or
   a constant-valued map value.
   (Map KEYS that are constants must be strings or numbers and are left unchanged.)"
  [body bset]
  (let [body-sbinds-atm (atom [])
        bset (reduce-kv (fn [m k v] (assoc m (-> k name symbol) v)) {} bset)]
    (letfn [(bsbinds! [x]
              (cond  (sbind? x)   (swap! body-sbinds-atm conj x)
                     (map? x)     (doseq [[k v] x] (bsbinds! k) (bsbinds! v))
                     (vector? x)  (doseq [e x] (bsbinds! e))))]
      (bsbinds! body)
      (let [body-sbinds @body-sbinds-atm
            val-sbinds (->> body-sbinds
                            (filter #(= (:binds %) :val))
                            (mapv #(assoc % :val
                                          (or (:constant? %) (get bset (:sym %))))))]
        (reduce
         (fn [res vsb]
           (reset! path []) (reset! found? nil)
           (let [k ; @path has the nil-sbinds; we need the :val on the arg bset.
                 (->> (mapv (fn [kpart]
                              (cond (sbind? kpart)        (get bset (:sym kpart))
                                    ;;(or (string? kpart)
                                    ;;(number? kpart))  (some #(when (= % kpart) (:val %)) val-sbinds),
                                    :else                 kpart))
                            (do (sbind-path-to! vsb body) @path))
                      (mapv #(if (keyword? %) (name %) %)))]
             (assoc res k (:val vsb))))
         {} val-sbinds)))))

(defn reduce-express
  "This function performs $reduce on an express function.
   The express function is entirely processable by the ordinary method of $reduce,
   but here it is used for the assoc-in-map that it creates and attaches to the
   result when it is executed. It is the assoc-in-map that makes threading each
   bset into the $reduce result possible."
  ([b-sets efn] (reduce-express b-sets efn {}))
  ([b-sets efn init]
   (->> (map efn b-sets) ; The b-sets are executed for there meta! Crazy, huh?
        (mapcat #(-> % meta :bi/ai-map))
        (sort-by first)
        (reduce (fn [m [k v]] (assoc-in m k v)) init))))

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
                   :else (fj/fail  "No such database: %s" db-name))]
    (update-in mc [type db-name] #(update-db % schema-data))))