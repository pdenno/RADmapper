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
   [clojure.data.json            :as json]
   [clojure.data.codec.base64    :as b64]
   [clojure.spec.alpha           :as s]
   [clojure.string               :as str :refer [index-of]]
   [clojure.walk                 :refer [keywordize-keys]]
   [dk.ative.docjure.spreadsheet :as ss]
   [datahike.api                 :as d]
   [rad-mapper.query             :as qu]
   [rad-mapper.util              :as util]))

(def ^:dynamic $  "JSONata context variable" (atom nil))
(def           $$ "JSONata root context."    (atom nil))

(declare aref)

(defmacro with-context
  "Dynamically bind $ to the value provided."
  [val & body]
  `(binding [$ (atom ~val)] ~@body))

(defn set-context! [val] (reset! $ val))

(s/def ::number number?)
(s/def ::pos-number (s/and number? pos?))
(s/def ::integer integer?)
(s/def ::string string?)
(s/def ::pos-limit (s/or :number #(and (number? %) (pos? %)) :unlimited #{:unlimited}))
(s/def ::limit (s/or :number number? :unlimited #{:unlimited}))
(s/def ::str|regex (s/or :string string? :regex util/regex?))
(s/def ::non-zero (s/and number? #(-> % zero? not)))
(s/def ::numbers (s/and vector? (s/coll-of ::number :min-count 1)))
(s/def ::strings (s/and vector? (s/coll-of ::string :min-count 1)))

(defn reset-env
  "Clean things up just prior to running user code."
  ([] (reset-env nil))
  ([context] (reset! $ context)))

(defn flatten-except-json
  "Adapted from core/flatten:
   Takes any nested combination of sequential things (lists, vectors, etc.)
   and returns their contents as a single, flat lazy sequence.
   (flatten nil) returns an empty sequence.
   EXCEPTION: If the thing is a vector with metadata :type :bi/json-array, it isn't flattened. "
  [x]
  (letfn [(seq-except? [o]
            (and (sequential? o)
                 (not (-> o meta :bi/json-array?))))]
    (-> (remove seq-except? (tree-seq seq-except? seq x))
        vec)))

(defn container?   [obj] (-> obj meta :bi/container?))
(defn containerize [obj] (with-meta obj (merge (meta obj) {:bi/container? true})))
(defn containerize?
  "If obj is a vector, set  metadata :bi/container?."
  [obj]
  (if (vector? obj)
    (with-meta obj (merge (meta obj) {:bi/container? true}))
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
            (cond (map? o) (reduce-kv
                            (fn [m k v]
                              (if (or (nil? v) (and (coll? v) (empty? v))) m (assoc m k (elim-empty v))))
                            {}
                            o)
                  (vector? o) (->> o (remove nil?) (mapv elim-empty))
                  :else o))]
    (cond (container? obj)
          (let [len (count obj)]
            (cond (== 0 len) nil ; Or should I call it ::no-match ? Rule 1
                  (== 1 len) (-> obj first elim-empty) ; (-> Rule 2, Rule 1)
                  :else (-> (elim-empty obj) flatten-except-json containerize))) ; Rule 1, Rule 3 JSON array.

          (vector? obj) (let [obj (->> obj (remove nil?) vec)
                              len (count obj)]
                          (cond (== 0 len) nil ; Or should I call it ::no-match ? Rule 1
                                ;(== 1 len) (first obj)
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

;;; JSONata ~> is like Clojure ->, you supply it with a form having one less argument than needed.
;;; [6+1, 3] ~> $sum()           ==> 10
;;; 4 ~> function($x){$x+1}()    ==>  5
;;; The only reason for keeping this around (rather than rewriting it as ->) is that it only takes two args.
(defmacro thread "Implements JSONata ~>"
  [x y]
  `(-> ~x ~y))

;;; ------------------ Path implementation ---------------------------------
(defn run-steps
  "Run or map over each path step function, passing the result to the next step."
  [& steps]
  (binding [$ (atom @$)] ; Make a new temporary context that can be reset in the steps.
    (let [init-step? (= :bi/init-step (-> steps first meta :bi/step-type))]
      (loop [res   (if init-step? (set-context! (-> ((first steps) @$) containerize?)) @$)
             steps (if init-step? (rest steps) steps)]
      (cond (empty? steps) res

            ;; bi/get-step followed by bi/filter-step ("non-compositional" ???)
            (and (= :bi/get-step (-> steps first meta :bi/step-type))
                 (= :bi/filter-step (-> steps second meta :bi/step-type))) ; Then don't do next step. Steal its argument.
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
                             (throw (ex-info "Huh?" {:meta (-> sfn meta :bi/step-type)}))))]
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
  (with-meta
    (fn filter-step [obj prior-step]
      (let [prix   (try (pred|ix-fn @$) (catch Exception _e nil))
            call-type (:bi/call-type prior-step)
            ob (if (= :bi/get-step call-type)
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
    {:bi/step-type :bi/filter-step}))

(defn get-step
  "Perform the mapping activity of the 'a' in $.a, for example.
   This function is called with the state object.
   It returns the state object with :sys/$ updated."
  [k]
  (with-meta
    (fn get-step [& args] ; No arg if called in a primary.
      (let [obj (-> (if (-> args first empty?) @$ (first args)))]
        (cond (map? obj)      (get obj k)
              (vector? obj)   (->> obj
                                   containerize
                                   (cmap #(get % k))
                                   ;; lightweight flatten
                                   (reduce (fn [res x] (if (vector? x) (into res x) (conj res x))) [])
                                   containerize)
              :else           nil)))
    {:bi/step-type :bi/get-step :bi/arg k}))

;;; ToDo: Currently this really only gets called for [] syntax. Is there more I just haven't seen?
(defmacro value-step
  "Return a function that evaluates what is in the the []  'hello' in [1,2,3].['hello']
   and the truth values [[false] [true] [true]] of [1,2,3].[$ = 2]."
  [body]
  `(with-meta
     (fn ~'value-step [x#]
       (let [func# (fn [y#] (binding [$ (atom y#)] ~body))]
         (if (vector? x#)
           (->> (mapv func# x#) (mapv vector))
           (vector (func# x#)))))
     {:bi/step-type :bi/value-step}))

(defn get-scoped
  "Access map key like clj/get, but with arity overloading for $."
  ([k] (get-scoped @$ k))
  ([obj k] (get obj k)))

(defmacro primary
  "Return a function with meta {:bi/step-type :bi/primary} that optionally takes
   the context atom and runs the body."
  [body]
  `(with-meta
     (fn ~'primary [& arg#] (binding [bi/$ (if (empty? arg#) bi/$ (-> arg# first atom))] ~body))
     {:bi/step-type :bi/primary}))

(defmacro init-step
  "All the arguments of bi/run-steps are functions. This one just runs the argument body,
   which might construct a literal value, be a literal value, or call a function."
  [body]
  `(with-meta
     (fn [_x#] ~body)
     {:bi/step-type :bi/init-step}))

(defmacro map-step
  "All the arguments of bi/run-steps are functions. This one maps $ over the argument body."
  [body]
  `(with-meta
     (fn [_x#] ~body)
     {:bi/step-type :bi/map-step}))

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
            (cond (map? form) (reduce-kv (fn [m k v] (assoc m k (sba-aux v))) {} form)
                  (vector? form) (mapv sba-aux form)
                  (seq? form) (cond (and (core/= (first form) 'bi/key) (== 2 (count form)))
                                    (list 'bi/key sym (second form))
                                    (core/= form '(core/deref rad-mapper.builtins/$)) sym ; 2022-05-29, added.
                                    :else (map sba-aux form))
                  :else form))]
    (sba-aux form)))

;;;--------------------------- JSONata built-in functions ------------------------------------

;;;------------- String --------------
;;; $base64decode
(defn $base64decode
  "Converts base 64 encoded bytes to a string, using a UTF-8 Unicode codepage."
  [c]
  (-> c .getBytes b64/decode String.))

;;; $base64encode
(defn $base64encode
  "Converts an ASCII string to a base 64 representation.
   Each each character in the string is treated as a byte of binary data.
   This requires that all characters in the string are in the 0x00 to 0xFF range,
   which includes all characters in URI encoded strings.
   Unicode characters outside of that range are not supported."
  [s]
  (-> s .getBytes b64/encode String.))

;;; $contains
(defn $contains
  "Returns true if str is matched by pattern, otherwise it returns false.
   If str is not specified (i.e. this function is invoked with one argument),
   then the context value is used as the value of str.

   The pattern parameter can either be a string or a regular expression (regex).
   If it is a string, the function returns true if the characters within pattern are
   contained contiguously within str.
   If it is a regex, the function will return true if the regex matches the contents of str."
  ([pat] ($contains @$ pat))
  ([s pat]
   (s/assert ::string s)
   (if (util/regex? pat)
     (if (re-find pat s) true false)
     (if (index-of s pat) true false))))

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
(defn $eval
  ([s] ($eval s @$))
  ([s context]
   (s/assert ::string s)
   (let [rewrite (ns-resolve 'rad-mapper.rewrite 'rewrite*)
         form (rewrite :ptag/exp s :rewrite? true)]
     (binding [*ns* (find-ns 'user)]
       (try
         (reset-env context)
         (let [res (eval form)]
           (if (and (fn? res) (= :bi/primary (-> res meta :bi/step-type)))
             (jflatten (res))
             (jflatten res)))
         (catch Exception e
           (throw (ex-info "Error evaluating form:" {:error e :form form}))))))))

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
;;; ToDo: If in conforming JSONata null groups is not removed from the result,
;;;       it may be necessary to mark this with special metadata {:bi/regex-result true}.

;;; ToDo: Go back and find the old defn*
(defn $match
  "Return a JSONata-like map for the result of regex mapping.
   Pattern is a Clojure regex."
  ([pattern] ($match @$ pattern))
  ([s_ pattern]
   (letfn [(match-aux [s]
             (let [result (re-find pattern s)]
               (cond (string? result) {"match" result
                                       "index" (index-of s result)
                                       "groups" []}
                     (vector? result)
                     (let [[success & groups] result]
                       {"match" success
                        "index" (index-of s success)
                        "groups" (vec groups)}))))]
     ;; ToDo: I think this should be the pattern of every defn* ???
     (if (vector? s_) (mapv match-aux s_) (match-aux s_)))))

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
                     (if (= :unlimited lim) (-> s count range) (range lim)))),
           (fn? replacement)
           (reduce (fn [res _i]
                     (try (let [repl (-> ($match res pattern) replacement)]
                            (str/replace-first res pattern repl))
                          ;; ToDo: Need a better way! See last test of $replace in builtins_test.clj
                          (catch Exception _e res)))
                   s
                   (if (= :unlimited lim) (-> s count range) (range lim))), ; ToDo: (-> s count range) is a guess.
           :else (throw (ex-info "Replacement pattern must be a string or function."
                                 {:replacement replacement}))))))

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
;;; $average
;;; $ceil
;;; $floor
;;; $formatInteger
;;; $round
;;; $formatBase
;;; $formatNumber

;;; $number
(defn $number
  " * Numbers are unchanged.
    * Strings that contain a sequence of characters that represent a legal JSON number are converted to that number.
    * Boolean true casts to 1, Boolean false casts to 0.
    All other values cause an error to be thrown."
  [v_]
  (cond (number? v_) v_
        (string? v_) (let [n (read-string v_)]
                      (if (number? n)
                        n
                        (throw (ex-info "Cannot be cast to a number:" {:value v_}))))
        (boolean? v_) (if v_ 1 0)
        :else (throw (ex-info "Cannot be cast to a number:" {:value v_}))))

;;; $power
(defn $power
  "Return the largest the numeric argument (an array or singleton)."
  [x y]
  (s/assert ::number x)
  (s/assert ::number y)
  (if (and (integer? x) (pos-int? y))
    (int (Math/pow x y))
    (Math/pow x y)))

;;; ToDo: Maybe these should be defn* ?
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
;;; $random

;;; ToDo: This is bogus!
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

;;;--------------- Logic ------------
;;; $boolean
;;; $exists
;;; $not

;;;--------------- Collections -------
;;; $append
;;; $count
;;; $distinct
;;; $reverse
;;; $shuffle
;;; $sort
;;; $zip

;;;---------------- JSON Object ------
;;; $type
;;; $lookup
;;; $merge
;;; $assert
;;; $sift
;;; $error
;;; $each
;;; $keys
;;; $spread

;;;------------- DateTime -----------
;;; $fromMillis
;;; $millis,
;;; $now
;;; $toMillis

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
                                      {:vars (-> func meta :bi/params)})))))

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
                            {:vars (-> func meta :bi/params)})))))

;;; $reduce
(defn reduce-body-enforce
  "This is for reducing with an enforce function, where metadata on the
   function directs various behaviors such as whether results are returned
   or a database is updated."
  [coll func init] ; ToDo: Collection could be a DB (e.g. from $MCgetSource()).
  (let [result (->> (loop [c coll,
                           r init]
                      (if (empty? c) r (recur (rest c), (conj r (func (first c))))))
                    (mapv keywordize-keys))
        _db (qu/db-for! result)]
      ;; result is just the collection of results from body mechanically produced.
      ;; They describe fact types that need to be organized according to the schema.
      ;; (-> func meta :options) describes the schema, among other things.
      result))

;;; ToDo: What is the point of the 4th parameter in function called by $reduce?
;;;       My doc-string below doesn't say.
;;;       In the above I pass it the collection untouched every time.
;;;       The JSONata documentation (http://docs.jsonata.org/higher-order-functions) doesn't say either!
(defn reduce-body-typical
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
  ([coll func init]  (if (-> func meta :enforce?)
                       (reduce-body-enforce coll func init)
                       (if (core/<= 2 (-> func meta :bi/params count) 4)
                         ($reduce (into (vector init) coll) func)
                         (throw (ex-info "$reduce expects a function of 2 to 4 arguments:"
                                         {:vars (-> func meta :bi/params)})))))
  ([coll func] (if (-> func meta :enforce?)
                 (reduce-body-enforce coll func [])
                 (reduce-body-typical coll func))))

;;; $sift

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
                            {:vars (-> func meta :bi/params)})))))

;;;==========================================================================
;;;=================== Non-JSONata functions ================================
(defn $readFile
  "Read a file of JSON or XML, creating a map."
  ([fname] ($readFile fname {})) ; For Javascript-style optional params; see https://tinyurl.com/3sdwysjs
  ([fname opts]
   (let [type (second (re-matches #"^.*\.([a-z,A-Z,0-9]{1,5})$" fname))]
     (reset! $$ (case (or (get opts "type") type "xml")
                  "json" (-> fname slurp json/read-str)
                  "xml"  (-> fname util/read-xml :xml/content first :xml/content util/simplify-xml)
                  "edn"  (-> fname slurp read-string qu/json-like)))))) ; Great for testing!

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

;;; $readSpreadsheet
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
                     keys (map keyword (take len (util/string-permute "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
                     raw (ss/select-columns (zipmap keys keys) sheet)]
                 (if invert?
                   (transpose-sheet raw)
                   ;; ToDo This is all sort of silly. Can we access cells a better way?
                   (rewrite-sheet-for-mapper raw)))))))

;;;============================= Mapping Context, query, enforce ======================

;;; Thoughts on schema
;;;   - Learned schema are sufficient for source data (uses qu/db-for!)
;;;   - What is provided as argument overrides what is learned in $query.
;;;   - $enforce could be with an argument schema.
(defn $schemaFor
  "Study the argument data and heuristically suggest the types and multiplicity of data.
   Note that this function does not make a guess at what the keys (db/key) are."
  [data_]
  (qu/learn-schema-walking data_))

;;;---------- query ---------------------------------------
(defn qform-runtime-sub
  "Return a DH query [:find...] form that substitutes values as though syntax quote were being used."
  [body param-val-map]
  (letfn [(tp-aux [x]
            (cond (vector? x)                  (mapv tp-aux x),
                  (seq? x)                     (map  tp-aux x),
                  (contains? param-val-map x)  (param-val-map x),
                  :else x))]
    (let [vars (->> (reduce (fn [r x] (into r x)) body)
                    (filter #(str/starts-with? % "?"))
                    distinct)]
    `[:find ~@vars
      :keys ~@(map symbol vars)
      :where ~@(tp-aux body)])))

(defn immediate-query-fn
    "Return a function that can be used immediately to make the query defined in body."
  [body]
  (fn [data|db]
    (let [conn (if (core/= datahike.db.DB (type data|db)) data|db (qu/db-for! data|db))]
      (->> (d/q (qform-runtime-sub body {}) conn) ; This is possible because body is data to d/q.
           ;; Remove binding sets that involve a schema entity.
           (remove (fn [bset] (some (fn [bval]
                                      (and (keyword? bval)
                                           (core/= "db" (namespace bval))))
                                    (vals bset))))
           vec))))

(defn higher-order-query-fn
  "Return a function that can be called with parameters to return a function to m
   the parameterizes query defined by body and params. (It's just a closure...)"
  [body params]
  (fn [& args]
    (let [param-subs (zipmap params args)] ; the closure.
      (fn [data|db]
        (let [conn (if (core/= datahike.db.DB (type data|db)) data|db (qu/db-for! data|db))]
           (->> (d/q (qform-runtime-sub body param-subs) conn)
                ;; Remove binding sets that involve a schema entity.
                (remove (fn [bset] (some (fn [bval]
                                           (and (keyword? bval)
                                                (core/= "db" (namespace bval))))
                                         (vals bset))))
                vec))))))

(defn query
  "There are two uses scenarios for query:
      (1) Calls to query where no parameters are specified return a function
          that takes data and returns binding sets.
      (2) Calls to query that provide parameters return a function that takes
          values for those parameters and return a function of type (1).

  'params' is an ordered vector parameters (jvars) that will be matched to 'args' used
   to parameterized the query form, thus producing a 'customized' query function.

   Example usage (of the second sort):

  ( $data := $MCnewContext() ~> $MCaddSource($readFile('data/testing/owl-example.edn'));
    $q := query($type){[?class :rdf/type            $type]
                       [?class :resource/iri        ?class-iri]
                       [?class :resource/namespace  ?class-ns]
                       [?class :resource/name       ?class-name]};
    $q($data,'owl/Class') )"
  [params body]
  (if (empty? params)
    (immediate-query-fn body)
    (higher-order-query-fn body params)))

(defmacro enforce
  "See query!!!" ; ToDo: a doc string...
  [& {:keys [params body]}]
  (if (empty? params)
    ;; The immediate function.
    `(->
      (fn [~'b-set] ~body)
      (with-meta {:bi/params '[~'b-set] :enforce? true}))
    `(fn [~@params]
       (->
        (fn [~'b-set] ~body)
        (with-meta {:bi/params '[~'b-set] :enforce? true})))))

;;; ToDo: update the schema. This doesn't yet do what its doc-string says it does!
(defn update-db
  "DB is a DH database. Create a new one based on the existing one's content and the new data.
   This entails updating the schema."
  [db data]
  (d/transact db data))

(defn get-from-b-set
  "Given a binding-set and ?query-var (key), return the map's value at that index."
  [bs k]
  (when-not (contains? bs k)
    (throw (ex-info "Argument binding set does not contain the key provided"
                    {:binding-set bs :key k})))
  (get bs k))

;;; ToDo: Theses are always an atom, right?
(defn $MCnewContext
  "Create a new modeling context object."
  []
  (atom {:schemas {} :sources {} :targets {}}))

(defn $MCaddSource
  "Add source data to the argument modeling context.
   If no name is provided for the source, a new one is created.
   Default names follow in the series 'source-data-1', 'source-data-2'...
   If the source already exists, the data is added, possibly updating the schema."
  ([mc data] ($MCaddSource mc data (str "source-data-" (-> mc deref :sources count inc))))
  ([mc data src-name]
   (if (-> mc deref :sources (contains? src-name))
     (swap! mc #(update-in % [:sources src-name] (update-db (-> mc deref :sources (get src-name)) data)))
     (swap! mc #(assoc-in  % [:sources src-name] (qu/db-for! data))))))

;;; ToDo: Currently this just does what $MDaddSource does.
(defn $MCaddTarget
  "Add target data to the argument modeling context.
   This is used, for example, for in-place updating.
   If no name is provided for the target, a new one is created.
   Default names used when not provided follow in the series 'target-data-1', 'target-data-2'...
   If the target already exists, the data is added, possibly updating the schema."
  ([mc data] ($MCaddTarget mc data (->> mc :targets keys (util/default-name "data-targets-"))))
  ([mc data src-name]
   (if (-> mc :targets (contains? src-name))
     (update-in mc [:targets src-name] #(update-db % data))
     (assoc-in  mc [:targets src-name] (qu/db-for! data)))))

(defn $MCgetSource [mc name] (-> mc :sources (get name)))
(defn $MCgetTarget [mc name] (-> mc :targets (get name)))

(defn $MCaddSchema
  "Add knowledge of schema to an existing DB.
   This can be a computational expensive operation when the DB is large."
  [mc schema-data db-name]
  (let [type (cond (-> mc :sources (contains? db-name)) :sources
                   (-> mc :targets (contains? db-name)) :targets
                   :else (throw (ex-info "No such database:" {:db-name db-name})))]
    (update-in mc [type db-name] #(update-db % schema-data))))
