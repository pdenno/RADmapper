(ns rad-mapper.builtins
  "Built-in functions implementing the expression language of the mapping language.
   Functions with names beginning with a '$' are available to the user (e.g. $filter).
   Others (such as bi/access and bi/strcat) implement other parts of the expression language
   but are not available directly to the user except through the operators (e.g. the dot, and &
   respectively for navigation to a property and concatenation)."
  (:refer-clojure :exclude [+ - * /])
  (:require
   [dk.ative.docjure.spreadsheet :as ss]
   [datahike.api                 :as d]
   [datahike.pull-api            :as dp]
   [rad-mapper.query             :as qu]
   [rad-mapper.util              :as util]
   [clojure.string               :as str]))

;;; ToDo:
;;;  -1) defn* should check for vec/scalar.
;;;   0) defn*s do not need to reset! $ (it is in the macro).
;;;   1) Singleton wrapper (on defn* ?)
;;;   2) Implement dynamic var *advance* ???
;;;   3) Consider threading/binding instead of resetting the atom $.
;;;   4) Investigate Small Clojure Interpreter.

(defn jsonata-flatten
  "See http://docs.jsonata.org/processing section 'Sequences'"
  [s]
  (if (vector? s)
    (let [res (-> s flatten vec)
          len (count res)]
      (cond
        (== 0 len) nil,
        (== 1 len) (first res)
        :else res))
    s))

(def $ "The JSONata context variable." (atom nil))
(def $$ "The JSONata root variable." (atom nil))
(def $$$ "The RADmapper mapping concept object." (atom nil))

(defn reset-special!
  "Reset! an atom."
  [atm v]
  (when-not (#{$ $$ $$$} atm)
    (throw (ex-info "In execution, expected an atom." {:got atm})))
  (reset! atm v))

;;; ToDo: No provisions for parameter destructuring. Okay?
;;; ToDo: This jumps through some hoops to avoid ns-qualified params. Is there another way?
;;; (macroexpand-1 `(defn* example "This is an example." [x y_] {:val (+ x y_)}))
(defmacro defn*
  "Define two function arities using the body:
     (1) the ordinary one, that has the usual arguments for the built-in, and,
     (2) a function where the missing argument will be assumed to be the context variable, $.
   The parameter ending in a \\_ is the one elided in (2). (There must be such a parameter.)
   Additionally, reset the context variable to the value returned by the function.
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
       ([~@(vals param-map)] (reset! $ ~@(rewrite body)))))))

;;;========================= JSONata built-ins  =========================================
(defn* + "plus" [x_ y]
  (if (number? x_)
    (clojure.core/+ x_ y)
    (throw (ex-info "The left side of the '+' operator must evaluate to a number." {:op1 x_}))))

(defn* - "minus" [x_ y]
  (if (number? x_)
    (clojure.core/- x_ y)
    (throw (ex-info "The left side of the '-' operator must evaluate to a number." {:op1 x_}))))

(defn* * "times"  [x_ y]
    (if (number? x_)
    (clojure.core/* x_ y)
    (throw (ex-info "The left side of the '*' operator must evaluate to a number." {:op1 x_}))))

(defn* / "divide" [x_ y]
  (if (number? x_)
    (double (clojure.core// x_ y))
    (throw (ex-info "The left side of the '/' operator must evaluate to a number." {:op1 x_}))))

(defn* $number
  " * Numbers are unchanged
    * Strings that contain a sequence of characters that represent a legal JSON number are converted to that number
    * Boolean true casts to 1, Boolean false casts to 0
    All other values cause an error to be thrown."
  [v_]
  (cond (number? v_) v_
        (string? v_) (let [n (read-string v_)]
                      (if (number? n)
                        n
                        (throw (ex-info "Cannot be cast to a number:" {:value v_}))))
        (boolean? v_) (if v_ 1 0)
        :else (throw (ex-info "Cannot be cast to a number:" {:value v_}))))

(defn* $sum
  "Sum returns the sum of the argument (a vector of numbers)."
  [v_]
  (if (and (vector? v_) (every? number? v_))
    (apply + v_)
    (throw (ex-info "In $sum, argument does not look like a vector of numbers." {:value v_}))))

;;; ToDo: $contains(), $split(), $replace()
;;; ToDo: flags on regular expressions. /regex/flags  (the only flags are 'i' and 'm' (case insenstive, multi-line)).
;;; (re-find #"foo" "foovar")
(defn* $match
  "Return a JSONata-like map for the result of regex mapping.
   Pattern is a Clojure regex."
  [s_ pattern]
  (letfn [(match-aux [s]
            (let [result (re-find pattern s)]
              (cond (string? result) {"match" result
                                      "index" (str/index-of s result)
                                      "groups" []}
                    (vector? result)
                    (let [[success & groups] result]
                      {"match" success
                       "index" (str/index-of s success)
                       "groups" (vec groups)}))))]
    ;; ToDo: I think this should be the pattern of every defn* ???
    (if (vector? s_)
      (mapv match-aux s_)
      (match-aux s_))))

(defn* $string
  "Return the argument as a string."
  [s_] (str s_))

(defn access-internal
  "The JSONata . operator; it does an implicit map over the property."
  [obj prop]
  ;; Could be a vector of content but also could be a vector of vectors of content.
  ;; The latter because of JSONata's implicit 'map over' semantics. Don't go deeper.
  (let [res (cond (fn? prop) (prop obj) ; query is like this.
                  (and (vector? obj) (every? map? obj)) (mapv #(get % prop) (filter #(contains? % prop) obj)),
                  (map? obj) (get obj prop),
                  :else (throw (ex-info "Expected a map or vector of maps" {:got obj})))]
    (jsonata-flatten res)))

(defn* access "JSONata . operator" [obj_ prop] (access-internal obj_ prop))

;;; JSONata ~> is like Clojure ->, you supply it with a form having one less argument than needed.
;;; [6+1, 3] ~> $sum()           ==> 10
;;; 4 ~> function($x){$x+1}()    ==>  5
;;; The only reason for keeping this around (rather than rewriting it as ->) is that it only takes two args.
(defmacro thread [x y] `(-> ~x ~y))

;;; ToDo: Review value of meta in the following.
;;;----------------- Higher Order Functions --------------------------------
;;; These cannot access the context variable implicitly (by reduced arity).
;;; Thus Account.Order.Product.ProductID.$map($, $string)
;;; not  Account.Order.Product.ProductID.$map($string). Thus not defn*.
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
  (reset! $
          (let [nvars  (-> func meta :params count)]
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
                                    {:vars (-> func meta :params)}))))))

;;; (bi/$filter [1 2 3 4] (with-meta (fn [v i a] (when (even? v) v)) {:params {:val-var 'v :index-var 'i :array-var 'a}}))
(defn* $filter
  "Return a function for the argument form."
  [coll_ func]
  (reset! $ (let [nvars (-> func meta :params count)]
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
                                      {:vars (-> func meta :params)}))))))

;;; ToDo: Check whether $map can use bi/access.
;;; Certainly "mapping" in the sense of  $.(<expr) can, but that's a different sort of thing.
(defn $reduce
  "Signature: $reduce(array, function [, init])

  Returns an aggregated value derived from applying the function parameter successively to
  each value in array in combination with the result of the previous application of the function.

  The function must accept at least two arguments, and behaves like an infix operator
  between each value within the array. The signature of this supplied function must be of the form:
  myfunc($accumulator, $value [, $index [, $array]])

  Example:   ( $product := function($i, $j){$i * $j};
               $reduce([1..5], $product)
             )
  This multiplies all the values together in the array [1..5] to return 120.

  If the optional init parameter is supplied, then that value is used as the initial value in the aggregation (fold) process.
  If not supplied, the initial value is the first value in the array parameter"
  ([coll func init] ($reduce (into (vector init) coll) func))
  ([coll func]
   (reset! $ (let [nvars  (-> func meta :params count)]
               ;; Deal with the accumulator function having 2-4 arguments.
               (cond
                 (== nvars 4)
                 (loop [c (rest coll), i 0, r (first coll)]
                   (if (empty? c)
                     r
                     (let [val (func r (first c) i coll)]
                       (recur (rest c), (inc i), val))))
                 (== nvars 3)
                 (loop [c (rest coll), i 0, r (first coll)]
                   (if (empty? c)
                     r
                     (let [val (func r (first c) i)]
                       (recur (rest c), (inc i), val))))
                 (== nvars 2)
                 (loop [c (rest coll), r (first coll)]
                   (if (empty? c)
                     r
                     (let [val (func r (first c))]
                       (recur (rest c) val))))
                 :else
                 (throw (ex-info "$reduce expects a function of 2 to 4 arguments:"
                                 {:vars (-> func meta :params)})))))))

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
  (reset! $ (let [nvars  (-> func meta :params count)]
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
                                    {:vars (-> func meta :params)}))))))

(defn strcat
  "The JSONata & operator."
  [& objs]
  (apply str objs))

;;; rewrite.clj would have been a good place for this, but here we need it at runtime!
;;; ToDo: sym-bi-access IS executed for $map in rewrite. So can that for filter too?
;;; N.B. filter-fn below calls eval.
(defn sym-bi-access
  "When doing mapping such as '$.(A * B)' the expressions A and B
   were rewritten (bi/access 'A'). There needs to be a first argument
   to that, before the 'A'. This function inserts the argument symbol
   in such forms so that they can be wrapped in (fn [<that symbol>] ...)
   and called in map/filter/reduce settings."
  [form sym]
  (letfn [(sba-aux [form]
            (cond (map? form) (reduce-kv (fn [m k v] (assoc m k (sba-aux v))) {} form)
                  (vector? form) (mapv sba-aux form)
                  (seq? form) (if (and (= (first form) 'bi/access) (== 2 (count form)))
                                 `(~(first form) ~sym ~@(map sba-aux (rest form)))
                                 (map sba-aux form))
                  :else form))]
    (sba-aux form)))

(def ^:dynamic *test-sym* "Used with sym-bi-access calls in testing" 'foo) ; <========== ToDo: Why can't this be nil?
(defn filter-fn
  "Return a function for form, making the sym-bi-access insertions."
  [form]
  (let [sym (or *test-sym* (gensym "x"))]
    (eval `(fn [~sym] ~(sym-bi-access form sym)))))

;;; ToDo I don't think this is a candidate for defn*. [<exp>] returns the value of <exp> (which in JSONata is same as a singleton array).
(defmacro filter-aref
  "Performs array reference or predicate application (filtering) on the arguments following JSONata rules:
    (1) If the expression in square brackets (second arg) is non-numeric, or is an expression that doesn't evaluate to a number,
        then it is treated as a predicate.
    (2) Negative indexes count from the end of the array, for example, arr[-1] will select the last value, arr[-2] the second to last, etc.
        If an index is specified that exceeds the size of the array, then nothing is selected.
    (3) If no index is specified for an array (i.e. no square brackets after the field reference), then the whole array is selected.
        If the array contains objects, and the location path selects fields within these objects, then each object within the array
        will be queried for selection."
  [obj pred|ix]
  `(let [prix# (try ~pred|ix (catch Exception _e# nil))
         obj# ~obj]
     (if (number? prix#)
       ;; Array behavior.
       ;; Singletons are treated as values, AND values are treated as singletons!
       (let [obj# (if (vector? obj#) obj# (vector obj#))
             ix#  (-> prix# Math/floor int) ; Really! I checked!
             len# (count obj#)]
         ;; [:a :b :c :d][0] ==> :a ; [:a :b :c :d][-4] ==> :a.
         (if (or (and (pos? ix#) (>= ix# len#))
                 (and (neg? ix#) (> (Math/abs ix#) len#)))
           nil ; Rule 2, above.
           (if (neg? ix#)
             (nth obj# (clojure.core/+ len# ix#))
             (nth obj# ix#))))
       ;; Filter behavior
       (reset! $ (filterv (fn [arg#] (reset! $ arg#) ~pred|ix) obj#)))))

;;;=================== Non-JSONata functions ================================

;;; ToDo: Currently no JSON
(defn $readFile
  "Read a file of JSON or XML, creating a map."
  ([fname] ($readFile fname {})) ; For Javascript-style optional params; see https://tinyurl.com/3sdwysjs
  ([fname opts]
   (let [type (second (re-matches #"^.*\.([a-z,A-Z,0-9]{1,5})$" fname))]
     (case (or (get opts "type") type "xml")
       "xml" (reset! $ (-> fname util/read-xml :xml/content first :xml/content util/simplify-xml))
       "edn" (reset! $ (-> fname slurp read-string qu/json-like)))))) ; Great for testing!

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

;;; ($readSpreadsheet
(defn $readSpreadsheet
  "Read the .xlsx and make a clojure map for each row. No fancy names, just :A,:B,:C,...!"
  ([filename sheet-name] ($readSpreadsheet filename sheet-name false))
  ([filename sheet-name invert?]
   (reset! $ (when-let [sheet (->> (ss/load-workbook filename) (ss/select-sheet sheet-name))]
               (let [row1 (mapv ss/read-cell (-> sheet ss/row-seq first ss/cell-seq ss/into-seq))
                     len  (loop [n (dec (-> sheet ss/row-seq first .getLastCellNum))]
                            (cond (= n 0) 0,
                                  (not (nth row1 n)) (recur (dec n)),
                                  :else (inc n)))
                     keys (map keyword (take len (util/string-permute "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
                     raw (ss/select-columns (zipmap keys keys) sheet)]
                 (if invert?
                   (transpose-sheet raw)
                   ;; ToDo This is all sort of silly. Can we access cells a better way?
                   (rewrite-sheet-for-mapper raw)))))))

;;;============================= JSONata mostly-one-liners ======================

;;;------------- String
;;; $trim
;;; $uppercase
;;; $length
;;; $substringAfter
;;; $substring
;;; $base64encode
;;; $encodeUrl
;;; $eval
;;; $string
;;; $encodeUrlComponent
;;; $contains
;;; $join
;;; $substringBefore
;;; $base64decode
;;; $split
;;; $pad
;;; $replace
;;; $lowercase
;;; $decodeUrl
;;; $decodeUrlComponent

;;;------------- Numeric
;;; $abs
;;; $floor
;;; $parseInteger
;;; $number
;;; $formatInteger
;;; $round
;;; $formatBase
;;; $formatNumber
;;; $ceil
;;; $random
;;; $power
;;; $average
;;; $max
;;; $min
;;; $sum  (see above, start defining specs, drop all the throws.)
;;; $sqrt

;;;--------------- Logic
;;; $boolean
;;; $exists
;;; $not

;;;--------------- Collections
;;; $append
;;; $count
;;; $distinct
;;; $reverse
;;; $shuffle
;;; $sort
;;; $zip

;;;---------------- JSON Object
;;; $type
;;; $lookup
;;; $merge
;;; $assert
;;; $sift
;;; $error
;;; $each
;;; $keys
;;; $spread

;;;------------- DateTime
;;; $fromMillis
;;; $millis,
;;; $now
;;; $toMillis

;;;-------------- Higher (the higher not yet defined)
;;; $sift
;;; $single


;;;============================= Mapping Context, query, enforce ======================

;;; Thoughts on schema
;;;   - Learned schema are sufficient for source data (uses qu/db-for!)
;;;   - One needs to to specify schema on $transform, even if it is {}
;;;   - What is provided as argument overrides what is learned in $query.
;;;   - $enforce could be with an argument schema.
(defn* $schemaFor
  "Study the argument data and heuristically suggest the types and multiplicity of data.
   Note that this function does not make a guess at what the keys (db/key) are."
  [data_]
  (qu/learn-schema-walking data_))

;;;--------------------------------- query ---------------------------------------
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
      :keys ~@(->> vars (map #(subs (str %) 1)) (map symbol))
      :where ~@(tp-aux body)])))

(defn immediate-query-fn
    "Return a function that can be used immediately to make the query defined in body."
  [body]
  (fn [mc]
    (let [conn mc]
      (->> (d/q (qform-runtime-sub body {}) conn)
           ;; Remove binding sets that involve a schema entity.
           (remove (fn [bset] (some (fn [bval]
                                      (and (keyword? bval)
                                           (= "db" (namespace bval))))
                                    (vals bset))))
           vec))))

(defn higher-order-query-fn
  "Return a function that can be called with parameters to return a function to m
   the parameterizes query defined by body and params. (It's just a closure...)"
  [body params]
  (fn [& args]
    (let [param-subs (zipmap params args)] ; the closure.
      (fn [mc]
         (let [conn mc]
           (->> (d/q (qform-runtime-sub body param-subs) conn)
                ;; Remove binding sets that involve a schema entity.
                (remove (fn [bset] (some (fn [bval]
                                           (and (keyword? bval)
                                                (= "db" (namespace bval))))
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

;;; Currently there is no macro for bi/enforce. It is simply rewritten to a Clojure fn.
#_(defmacro enforce [& {:keys [config body]}]  (:nyi))

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

(defn $MCgetSource [mc name] (-> mc :sources mc (get name)))
(defn $MCgetTarget [mc name] (-> mc :targets mc (get name)))

(defn $MCaddSchema
  "Add knowledge of schema to an existing DB.
   This can be a computational expensive operation when the DB is large."
  [mc schema-data db-name]
  (let [type (cond (-> mc :sources (contains? db-name)) :sources
                   (-> mc :targets (contains? db-name)) :targets
                   :else (throw (ex-info "No such database:" {:db-name db-name})))]
       (update-in mc [type db-name] #(update-db % schema-data))))
