(ns rad-mapper.parse
  "Parse the JSONata-like message mapping language."
  (:require
   [clojure.pprint :as pp :refer [cl-format]]
   [clojure.string :as str]
   [clojure.set    :as set]
   [clojure.spec.alpha :as s]
   [rad-mapper.util :as util]))

;;; The 'defparse' parsing functions pass around complete state.
;;; The 'parse state' (AKA pstate) is a map with keys:
;;;   :result  - the parse structure from the most recent call to (parse :<some-rule-tag> pstate)
;;;   :tokens  - tokenized content that needs to be parsed into :model. First on this vector is also :tkn.
;;;   :tags    - a stack of tags indicating where in the grammar it is parsing (used for debugging)
;;;   :head    - current token, not yet consumed. It is also the first token on :tokens.
;;;   :line    - line in which token appears.
;;;   :col     - column where token starts.
;;;   :local   - temporarily stored parse content used later to form a complete grammar element.
;;;              It is a vector (stack) of maps. On entry, defparse pushes a new empty map on it;
;;;              on exit, it pops it. Macros store and recall push onto the top map of the stack.
;;;              For example use, see :ptag/MapSpec and parse-list-terminated.

;;; ToDo:
;;;   1) $.( and $.{    See http://docs.jsonata.org/sorting-grouping
;;;      Write a function to read in the context so you can use their data in testing,  such as shown:
;;;      Account.Order.Product { `Product Name`: $.{"Price": Price, "Qty": Quantity}}
;;;   2) Write defparse :ptag/exp; it should have a spec naming all expressions.
;;;      :ptag/code-block can essentially be treated as an expression. (Could even clear $, $$$ between calls.)

(def ^:dynamic *debugging?* false)
(util/config-log (if *debugging?* :debug :info))

(def block-size "Number of lines to tokenize together. Light testing suggests 5 is about fastest." 5)

;;; ============ Tokenizer ===============================================================
(def keywords
  #{"alias" "and" "else" "elseif" "endif" "false" "for" "function" "enforce" "if" "in" "int" "library" "list" "metadata"
    "of" "or" "query" "return" "source" "string" "target" "then" "transform" "true" "where"})

(defn straight-to-bi [m]
  (reduce (fn [res s]
            (assoc res s (symbol "bi" (cond (char? s) (str s)
                                            (keyword? s) (name s)
                                            :else s)))) {} m))

;;; http://docs.jsonata.org/string-functions
(def string-fns
  '{"$trim" bi/$trim, "$uppercase" bi/$uppercase, "$length" bi/$length, "$substringAfter" bi/$substringAfter,  "$substring" bi/$substring,
    "$base64encode" bi/$base64encode, "$encodeUrl" bi/$encodeUrl, "$eval" bi/$eval, "$string" bi/$string, "$encodeUrlComponent" bi/$encodeUrlComponent,
    "$contains" bi/$contains, "$match" bi/$match, "$join" bi/$join, "$substringBefore" bi/$substringBefore, "$base64decode" bi/$base64decode,
    "$split" bi/$split, "$pad" bi/$pad, "$replace" bi/$replace, "$lowercase" bi/$lowercase, "$decodeUrl" bi/$decodeUrl,
    "$decodeUrlComponent" bi/$decodeUrlComponent})

(def numeric-fns
  '{"$sqrt" bi/$sqrt, "$abs" bi/$abs, "$floor" bi/$floor, "$parseInteger" bi/$parseInteger, "$number" bi/$number, "$formatInteger" bi/$formatInteger,
    "$round" bi/$round, "$formatBase" bi/$formatBase, "$formatNumber" bi/$formatNumber, "$ceil" bi/$ceil, "$random" bi/$random, "$power" bi/$power})

(def agg-fns      '{"$average" bi/$average, "$max" bi/$max, "$min" bi/$min, "$sum" bi/$sum})
(def boolean-fns  '{"$boolean" bi/$boolean, "$exists" bi/$exists, "$not" bi/$not})
(def array-fns    '{"$append" bi/$append, "$count" bi/$count, "$distinct" bi/$distinct, "$reverse" bi/$reverse,
                    "$shuffle" bi/$shuffle, "$sort" bi/$sort, "$zip" bi/$zip})
(def object-fns   '{"$type" bi/$type, "$lookup" bi/$lookup, "$merge" bi/$merge, "$assert" bi/$assert, "$sift" bi/$sift, "$error" bi/$error,
                    "$each" bi/$each, "$keys" bi/$keys, "$spread" bi/$spread})
(def datetime-fns '{"$fromMillis" bi/$fromMillis, "$millis" bi/$millis, "$now" bi/$now, "$toMillis" bi/$toMillis})
(def higher-fns   '{"$filter" bi/$filter, "$map" bi/$map, "$reduce" bi/$reduce, "$sift" bi/$sift, "$single" bi/$single})

;;; Non-JSONata functions
(def file-fns '{"$readFile" bi/$readFile, "$readSpreadsheet" bi/$readSpreadsheet})
(def mc-fns   '{"$MCaddSchema" bi/$MCaddSchema, "$MCaddSource" bi/$MCaddSource, "$MCaddTarget" bi/$MCaddTarget,
                "$MCgetSource" bi/$MCgetSource, "$MCgetTarget" bi/$MCgetTarget, "$MCnewContext" $MCnewContext})

(def builtin-fns (merge numeric-fns agg-fns boolean-fns array-fns string-fns object-fns datetime-fns higher-fns file-fns mc-fns))
(def builtin? (-> builtin-fns keys (into ["$$$" "$$" "$"]) set))
(def builtin-un-op #{\+, \- :not})

;;; Binary operators.
(def numeric-operators '{\% bi/%, \* bi/*, \+ bi/+, \- bi/-, \/ bi//}) ; :range is not one of these.
(def comparison-operators '{:<= <=, :>= >=, :!= not=, \< <, \= =, \> >, "in" bi/in})
(def boolean-operators '{:and and :or or})
(def string-operators '{\& bi/&})
(def other-operators '{\. bi/access, :thread bi/thread})
;;; ToDo Re: binary-op? see also http://docs.jsonata.org/other-operators; I'm not doing everything yet.
(def binary-op? (merge numeric-operators comparison-operators boolean-operators string-operators other-operators))

(def ^:private syntactic ; chars that are valid tokens in themselves.
  #{\[, \], \(, \), \{, \}, \=, \,, \., \:, \;, \*, \+, \/, \-, \<, \>, \%, \&, \\, \?})

(def ^:private long-syntactic ; chars that COULD start a multi-character syntactic elements.
  #{\<, \>, \=, \., \:, \/, \', \?, \~, \!}) ; Don't put eol-comment (//) here. \/ is for regex vs divide.

(defrecord JaJvar [jvar-name special?])
(defrecord JaQvar [qvar-name])
(defrecord JaField [field-name]) ; Used for fields (e.g. the a in $.a, and function params
(defrecord JaTripleRole [role-name])
(defrecord JaEOLcomment [text])

;;; (regex-from-string "/abc\\/.*/")
(defn regex-from-string
  "Argument starts a JS-like regex.
   Return a map containing the regex :tkn (java.util.regex.Pattern) and the :raw text"
  [st]
  (assert (str/starts-with? st "/"))
  (let [in-len (count st)
        raw (loop [cnt 1
                   in (subs st 1)
                   done? false
                   out "/"]
              (cond done? out
                    (> cnt in-len) (throw (ex-info "Expected regex terminating /" {:string st}))
                    :else (recur (inc cnt)
                                 (if (str/starts-with? in "\\") (subs in 2) (subs in 1))
                                 (str/starts-with? in "/")
                                 (if (str/starts-with? in "\\") (str out "\\" (subs in 1 2)) (str out (subs in 0 1))))))]
    {:raw raw :tkn (re-pattern (subs raw 1 (-> raw count dec)))}))

(defn regex-or-divide
  "Return as :tkn either a Clojure regex or a /. Uses a heuristic,
   specifically does a closing '/' come before a space."
  [st]
  (let [s (subs st 1)
        slash-pos (str/index-of s \/)
        space-pos (str/index-of s " ")]
    (if (and slash-pos (or (not space-pos) (< slash-pos space-pos)))
      (regex-from-string st)
      {:raw "/" :tkn \/})))

(defn single-quoted-string
  "Return a token map for a single-quoted string. Note that for double-quoted strings,
   you get this for free from the Clojure reader."
  [s]
  (loop [chars (rest s)
         raw "'"
         res ""]
    (cond (empty? chars) (throw (ex-info "unbalanced single-quoted string:" {:input s}))
          (= \' (first chars)) {:raw (str raw \') :tkn res}
          (and (= \\ (first chars)) (= \' (second chars)))
          (recur (-> chars rest rest)
                 (str raw "\\'")
                 (str res "'"))
          :else (recur (rest chars)
                       (str raw (first chars))
                       (str res (first chars))))))
(defn read-qvar
  "read a query var"
  [st]
  (let [s (-> st str/split-lines first)]
    (if-let [[_ matched] (re-matches #"(\?[a-z,A-Z][a-zA-Z0-9\-\_]*).*" s)]
      {:raw matched :tkn (->JaQvar matched)}
      (throw (ex-info "String does not start a legal query variable:" {:string s})))))

(defn read-triple-role
  "read a triple role"
  [st]
  (let [s (-> st str/split-lines first)]
    ;; ToDo: Only one '/' allowed!
    (if-let [[_ matched] (re-matches #"(\:[a-zA-Z][a-zA-Z0-9/\-\_]*).*" s)]
      (if (or (> (-> (for [x matched :when (= x  \/)] x) count) 1)
              (= \/ (nth matched (-> matched count dec))))
        (throw (ex-info "String does not start a legal triple role:" {:string s}))
        {:raw matched :tkn (->JaTripleRole (read-string matched))})
      (throw (ex-info "String does not start a legal triple role:" {:string st})))))

;;; ToDo multi-line comment (e.g. /* ... */ would go in here, sort of.
(defn read-long-syntactic
  "Return a map containing a :tkn and :raw string for 'long syntactic' lexemes,
   which include arbitray query vars and roles too."
  [st ws]
  (let [len (count st)
        c0  (nth st 0)
        c1  (and (> len 1) (nth st 1))]
    (when-let [result (cond (and (= c0 \/) (= c1 \/)) {:raw "//" :tkn :eol-comment}
                            (= c0 \/) (regex-or-divide st)
                            (= c0 \') (single-quoted-string st)
                            (and (= c0 \?) (re-matches #"[a-zA-Z]" (str c1))) (read-qvar st),
                            (and (= c0 \:) (re-matches #"[a-zA-Z]" (str c1))) (read-triple-role st),
                            (and (= c0 \:) (= c1 \=)) {:raw ":=" :tkn :binding},
                            (and (= c0 \<) (= c1 \=)) {:raw "<=" :tkn :<=},
                            (and (= c0 \>) (= c1 \=)) {:raw ">=" :tkn :>=},
                            (and (= c0 \=) (= c1 \=)) {:raw "==" :tkn :==},
                            (and (= c0 \.) (= c1 \.)) {:raw ".." :tkn :range},
                            (and (= c0 \!) (= c1 \=)) {:raw "!=" :tkn :!=},
                            (and (= c0 \~) (= c1 \>)) {:raw "~>" :tkn :thread})]
      (assoc result :ws ws))))

(defn position-break
  "Return the first position in s containing a syntactic character or ws.
   Return nil if it contains none. The purpose is tokenize correctly things like 'a*b'
   where there is no intervening stuff. That said, 'a ?b : c' isn't a valid conditional
   expression owing to qvars."
  [s]
  (let [len (count s)]
    (loop [n 0]
      (let [c (get s n)]
        (cond
          (= len n) nil
          (long-syntactic c) n ; ToDo swapped with syntactic
          (syntactic c) n
          (#{\space \tab \newline} c) n
          :else (recur (inc n)))))))

(defn whitesp
  "Evaluates to whitespace at head of string or empty string if none."
  [s] ; https://stackoverflow.com/questions/15020669/clojure-multiline-regular-expression
  (if s (or (nth (re-matches #"(?s)(\s+).*$" s) 1) "") ""))

(defn get-more
  "Update :string-block and :line-seq by getting more lines from the line-seq lazy seq."
  [pstate]
  (as-> pstate ?ps
    (assoc  ?ps :string-block (->> ?ps :line-seq (take block-size) (map #(str % "\n")) (apply str)))
    (update ?ps :line-seq #(drop block-size %))))

(def *debugging-tokenizer?* false)

;;; ToDo:  This is going to need work for multi-line tokens such as comments and strings.
;;; https://www.regular-expressions.info/modifiers.html (?s) allows  .* to match all characters including line breaks.
(defn one-token-from-string
  "Return a map with keys :ws, :raw and :tkn from the front of the argument string."
  [string-block line] ; line is just or error reporting.
  (let [ws (whitesp string-block)
        s (subs string-block (count ws))
        c (first s)
        result
        (if (empty? s)
          {:ws ws :raw "" :tkn ::end-of-block},  ; Lazily pulling lines from line-seq; need more.
          (or  (and (empty? s) {:ws ws :raw "" :tkn ::eof})                   ; EOF
               (when-let [[_ cm] (re-matches #"(?s)(\/\/[^\n]*).*" s)]        ; EOL comment
                 {:ws ws :raw cm :tkn (->JaEOLcomment cm)})
               (and (long-syntactic c) (read-long-syntactic s ws))         ; /regex-pattern/ ++, <=, == etc.
               (and (syntactic c) {:ws ws :raw (str c) :tkn c})            ; literal syntactic char.
               (when-let [[_ num] (re-matches #"(?s)(\d+(\.\d+(e[+-]?\d+)?)?).*" s)]
                 {:ws ws :raw num :tkn (read-string num)}),                   ; number
               (when-let [[_ st] (re-matches #"(?s)(\"[^\"]*\").*" s)]        ; string literal
                 {:ws ws :raw st :tkn (read-string st)})
               (let [pos (position-break s)
                     word (subs s 0 (or pos (count s)))]
                 (or ; We don't check for "builtin-fns"; as tokens they are just jvars.
                  (and (keywords word)    {:ws ws :raw word :tkn (keyword word)})
                  (when-let [[_ id] (re-matches #"^([a-zA-Z0-9\_]+).*" word)]              ; field.
                    {:ws ws :raw id :tkn (->JaField id)})
                  ;; ToDo: Handle $ and $$ separately! (Drop $$$ ?)
                  (when-let [[_ id] (re-matches #"^(\$[a-zA-Z][A-Za-z0-9\_]*).*" word)]    ; jvar
                    {:ws ws :raw id :tkn (map->JaJvar {:jvar-name id})})
                  (when-let [[_ id] (re-matches #"^(\${1,3}).*" word)]                     ; $, $$, $$$.
                    {:ws ws :raw id :tkn (map->JaJvar {:jvar-name id :special? true})})
                  (when-let [[_ id] (re-matches #"^(:[a-zA-Z][a-zA-Z0-9\-\_]*).*" word)]   ; triple role
                    {:ws ws :raw id :tkn (->JaTripleRole id)})))
               (throw (ex-info "Char starts no known token: " {:raw c :line line}))))]
    (when *debugging-tokenizer?*
      (cl-format *out* "~%***result = ~S string strg = ~S" result string-block))
    result))

(defn tokens-from-string
  "Return pstate with :tokens and :string-block updated as the effect of tokenizing
   :string-block into :tokens."
  [pstate]
  (loop [ps pstate
         col 1]
    (let [lex (one-token-from-string (:string-block ps) (:cursor ps)) ; Returns a map with keys :ws :raw and :tkn.
          new-lines (->> lex :ws (re-seq #"\n") count) ; :ws is in front of token.
          col (if (> new-lines 0)
                (- (count (:ws lex)) (str/last-index-of (:ws lex) "\n"))
                (+ (count (:ws lex)) col))
          tkn {:tkn (:tkn lex) :line (+ (:cursor ps) new-lines) :col col}]
      (as-> ps ?ps
        (update ?ps :string-block #(subs % (+ (count (:raw lex)) (count (:ws lex)))))
        (update ?ps :cursor #(+ % new-lines))
        (if (= ::end-of-block (:tkn lex))
          ?ps
          (recur
           (if (instance? JaEOLcomment (:tkn lex))
             (update ?ps :comments conj tkn)
             (update ?ps :tokens   conj tkn))
           (+ (-> lex :raw count) col)))))))

(defn tokenize
  "Update :tokens and :line-seq. A token is a map with keys :tkn, :line :col."
  [pstate]
  (let [ps (get-more pstate)]        ; charges up :string-block...
    (if (-> ps :string-block empty?) ; ...or not, if done.
      (-> ps
          (assoc  :end-of-file? true) ;
          (update :tokens conj {:tkn ::eof}))
      (tokens-from-string ps))))

(defn refresh-tokens
  "Add more :tokens if :tokens is empty or count is < min-tkn."
  ([pstate] (refresh-tokens pstate nil))
  ([pstate min-tkn]
   (loop [ps pstate]
     (let [cnt (-> ps :tokens count)]
       (cond (:end-of-file? ps)                     ps,
             (and min-tkn (>= cnt min-tkn))         ps,
             (and (not min-tkn) (not (zero? cnt)))  ps,
             :else (recur (tokenize ps)))))))

;;; ============ Parser Utilities ============================================================
(defn line-msg
  "Return a string 'Line <n>: ' or 'Line <n>: <msg', depending on args."
  ([pstate] (line-msg pstate ""))
  ([pstate msg & args]
   (if (not-empty args)
     (cl-format nil "Line ~A: ~A ~{~A~^, ~}" (-> pstate :tokens first :line) msg args)
     (cl-format nil "Line ~A: ~A"            (-> pstate :tokens first :line) msg))))

(defn look
  "Sets a value in the :look map of pstate and might do some tokenizing in the process.
   n = 1 is one past :head, :tokens[0].
   Note the the value of (:look pstate) will be wrong if the call isn't 'fresh'."
  [pstate n] ; 2 of 3, refreshing tokens.
  (let [ps (refresh-tokens pstate n) ; no-op if (-> ps :tokens count) >= n.
        tokens (:tokens ps)
        cnt (count tokens)]
    (assoc-in ps [:look n] (if (> n cnt) ::eof (-> tokens (nth (dec n)) :tkn)))))

(defn ps-throw
  "A special throw to eliminate to clean up line-seq and "
  [pstate msg data]
  (as-> pstate ?ps
    (dissoc ?ps :line-seq) ; So REPL won't freak out over the reader being closed...
    (throw (ex-info (line-msg ?ps msg) data))))

(defn ps-assert
  "A special s/assert that does ps-throw on an error."
  [ps]
  (try (s/assert ::ps ps)
       (catch Exception e
         (ps-throw ps (str "pstate is invalid: " (.getMessage e))
                   {:tags (:tags ps)
                    :head (:head ps)
                    :tokens (:tokens ps)}))))

(defn match-head
  "Return true if token matches test, which is a string, character, fn or regex."
  [pstate test]
  (let [head (:head pstate)]
    (cond (= test ::pass) true
          (= test head) true
          (map? test) (test head)
          (set? test) (test head)
          (fn? test) (test head)
          (instance? java.util.regex.Pattern test) (re-matches test head)
          :else
          (ps-throw pstate (cl-format nil "Expected a ~A token." test)
                    {:test test :got head :tags (:tags pstate)}))))

(defn eat-token
  "Move head of :tokens to :head ('consuming' the old :head) With 2 args, test :head first."
  ([pstate] (eat-token pstate ::pass))
  ([pstate test]
   (when *debugging?* (cl-format *out* "~AEAT ~A  (type ~A)~%"
                                 (util/nspaces (* 3 (-> pstate :tags count dec)))
                                 (pp/write (:head pstate) :readably false :stream nil)
                                 (-> pstate :head util/class-name name)))
   (match-head pstate test)
   ;; The actual work of eating a token
   (let [ps1 (if (-> pstate :tokens empty?) (refresh-tokens pstate) pstate) ; 3 of 3, refreshing tokens.
         next-up (-> ps1 :tokens first)]
     #_(cl-format *out* "~%Eating ~A, next-up = ~A" (:head ps1) (:tkn next-up))
     #_(when (and (= (:head ps1) \.) (= (:tkn next-up) \.))
       (ps-throw ps1 "HERE! (eat-token)" {:head (:head ps1) :next-up next-up :tags (:tags ps1) :tokens (:tokens ps1)}))
     (as-> ps1 ?ps
       (assoc ?ps :head (if next-up (:tkn next-up) ::eof)) ; One of two places :head is set; the other is make-pstate.
       (assoc ?ps :tokens (-> ?ps :tokens rest vec))       ; 2 of 3, setting :tokens.
       (ps-assert ?ps)))))

(defn token-vec [ps] (into (-> ps :head vector) (mapv :tkn (:tokens ps))))

(def balanced-map "What balances with the opening syntax?" { \{ \}, \( \), \[ \] :2d-array-open :2d-array-close})
(def balanced-inv (set/map-invert balanced-map))

(defn find-token
  "Return position if tkn is found within the item (before semicolon)."
  [tvec tkn & {:keys [stop-tokens stop-pos]}]
  (when (not-empty tvec)
    (let [stop-pos (or stop-pos (->> (map #(.indexOf tvec %) stop-tokens) (apply max)))
          stop-pos (if (pos? stop-pos) stop-pos (count tvec)) ; In testing, might not have full item; not stop.
          tkn-pos  (.indexOf tvec tkn)]
      (cond (== tkn-pos  -1) nil,
            (and (pos? stop-pos) (< stop-pos tkn-pos)) nil,
            :else tkn-pos))))

;;; (find-balanced-pos [ \{, \{, :foo, \}, \}, ]     \}) ==> 4
(defn find-balanced-pos
  "Return the position of a balanced instance of the argument token (a close-syntax token).
   Thus if tvec is [ \\{, \\{, foo, \\}, \\}, ] it is 4, not 3. Return nil if none."
  [tvec close-tkn]
  (when-let [open-tkn (balanced-inv close-tkn)]
    (assert (= open-tkn (first tvec)))
    (loop [cnt 1
           pos 0
           tvec (rest tvec)]
      (cond (== 0 cnt) pos
            (empty? tvec) nil
            :else
            (let [tkn (first tvec)]
              (recur (cond (= tkn open-tkn)  (inc cnt)
                           (= tkn close-tkn) (dec cnt)
                           :else cnt)
                     (inc pos)
                     (rest tvec)))))))

(defn balanced?
  "Return true if, before position POS there is a closing syntax character for each
   argument TKN opening syntax character."
  [tvec open-tkn pos]
  (let [close-tkn (get balanced-map open-tkn)]
    (== 0 (reduce (fn [cnt tkn]
                    (cond (= tkn open-tkn) (inc cnt)
                          (= tkn close-tkn) (dec cnt)
                          :else cnt))
                  0
                  (subvec tvec 0 pos)))))

(defmacro defparse [tag [pstate & keys-form] & body]
  `(defmethod parse ~tag [~'tag ~pstate ~@(or keys-form '(& _))]
     (when *debugging?* (cl-format *out* "~%~A==> ~A" (util/nspaces (* 3 (-> ~pstate :tags count))) ~tag))
     (as-> ~pstate ~pstate
       (update ~pstate :call-count inc) ; ToDo: (maybe) There was a max calls check here based on the token count,
       (update ~pstate :tags conj ~tag) ; but with the buffered reading enhancement, we don't have token count.
       (update ~pstate :local #(into [{:locals-for ~tag}] %))
       (let [res# (do ~@body)] (if (seq? res#) (doall res#) res#))
       (cond-> ~pstate (-> ~pstate :tags not-empty) (update :tags pop))
       (update ~pstate :local #(vec (rest %)))
       (do (when *debugging?* (cl-format *out* "~%~A<-- ~A   ~S"
                                         (util/nspaces (* 3 (-> ~pstate :tags count)))
                                         ~tag
                                         (:result ~pstate)))
           (ps-assert ~pstate)))))

;;; Abbreviated for simple forms such as builtins.
(defmacro defparse-auto [tag test]
  `(defparse ~tag
     [pstate#]
     (-> pstate#
         (assoc :result (:head pstate#))
         (eat-token pstate# ~test))))

;;; This is an abstraction to protect :result while something else is swapped in.
;;; The 'from' is what key of ps to take from (defaults to :result).
(defmacro store [ps key & [from]]
  `(let [ps# ~ps
         key# ~key]
     (assoc-in ps# [:local 0 key#]
               (~(or from :result) ps#))))

;;; ...and this is for getting the value back.
(defmacro recall [ps tag]
  `(let [ps# ~ps]
     (-> ~ps :local first ~tag)))

(defn parse-dispatch [tag & _] tag)

(defmulti parse #'parse-dispatch)

(defn make-pstate
  "Make a parse state map and start tokenizing."
  [reader]
  (as-> {:head nil ; In this order for easy debugging.
         :tags []
         :local []
         :look {}
         :tokens []
         :reader reader
         :line-seq (line-seq reader)
         :call-count 0
         :cursor 1 ; what line you are on; used in tokenizing.
         :comments []} ?ps
    (refresh-tokens ?ps) ; 1 of 3, refreshing tokens.
    (assoc ?ps :head (-> ?ps :tokens first :tkn)) ; One of two places :head is set; the other is eat-token.
    (assoc ?ps :tokens (-> ?ps :tokens rest vec)) ; 1 of 3, setting :tokens.
    (ps-assert ?ps)))

(defn parse-string
  "Toplevel parsing function.
   NB: This function is typically used for debugging with a literal string argument.
   If the text is intended to have JS escape, \\, in it, it has to be escaped!"
  ([str] (parse-string :ptag/code-block str))
  ([tag str]
   (let [pstate (->> str tokenize make-pstate (parse tag))]
     (if (not= (:head pstate) ::eof)
       (throw (ex-info "Tokens remain" {:pstate pstate}))
        #_(do (when *debugging?*
             (log/error (cl-format nil "~2%*** Tokens remain. pstate=~A ~%" pstate)))
           pstate)
       pstate))))

(defn esc-esc
  "Escape characters in input are intended for the JS-like target language, not Clojure.
   Thus this escapes them. (It duplicates backslash.)
   I don't think this is what re-quote-replacement does!"
  [in-str]
  (loop [input in-str
         output ""]
    (if (empty? input) output
        (recur
         (subs input 1)
         (if (str/starts-with? input "\\")
           (str output "\\")
           (str output (subs input 0 1)))))))

(defn parse-file
  "Parse a whole file given a filename string."
  [filename]
  (parse-string :ptag/code-block (-> filename slurp esc-esc)))

(defn parse-ok?
  "Return true if the string parses okay."
  [tag text]
  (as-> (parse-string tag text) ?pstate
    (and (= ::eof (:head ?pstate))
         (or (not (contains? (s/registry) tag))
             (s/valid? tag (:result ?pstate))))))

(defn parse-list
  "Does parse parametrically for <open-char> [ <item> <char-sep>... ] <close-char>"
  ([pstate char-open char-close char-sep]
   (parse-list pstate char-open char-close char-sep :ptag/exp))
  ([pstate char-open char-close char-sep parse-tag]
   (when *debugging?*
     (cl-format *out* "~%>>>>>>>>>>>>>> parse-list (~A) >>>>>>>>>>>>>>>>>>" parse-tag))
   (let [final-ps
         (as-> pstate ?ps
           (eat-token ?ps char-open)
           (assoc-in ?ps [:local 0 :items] [])
           (loop [ps ?ps]
             (cond
               (= ::eof (:head ps))
               (ps-throw ps "parsing a list" {:tag parse-tag}),
               (= char-close (:head ps))
               (as-> ps ?ps1
                 (eat-token ?ps1)
                 (assoc ?ps1 :result (recall ?ps1 :items))),
               :else
               (as-> ps ?ps1
                 (parse parse-tag ?ps1)
                 (update-in ?ps1 [:local 0 :items] conj (:result ?ps1))
                 (recur (cond-> ?ps1 (= char-sep (:head ?ps1)) (eat-token char-sep)))))))]
     (when *debugging?*
       (println "\nCollected" (:result final-ps))
       (println "\n<<<<<<<<<<<<<<<<<<<<< parse-list <<<<<<<<<<<<<<<<<<<<<<<"))
     final-ps)))

;;; ToDo: Have a separate variable to turn off debugging on the auto things.
(defn parse-list-terminated
  "Does parse parametrically for '[ <item> ','... ] <terminator>'. Does not eat terminator."
  [pstate & {:keys [term-fn sep-fn parse-tag] :or {sep-fn #(= \; %)
                                                   term-fn #(= \} %)
                                                   parse-tag :ptag/exp}}]
  (when *debugging?*
    (cl-format *out* "~%>>>>>>>>>>>>>> parse-list-terminated (~A) >>>>>>>>>>>>>>>>>>" parse-tag))
  (let [final-ps
        (as-> pstate ?ps
          (assoc-in ?ps [:local 0 :items] [])
          (loop [ps ?ps]
            (cond
              (= ::eof (:head ps)) (ps-throw ps "parsing a terminated list" {:tag parse-tag}),
              (term-fn (:head ps)) (assoc ps :result (recall ps :items)),
              :else
              (as-> ps ?ps
                (parse parse-tag ?ps)
                (update-in ?ps [:local 0 :items] conj (:result ?ps))
                (recur (cond-> ?ps (sep-fn (:head ?ps)) (eat-token #{\,\;})))))))]
    (when *debugging?*
      (println "\nCollected" (:result final-ps))
      (println "\n<<<<<<<<<<<<<<<<< parse-list-terminated <<<<<<<<<<<<<<<<"))
    final-ps))

;;; ToDo I'm not sure I use any of these autos!
;;; <builtin-num-bin-op> ::= + | - | * | / | div | mod
(def builtin-num-bin-op #{\+ \- \* \/ \% :div :mod})
(defparse-auto :ptag/builtin-num-bin-op builtin-num-bin-op)

;;; :range is NOT a bin-op!
;;;  <builtin-bin-op> ::= . | & | < | > | <= | >= | == | = | != | and | or
(def builtin-bin-op
  (into #{\. \< \> :<= :>= :== \= :not= \& :and :or}
        builtin-num-bin-op))
(defparse-auto :ptag/builtin-bin-op builtin-bin-op)

;;; <builtin-un-op> ::= "not" | "+" | "-"
(defparse-auto :ptag/builtin-un-op builtin-un-op)

;;; <builtin-op> ::= <builtin-bin-op> | <builtin-un-op>
(def builtin-op (set/union builtin-bin-op builtin-un-op))
(defparse-auto :ptag/builtin-op builtin-op)
(defparse-auto :ptag/builtin-fn builtin-fns)

(defn jvar? [x] (instance? JaJvar x))
(defn qvar? [x] (instance? JaQvar x))
(defn triple-role? [x] (instance? JaTripleRole x))
(defn field? [x] (instance? JaField x))

(s/def ::ps  (s/keys :req-un [::tokens ::head]))
(s/def ::tokens (s/and vector? (s/coll-of ::token)))
(s/def ::token  (s/and map? #(contains? % :tkn) #(-> % :tkn nil? not)))
(s/def ::head #(not (nil? %)))

;;;=============================== Grammar ===============================
(s/def ::CodeBlock (s/keys :req-un [::body]))
(defrecord JaCodeBlock [body])

;;; ToDo: The block can have context (in the bi/access sense) as shown on that page "Invoice.(.....)"
;;; That means, at least, that <code-block> is an expression, (but then so is (a + b)).
;;; When I'm done with this, therefore, it will be an expression language; there is nothing else.
;;; See http://docs.jsonata.org/programming about "Programming Constructs" variable binding
;;; ToDo: (maybe) Mixed <jvar-decl> and <exp> implies nested lets. Currently I'm assume only the last one is <exp>

;;; <code-block> := '(' ( <jvar-decl> | <exp> )* ')'
(defparse :ptag/code-block
  [ps]
  (as-> ps ?ps
    (eat-token ?ps \()
    (loop [ps ?ps
           exprs []]
      (cond (= (:head ps) \))
            (as-> ps ?ps1
              (assoc ?ps1 :result (->JaCodeBlock exprs))
              (eat-token ?ps1 \))),
            (= (:head ps) ::eof)  (ps-throw  ps "Runaway code block" {}),
            :else
            (let [ps (as-> ps ?ps1
                       (look ?ps1 1)
                       (if (= :binding (-> ?ps1 :look (get 1)))
                         (parse :ptag/jvar-decl ?ps1)
                         (parse :ptag/exp ?ps1)))]
              (recur ps (conj exprs (:result ps))))))))

;;; id-type-pair ::= <jvar> ':=' <exp> ';'
(defrecord JaJvarDecl [var init-val])
(defparse :ptag/jvar-decl
  [pstate]
  (as-> pstate ?ps
    (if (-> ?ps :head builtin-fns)
       (ps-throw ?ps "Attempting to rebind a built-in function."
                 {:built-in-fn (:head ?ps)})
       ?ps)
    (parse :ptag/jvar ?ps)
    (store ?ps :jvar)
    (eat-token ?ps :binding)
    (parse :ptag/exp ?ps)
    (store ?ps :init-val)
    (eat-token ?ps \;)
    (assoc ?ps :result (->JaJvarDecl (recall ?ps :jvar)
                                     (recall ?ps :init-val)))))

;;; This should return a <call-exp> at the end of path.
(defparse :ptag/map-call
  [pstate]
  (parse :ptag/exp pstate))

(defparse :ptag/map-comprehension
  [pstate]
  (parse :ptag/list-comprehension pstate))

;;;--------------------- exp ----------------------------------------------------------
(defn literal? [tkn]
  (or (string? tkn)
      (number? tkn)
      (#{:true :false} tkn)
      (= java.util.regex.Pattern (type tkn))))

(defmacro continue-mac
  "Abbreviation of tedious code."
  [ps tags]
  `(or ~@(map (fn [tag]
                `(try (let [ps-done# (-> (parse ~tag ~ps) (look 1))]
                        {:operand-tag ~tag
                         :next-tkns [(:head ps-done#) (-> ps-done# :look (get 1))]})
                      (catch Exception _e# nil)))
              tags)))

(defn operand-exp?
  "Return one of [:ptag/field :ptag/jvar :ptag/literal :ptag/fn-call :ptag/delimited] if the front
   of the token stack is something that could be continued as a binary expression followed by a binary operator.
   This is called by the <exp> grammar rule, so you can't use it to anticipate something that starts with an
   expression such as '<exp> ? <exp> : <exp>'; that would result in a non-consuming loop."
  [ps]
  (binding [*debugging?* false] ; I don't think we need to see this!
    (continue-mac ps [:ptag/field :ptag/jvar :ptag/literal :ptag/fn-call :ptag/unary-op-exp])))

(defparse :ptag/field
  [ps]
  (as-> ps ?ps
    (assoc ?ps :result (:head ?ps))
    (eat-token ?ps field?)))

(defparse :ptag/param
  [ps]
  (as-> ps ?ps
    (assoc ?ps :result (:head ?ps))
    (eat-token ?ps jvar?)))

(defn delimited-next?
  "Check 'next-tkns' from operand-exp?. Return true if the tokens
   start a delimited-exp (as opposed to a fn-call, etc.)."
  [{:keys [next-tkns]}]
  (or (#{[\., \{], [\., \(]} next-tkns)
      (= \[ (first next-tkns))))

(defn binary-next?
  "Check 'next-tkns' from operand-exp?. Return true if the tokens
   start a delimited-exp (as opposed to a fn-call, etc.)."
  [{:keys [next-tkns]}]
  (-> next-tkns first binary-op?))

;;; ToDo: Rethink use of in-binary?
;;; <exp> ::=  <base-exp> ( '?' <conditional-tail> | <exp> )?
(defrecord JaBinOpExp [exp1 bin-op exp2])
(defparse :ptag/exp
  [ps & {:keys [in-binary?]}]
  (let [base-ps (parse :ptag/base-exp ps :in-binary? in-binary?)]
    (cond (and (= \? (:head base-ps)) (not in-binary?))
          (parse :ptag/conditional-tail base-ps :predicate (:result base-ps)),
          (-> base-ps :head binary-op?) ; (and (not in-binary?)) is causing early termination.
          (as-> base-ps ?ps
            (store ?ps :operator :head)
            (eat-token ?ps)
            (parse :ptag/exp ?ps :in-binary? true)
            (assoc ?ps :result (->JaBinOpExp (:result base-ps) (recall ?ps :operator) (:result ?ps)))),
          :else base-ps)))

;;; <base-exp> ::= <delimited-exp> | <binary-exp> | (<builtin-un-op> <exp>) | <construct-def> | <fn-call> | <literal> | <field> | <jvar> | <qvar>
(defparse :ptag/base-exp
  [ps & {:keys [in-binary?]}]
  (let [tkn   (:head ps)
        operand-look (operand-exp? ps)
        ps (look ps 1)
        tkn2 (-> ps :look (get 1))]
    (cond (and (not in-binary?) (#{\{ \[ \(} tkn)) (parse :ptag/delimited-exp ps),
          (delimited-next? operand-look)                              ; <delimited-exp>
          (parse :ptag/delimited-exp ps :operand-info operand-look),
          (and (not in-binary?) (binary-next? operand-look))          ; <binary-exp>
          (parse :ptag/binary-exp ps :operand-info operand-look),
          (builtin-un-op tkn) (parse :ptag/unary-op-exp ps),          ; <unary-op-exp>
          (#{:function :query :enforce} tkn)                          ; <construct-def>
          (parse :ptag/construct-def ps)
          (and (or (builtin-fns tkn) (jvar? tkn)) (= \( tkn2))        ; <fn-call>
          (parse :ptag/fn-call ps),
          (or (literal? tkn) (#{\[ \{} tkn))                          ; <literal>
          (parse :ptag/literal ps),
          (or (jvar? tkn) (qvar? tkn) (field? tkn))                   ; <field>, <jvar>, or <qvar>
          (as-> ps ?ps
            (assoc ?ps :result tkn)
            (eat-token ?ps)),
          :else
          (ps-throw ps "Expected a unary-op, (, {, [, fn-call, literal, $id, ?id, or path element."
                          {:got tkn}))))

;;; <binary-exp>  ::= <operand-exp> <binary-op> <exp>
;;; <operand-exp> ::= <field> | <var> | literal | <fn-call> | <delimited-exp> | <unary-op-exp>
(defparse :ptag/binary-exp
  [ps & {:keys [operand-info]}]
  (as-> ps ?ps
    (parse (:operand-tag operand-info) ?ps) ; Parametric tag!
    (store ?ps :exp1)
    (store ?ps :op :head)
    (eat-token ?ps binary-op?)
    (parse :ptag/exp ?ps #_#_:in-binary? true) ; in-binary? so that the caller can consume \?, and other reasons.
    (assoc ?ps :result (->JaBinOpExp (recall ?ps :exp1) (recall ?ps :op) (:result ?ps)))))

;;; ToDo: This one needs some explaining!
;;; <delimited-exp> ::= <operand-exp> ( ('.' <paren-delimited-exp>) | ('.' <curly-delimited-exp>) | <square-delimited-exp> )
(defparse :ptag/delimited-exp
  [ps & {:keys [operand-info]}]
  (let [{:keys [operand-tag next-tkns]} operand-info
        tkn (:head ps)]
    (if (not operand-info) ; primary?
      (cond (= tkn \() (parse :ptag/paren-delimited-exp ps)
            (= tkn \{) (parse :ptag/curly-delimited-exp ps)
            (= tkn \[) (parse :ptag/square-delimited-exp ps))
      (as-> ps ?ps
        (parse operand-tag ?ps) ; Parametric tag!
        (store ?ps :operand)    ; Note that this ISN'T THE WHOLE OPERAND...See rewriting.
        (cond (= [\. \(] next-tkns) (as-> ?ps ?ps1
                                      (eat-token ?ps1 \.)
                                      (parse :ptag/paren-delimited-exp ?ps1)),
              (= [\. \{] next-tkns) (as-> ?ps ?ps1
                                      (eat-token ?ps1 \.)
                                      (parse :ptag/curly-delimited-exp ?ps1)),
              (= \[ (first next-tkns)) (parse :ptag/square-delimited-exp ?ps),
              :else (ps-throw ps "Expected a delimited-exp." {:operand-info operand-info}))
        (assoc-in ?ps [:result :operand] (recall ?ps :operand))))))

(defrecord JaParenDelimitedExp [exp operand])
(defparse :ptag/paren-delimited-exp
  [ps]
  (as-> ps ?ps
    (eat-token ?ps \()
    (parse :ptag/exp ?ps)
    (eat-token ?ps \))
    (assoc ?ps :result (map->JaParenDelimitedExp {:exp (:result ?ps)}))))

;;; <square-delimited-exp> ::=  <range-exp> |
(defrecord JaRange [start stop])
(defrecord JaSquareDelimitedExp [operand exp])
(defparse :ptag/square-delimited-exp
  [ps]
  (let [tvec (token-vec ps)
        close-pos (when (= (first tvec) \[) (find-balanced-pos tvec \]))
        range-pos (when close-pos (find-token tvec :range :stop-pos close-pos))]
    (if (and close-pos range-pos (< range-pos close-pos)) ; ToDo Heuristic! (Replace with try?)
      (parse :ptag/range-exp ps)
      (as-> ps ?ps
        (parse-list ?ps \[ \] \, :ptag/exp)
        (assoc ?ps :result (map->JaSquareDelimitedExp {:exp (:result ?ps)}))))))

  ;;; <array> '[' <exp>? ( ',' <exp>)? ']'

;;; The JSONata range expression is a bit weird; it is not a binary op but a construct with [ and ].
;;; <range-exp> ::= '[' <exp> '..' <exp> ']'
(defrecord JaRangeExp [start stop])
(defparse :ptag/range-exp
  [ps]
  (as-> ps ?ps
    (eat-token ?ps \[)
    (parse :ptag/exp ?ps)
    (store ?ps :start)
    (eat-token ?ps :range)
    (parse :ptag/exp ?ps)
    (store ?ps :stop)
    (eat-token ?ps \])
    (assoc ?ps :result (->JaRangeExp (recall ?ps :start) (recall ?ps :stop)))))

#_(defparse :ptag/array
  [ps]
  (as-> ps ?ps
    (parse-list ?ps \[ \] \, :ptag/exp)
    (assoc ?ps :result (->JaArray (:result ?ps)))))

;;; <obj> ::= "{" <map-pair> (',' <map-pair> )* "}"
(defrecord JaCurlyDelimitedExp [operand map-pairs])
(defparse :ptag/obj [ps] (parse :ptag/curly-delimited-exp ps))
(defparse :ptag/curly-delimited-exp
  [ps]
  (as-> ps ?ps
    (parse-list ?ps \{ \} \, :ptag/map-pair)
    (assoc ?ps :result (map->JaCurlyDelimitedExp {:exp (:result ?ps)}))))

;;; <conditional-tail> ::=  <exp> ':' <exp>
(defrecord JaConditionalExp [predicate exp1 exp2])
(defparse :ptag/conditional-tail
  [ps & {:keys [predicate]}]
  (as-> ps ?ps
    (eat-token ?ps \?)
    (parse :ptag/exp ?ps)
    (store ?ps :then)
    (eat-token ?ps \:)
    (parse :ptag/exp ?ps)
    (assoc ?ps :result (->JaConditionalExp predicate
                                           (recall ?ps :then)
                                           (:result ?ps)))))

;;; <unary-exp> ::= <unary-operator> <exp>
(defrecord JaUniOpExp [uni-op exp])
(defparse :ptag/unary-op-exp
  [ps]
    (as-> ps ?ps
      (store ?ps :op :head)
      (eat-token ?ps builtin-un-op)
      (let [{:keys [operand-tag]} (operand-exp? ?ps)] ; ToDo was operand-exp-no-operator?
        (as-> ?ps ?ps1
        (parse operand-tag ?ps1)
        (assoc ?ps1 :result (->JaUniOpExp (recall ?ps1 :op) (:result ?ps1)))))))

;;; <map-pair> ::=  <string>" ":" <exp>
(defrecord JaMapPair [key val])
(defparse :ptag/map-pair
  [pstate]
  (as-> pstate ?ps
    (parse :ptag/string ?ps)
    (store ?ps :key)
    (eat-token ?ps \:)
    (parse :ptag/exp ?ps)
    (assoc ?ps :result (->JaMapPair
                        (recall ?ps :key)
                        (:result ?ps)))))

;;;----- 'atomic' expressions, these are useful for parse-list-terminated  etc. -------
(defparse :ptag/jvar
  [ps]
  (as-> ps ?ps
    (assoc ?ps :result (:head ?ps))
    (eat-token ?ps jvar?)))

(defparse :ptag/string
  [ps]
  (let [tkn (:head ps)]
    (if (string? tkn)
      (-> ps (assoc :result tkn) eat-token)
      (ps-throw ps "expected a string literal" {:got tkn}))))

;;; <literal> ::= string | number | 'true' | 'false' | regex | <obj> | <square-delimited-exp>
(defparse :ptag/literal
  [ps]
  (let [tkn (:head ps)]
    (cond (literal? tkn) (-> ps (assoc :result tkn) eat-token), ; :true and :false will be rewritten
          (= tkn \{)     (parse :ptag/obj ps),
          (= tkn \[)     (parse :ptag/square-delimited-exp ps),
          :else (ps-throw ps "expected a literal string, number, 'true', 'false' regex, obj, range, or array."
                                {:got tkn}))))

;;; fn-call ::=  <jvar> '(' <exp>? (',' <exp>)* ')'
(defrecord JaFnCall [fn-name args])
(defparse :ptag/fn-call
  [ps]
    (as-> ps ?ps
      (store ?ps :fn-name :head)
      (eat-token ?ps jvar?)
      (eat-token ?ps \()
      (parse-list-terminated ?ps :term-fn #(= % \)) :sep-fn #(= % \,))
      (store ?ps :args)
      (eat-token ?ps \))
      (assoc ?ps :result (->JaFnCall (-> ?ps (recall :fn-name) :jvar-name) (recall ?ps :args)))))

;;; <triples> ::= <triple>+
(defparse :ptag/triples
  [ps]
  (let [ps-one (parse :ptag/triple ps)]
    (loop [result (vector (:result ps-one))
           ps ps-one]
      (if (not= \[ (:head ps))
        (assoc ps :result result)
        (let [ps (parse :ptag/triple ps)]
          (recur (conj result (:result ps))
                 ps))))))

;;; <triple> :: '[' <QueryVar> <TripleRole> (<QueryVar> | <exp>) ']'
(defrecord JaTriple[ent rel val-exp])
(defparse :ptag/triple
  [ps]
  (as-> ps ?ps
    (eat-token ?ps \[)
    (store ?ps :ent :head)
    (eat-token ?ps qvar?)
    (store ?ps :role :head)
    (eat-token ?ps #(or (triple-role? %) (qvar? %)))
    (if (qvar? (:head ?ps))
      (as-> ?ps ?ps1
          (store ?ps1 :third :head)
          (eat-token ?ps1))
      (as-> ?ps ?ps1
          (parse :ptag/exp ?ps1)
          (store ?ps1 :third)))
    (eat-token ?ps \])
    (assoc ?ps :result (->JaTriple (recall ?ps :ent) (recall ?ps :role) (recall ?ps :third)))))

;;; <fn-def> ::= 'function' '(' <jvar>? [',' <jvar>]* ')' '{' <exp> '}'
(defrecord JaFnDef [vars body])
(defparse :ptag/fn-def
  [ps]
  (as-> ps ?ps
    (eat-token ?ps :function)
    (eat-token ?ps \()
    (parse-list-terminated ?ps :term-fn #{\)} :sep-fn #{\,} :parse-tag :ptag/jvar)
    (store ?ps :jvars)
    (eat-token ?ps \))
    (eat-token ?ps \{)
    (parse :ptag/exp ?ps)
    (store ?ps :body)
    (eat-token ?ps \})
    (assoc ?ps :result (->JaFnDef (recall ?ps :jvars) (recall ?ps :body)))))

;;; <query-def> ::= 'query '(' <jvar>? [',' <jvar>]* ')' '{' <triples> '}'
(defrecord JaQueryDef  [params triples])
(defparse :ptag/query-def
  [ps]
  (as-> ps ?ps
    (eat-token ?ps :query)
    (eat-token ?ps \()
    (parse-list-terminated ?ps :term-fn #{\)} :sep-fn #{\,} :parse-tag :ptag/jvar)
    (store ?ps :params)
    (eat-token ?ps \))
    (eat-token ?ps \{)
    (parse :ptag/triples ?ps)
    (eat-token ?ps \})
    (assoc ?ps :result (->JaQueryDef (recall ?ps :params) (:result ?ps)))))

;;; ToDo: These aren't jvars, probably want some sort of optional parameter syntax.
;;; <enforce-def> ::= 'enforce' '(' <jvar>? [',' <jvar>]* ')' '{' <exp> '}'
(defrecord JaEnforceDef [params body])
(defparse :ptag/enforce-def
  [ps]
  (as-> ps ?ps
    (eat-token ?ps :enforce)
    (eat-token ?ps \()
    (parse-list-terminated ?ps :term-fn #{\)} :sep-fn #{\,} :parse-tag :ptag/exp)
    (store ?ps :params)
    (eat-token ?ps \))
    (eat-token ?ps \{)
    (parse :ptag/exp ?ps)
    (store ?ps :body)
    (eat-token ?ps \})
    (assoc ?ps :result (->JaEnforceDef (recall ?ps :params) (recall ?ps :body)))))

(defrecord JaImmediateUse [def args])
;;; <construct-def> ::= ( <fn-def> | <query-def> | <enforce-def> )( '(' <exp>* ')' )?
(defparse :ptag/construct-def
  [ps]
  (let [ps (case (:head ps)
             :function  (parse :ptag/fn-def ps),         ; <fn-def>
             :query     (parse :ptag/query-def ps),      ; <query-def>
             :enforce   (parse :ptag/enforce-def ps))]   ; <enforce-def>
    (if (and (= \( (:head ps)) (#{JaFnDef JaQueryDef} (-> ps :result type)))
      ;; This part to wrap it in a JaImmediateUse
      (as-> ps ?ps
        (store ?ps :def)
        (eat-token ?ps)
        (parse-list-terminated ?ps :term-fn #{\)} :sep-fn #{\,} :parse-tag :ptag/exp)
        (store ?ps :args)
        (eat-token ?ps \))
        (assoc ?ps :result (->JaImmediateUse (recall ?ps :def) (recall ?ps :args))))
      ;; This if it is just a definition (which will be assigned to a $id).
      ps)))
