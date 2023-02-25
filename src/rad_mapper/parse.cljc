(ns rad-mapper.parse
  "Parse the JSONata-like message mapping language."
  (:require
   [clojure.pprint :as pp :refer [cl-format]]
   [clojure.repl :refer [doc]]
   [clojure.string :as str :refer [index-of]]
   [clojure.set    :as set]
   [clojure.spec.alpha :as s]
   [rad-mapper.util :as util]
   [taoensso.timbre :as log]
   [rad-mapper.parse-macros :refer [defparse *debugging?* parse parse-dispatch]])
#?(:cljs (:require-macros [rad-mapper.parse-macros :refer [defparse]])))

;;; from utils.cljc
(defn nspaces
  "Return a string of n spaces."
  [n]
  (reduce (fn [s _] (str s " ")) "" (range n)))

;;; The 'defparse' parsing functions pass around complete state.
;;; The lexer mostly produces maps where the :tkn is a string.
;;; The parser uses these :tkn "string things" to produce map grammar map structures where ":tk/things",
;;; and ":op/things" are created to match the corresponding "string things."
;;; binary-op? is an example map from string things to :op/things: {"*" :op/multiply,...}.
;;;
;;; The 'parse state' (AKA pstate) is a map with keys:
;;;   :result    - the parse structure from the most recent call to (parse :<some-rule-tag> pstate)
;;;   :tokens    - tokenized content that needs to be parsed into :model. First on this vector is also :tkn.
;;;   :tags      - a stack of tags indicating where in the grammar it is parsing (used for debugging)
;;;   :head      - current token, not yet consumed. It is also the first token on :tokens.
;;;   :line      - line in which token appears.
;;;   :col       - column where token starts.
;;;   :local     - temporarily stored parse content used later to form a complete grammar element.
;;;                It is a vector (stack) of maps. On entry, defparse pushes a new empty map on it;
;;;                on exit, it pops it. Macros store and recall push onto the top map of the stack.
;;;                For example use, see :ptag/MapSpec and parse-list.

;;; ToDo:
;;;   - Rethink the lexer/parser dichotomy. See Lezer, for example. My continue-tag stuff and regex is pretty bad!

(util/config-log (if *debugging?* :debug :info))

(def block-size "Number of lines to tokenize together. Light testing suggests 5 is about fastest." 5)

;;; N.B.: In these maps of grammar elements, make sure you use a character literal when the map key is a single character of text!
;;; ============ Tokenizer ===============================================================
(def keywords {"express" :tk/express
               "false" :tk/false
               "function" :tk/function
               "key" :tk/key
               "query" :tk/query
               "rule"  :tk/rule
               "true" :tk/true})
(def syntactic?
  "Chars that are valid tokens in themselves."
  #{\[, \], \(, \), \{, \}, \=, \,, \., \:, \;, \*, \+, \/, \-, \<, \>, \%, \&, \\, \? \`})

(def long-syntactic?
  "chars that COULD start a multi-character syntactic elements. These are checked before syntactic? thus overlap permitted."
  #{\<, \>, \=, \., \:, \/, \', \" \?, \~, \{, \| \!}) ; Don't put eol-comment (//) here. \/ is for regex vs divide.

(def other? #{\(, \), \{, \}, \[, \], \:, \,, \;})

(defn fn-maps [strs] (reduce (fn [r s] (assoc r s (symbol "bi" s))) {} strs))
;;; http://docs.jsonata.org/string-functions
(def string-fns
  (fn-maps ["$base64decode" "$base64encode" "$contains" "$decodeUrl" "$decodeUrlComponent" "$encodeUrl" "$encodeUrlComponent"
            "$eval" "$join" "$length" "$lowercase" "$match" "$pad" "$replace" "$split" "$string" "$substring" "$substringAfter"
            "$substringBefore" "$trim" "$uppercase"]))

(def numeric-fns
  (fn-maps ["$abs" "$ceil" "$floor" "$formatBase" "$formatInteger" "$formatNumber" "$number" "$parseInteger" "$power"
            "$random" "$round" "$sqrt"]))

(def agg-fns      (fn-maps ["$average" "$max" "$min" "$sum"]))
(def boolean-fns  (fn-maps ["$boolean" "$exists" "$not"]))
(def array-fns    (fn-maps ["$append" "$count" "$distinct" "$reverse" "$shuffle" "$sort" "$zip"]))
(def object-fns   (fn-maps ["$assert" "$each" "$error" "$keys" "$lookup"  "$merge" "$sift" "$spread" "$type"
                            "$mapObject" "$assoc" "$reduceKV" ; These three are provisional.
                            ]))
(def datetime-fns (fn-maps ["$fromMillis" "$millis" "$now" "$toMillis"]))
(def higher-fns   (fn-maps ["$filter" "$map" "$reduce" "$sift" "$single"]))
;;; Non-JSONata functions
(def rm-fns       (fn-maps ["$db" "$eIdent" "$identities" "$pull" "$qIdent" "$read" "$readSpreadsheet"]))

(def builtin-fns (merge numeric-fns agg-fns boolean-fns array-fns string-fns object-fns datetime-fns higher-fns rm-fns))
(def builtin? (-> builtin-fns keys (into ["$$" \$]) set))
(def builtin-un-op #{\+, \- :not})

;;; Binary operators. [+ - * / < > <= >= =]
(def numeric-operators    '{\% :op/%, \* :op/multiply, \+ :op/add, \- :op/subtract, \/ :op/div}) ; :range is not one of these.
(def comparison-operators '{"<=" :op/lteq, ">=" :op/gteq, "!=" :op/!=, \< :op/lt, \= :op/eq, \> :op/gt "in" :op/in})
(def boolean-operators    '{"and" :op/and "or" :op/or})
(def string-operators     '{\& :op/concat-op})
(def other-operators      '{\. :op/get-step "~>" :op/thread})
(def non-binary-op?       '{".." :op/range \? :op/conditional ":=" :op/assign})

(def binary-op? (merge numeric-operators comparison-operators boolean-operators string-operators other-operators))

#_(defn spec?
  "Return true if the argument is a spec."
  [obj]
  (and (keyword? obj)
       (-> obj namespace not-empty)
       (try
         (= "Spec" ; ToDo: This works but seems hacky.
            (-> (with-out-str (clojure.repl/doc obj))
                clojure.string/split-lines
                (nth 2)))
         #?(:clj  (catch Exception _ nil)
            :cljs (catch :default _ nil)))))

(s/def ::Jvar          (s/and (s/keys :req-un [::typ ::jvar-name])       #(= (:typ %) :Jvar)      #(-> % :jvar-name  string?)))
(s/def ::Qvar          (s/and (s/keys :req-un [::typ ::qvar-name])       #(= (:typ %) :Qvar)      #(-> % :qvar-name  string?)))
(s/def ::Field         (s/and (s/keys :req-un [::typ ::field-name])      #(= (:typ %) :Field)     #(-> % :field-name string?)))
(s/def ::StringLit     (s/and (s/keys :req-un [::typ ::value])           #(= (:typ %) :StringLit) #(-> % :value string?)))
(s/def ::Comment       (s/and (s/keys :req-un [::typ ::text])            #(= (:typ %) :Comment)))
(s/def ::RegExp        (s/and (s/keys :req-un [::typ ::base ::flags])    #(= (:typ %) :RegExp)))
(s/def ::PatternRole   (s/and (s/keys :req-un [::typ ::role-name])       #(= (:typ %) :PatternRole))) ; ToDo: Needs work.
(def new-line "Platform's newline as a string" (with-out-str (newline)))

(defn regex-from-string
  "Returns a map for :raw and :tkn, where :tkn is a RegExp if it is possible to parse
   the string as starting a regular expression. Otherwise returns nil.
   Uses heuristics."
  [st]
  (assert (str/starts-with? st "/"))
  (let [in-len (count st)
        base (loop [cnt 1
                    in (subs st 1)
                    done? false
                    out \/]
               (cond done? out
                     (> cnt in-len) nil
                     (empty? in) nil
                     :else (recur (inc cnt)
                                  (if (str/starts-with? in "\\") (subs in 2) (subs in 1))
                                  (str/starts-with? in "/")
                                  (if (str/starts-with? in "\\") (str out "\\" (subs in 1 2)) (str out (subs in 0 1))))))]
    (when (and base (not (index-of base new-line))) ; Flags are after the closing /
      (let [flags (or (-> (re-matches #"(?s)([imugsy]{1,6}).*" (subs st (count base))) second) "") ; Was (?sm). Does that make sense?
            flag-map (cond-> {}
                       (index-of flags \i) (assoc :ignore-case? true)
                       (index-of flags \m) (assoc :multi-line? true)
                       (index-of flags \u) (assoc :unicode? true)
                       (index-of flags \g) (assoc :global? true)
                       (index-of flags \s) (assoc :dot-all? true)
                       (index-of flags \y) (assoc :sticky? true))]
        {:raw (str base flags)
         :tkn {:typ :RegExp :base base :flags flag-map}}))))

(defn regex-or-divide
  "Return as :tkn either an RegExp or a /.
   Uses heuristics to guess whether it is a regex."
  [st]
  (or (regex-from-string st)
      {:raw "/" :tkn \/}))

(defn quoted-string
  "Return a token map for a string delimited by matching single- or double-quote characters.
   Note that you get double-quoted strings for free from the Clojure reader."
  [s quote-char]
  (loop [chars (rest s)
         raw quote-char
         res ""]
    (cond (empty? chars) (throw (ex-info "unbalanced quoted string" {:string s}))
          (= quote-char (-> chars first))
          {:raw (str raw quote-char) :tkn {:typ :StringLit :value res}}
          (and (= \\ (-> chars first)) (= quote-char (-> chars second)))
          (recur (-> chars rest rest)
                 (str raw \\ quote-char)
                 (str res quote-char))
          :else (recur (rest chars)
                       (str raw (-> chars first))
                       (str res (-> chars first))))))

(defn read-qvar
  "read a query var"
  [st]
  (let [s (-> st str/split-lines first)]
    (if-let [[_ matched] (re-matches #"(\?[a-zA-Z][a-zA-Z0-9\-\_]*).*" s)]
      {:raw matched :tkn {:typ :Qvar :qvar-name matched}}
      (throw (ex-info  "String does not start a legal query variable:" {:string  s})))))

(defn read-pattern-role
  "read a pattern role"
  [st]
  (let [s (-> st str/split-lines first)]
    ;; ToDo: Only one '/' allowed!
    (if-let [[_ matched] (re-matches #"(\:[a-zA-Z][a-zA-Z0-9/\-\_]*).*" s)]
      (if (or (> (-> (for [x matched :when (= x  \/)] x) count) 1)
              (= \/ (nth matched (-> matched count dec))))
        (throw (ex-info "String does not start a legal pattern role:" {:string s}))
        {:raw matched :tkn {:typ :PatternRole :role-name (util/read-str matched)}})
      (throw (ex-info "String does not start a legal pattern role:" {:string st})))))

(defn whitesp
  "Evaluates to whitespace at head of string or empty string if none."
  [s] ; https://stackoverflow.com/questions/15020669/clojure-multiline-regular-expression
  (if s (or (nth (re-matches #"(?s)(\s+).*$" s) 1) "") ""))

(defn read-eol-comment
  "Return a token object:
   {:tkn :eol-comment :ws ws :raw <// string to the end of the line>} :value <string to the end of the line>}."
  [st ws]
  (let [line (-> st str/split-lines first)
        line (subs line (count ws))
        [raw text] (re-matches #"//(.*)$" line)]
    {:tkn {:typ :EOL-comment :text text} :raw raw :ws ws}))

;;; ToDo: Reading of blocks can mean that we don't have a complete comment. read-comment should get more (and give up at some point).
(defn read-c-comment
  "Return a token object (keys :ws, :raw, and :tkn {:typ :Comment :text <the text>} for a c-style comment.
   This is called with a string-block possibly containing whitespace before '/*'.
   N.B.: I haven't found a regex for JS/CLJS that doesn't blows right past the */."
  [string-block]
  (let [ws (whitesp string-block)
        s (subs string-block (count ws))
        o-regex  (re-pattern #"\s*/\*.*")
        c-regex  (re-pattern #"(.*\*/).*")
        comment (loop [lines (str/split-lines s)
                       open-count  (if (re-matches o-regex (first lines)) 1 0)
                       close-count (if (re-matches c-regex (first lines)) 1 0)
                       res (first lines)]
                  (cond (== open-count close-count)   (subs res 0 (+ (.indexOf  res "*/") 2)) ; Could be all on the first line too.
                        (-> lines second not)         nil            ; Wasn't all on first line, and ended.  Also, see ToDo above.
                        :else                         (recur (rest lines)
                                                             (if (re-matches o-regex (second lines)) (inc  open-count) open-count)
                                                             (if (re-matches c-regex (second lines)) (inc close-count) close-count)
                                                             (str res "\n" (->> lines second (re-matches c-regex) second)))))]
    {:ws ws :raw comment :tkn {:typ :C-comment :text comment}}))

(defn read-long-syntactic
  "Return a map containing a :tkn and :raw string for 'long syntactic' lexemes,
   which include arbitray query vars and roles too."
  [st ws]
  (let [len (count st)
        c0  (nth st 0)
        c1  (and (> len 1) (nth st 1))]
    (when-let [res (cond (and (= c0 \/) (= c1 \/)) (read-eol-comment st ws),
                         (= c0 \/) (regex-or-divide st),
                         (#{\' \"} c0) (quoted-string st c0),
                         (and (= c0 \?) (re-matches #"[a-zA-Z]" (str c1))) (read-qvar st),
                         (and (= c0 \:) (re-matches #"[a-zA-Z]" (str c1))) (read-pattern-role st),
                         (and (= c0 \:) (= c1 \=)) {:raw ":="},
                         (and (= c0 \<) (= c1 \=)) {:raw "<="},
                         (and (= c0 \>) (= c1 \=)) {:raw ">="},
                         (and (= c0 \=) (= c1 \=)) {:raw "=="},
                         (and (= c0 \.) (= c1 \.)) {:raw ".."},
                         (and (= c0 \!) (= c1 \=)) {:raw "!="},
                         (and (= c0 \~) (= c1 \>)) {:raw "~>"},
                         (and (= c0 \<) (= c1 \|)) {:raw "<|"},
                         (and (= c0 \|) (= c1 \>)) {:raw "|>"})]
      (cond-> res
        (#{":=" "<=" ">=" "==" ".." "<|" "|>" "!=" "~>"} (:raw res)) (assoc :tkn (:raw res))
        true (assoc :ws ws)))))

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
          (long-syntactic? c) n
          (syntactic? c) n
          (#{\space \tab \newline} c) n
          :else (recur (inc n)))))))

;;; ToDo: See split-at.
(defn get-more
  "Update :string-block and :line-seq by getting more lines from the line-seq lazy seq."
  [pstate]
  (as-> pstate ?ps
    (assoc  ?ps :string-block (->> ?ps :line-seq (take block-size) (map #(str % "\n")) (apply str)))
    (update ?ps :line-seq #(drop block-size %))))

(def ^:dynamic *debugging-tokenizer?* false)

(defn one-token-from-string
  "Return a map with keys :ws, :raw and :tkn from the front of the argument string.
   Tokenization returns maps with :tkn as a STRING for things that you might expect a keyword or symbol.
   For example + is the string \"+\". Parsing returns :op/add for this."
  [string-block line] ; line is just for error reporting.
  (when *debugging-tokenizer?*  (println (cl-format nil "-----> string-block = ~S " string-block)))
  (let [ws (whitesp string-block)
        s (subs string-block (count ws))
        c (-> s first)
        result
        (if (empty? s)
          {:ws ws :raw "" :tkn ::end-of-block},   ; Lazily pulling lines from line-seq; need more.
          ;; The problem with use of the clj regex in cljs is that it reads past the closing */
          (or  (when (re-matches #"(?s)/\*.*" s) (read-c-comment string-block)) ; JS needs the (?s) even though it is not used!
               #_(when-let [[_ cm _] (re-matches #?(:cljs #"(?s)(/\*.*\*/).*"  ; #"(?s)(/\*.*\*/).*" ; NOT WORKING cljs <==============
                                                    :clj  #"(?s)(\/\*(\*(?!\/)|[^*])*\*\/).*")
                                               s)]   ; comment; JS has problems with #"(?s)(\/\*(\*(?!\/)|[^*])*\*\/).*"
                 {:ws ws :raw cm :tkn {:typ :Comment :text cm}})

               (and (long-syntactic? c) (read-long-syntactic s ws))    ; string literals, /regex-pattern/ ++, <=, == etc.
               (when-let [[_ num] (re-matches #"(?s)([-]?\d+(\.\d+(e[+-]?\d+)?)?).*" s)] ; + cannot be used as a unary operator.
                 {:ws ws :raw num :tkn (util/read-str num)}),          ; number
               (when-let [[_ st] (re-matches #"(?s)(\`[^\`]*\`).*" s)] ; backquoted field
                 {:ws ws :raw st :tkn {:typ :Field :field-name st}})
               (and (syntactic? c) {:ws ws :raw (str c) :tkn c}) ; literal syntactic char.
               (let [pos (position-break s)
                     word (-> (subs s 0 (or pos (count s))) str/trim)]
                 (or ; We don't check for "builtin-fns"; as tokens they are just jvars.
                  (and (keywords word)    {:ws ws :raw word :tkn (keywords word)})
                  (when-let [[_ id] (re-matches #"^([a-zA-Z0-9\_]+).*" word)]              ; field or symbol in params map
                    {:ws ws :raw id :tkn (util/read-str id)})
                  (when-let [[_ id] (re-matches #"^(\$[a-zA-Z][A-Za-z0-9\_]*).*" word)]    ; jvar
                    {:ws ws :raw id :tkn {:typ :Jvar :jvar-name id}})
                  (when-let [[_ id] (re-matches #"^(\${1,3}).*" word)]                     ; $, $$, $$$.
                    {:ws ws :raw id :tkn {:typ :Jvar :jvar-name id :special? true}})
                  (when-let [[_ id] (re-matches #"^(:[a-zA-Z][a-zA-Z0-9\-\_]*).*" word)]   ; pattern role
                    {:ws ws :raw id :tkn {:typ :PatternRole :role-name id}})))
               (throw (ex-info "Char starts no known token:" {:raw c :line line}))))]
    (when *debugging-tokenizer?*
      (println (cl-format nil "<----- result = ~S" result)))
    result))

;;; ToDo: I've lost track of what happens to c-style comments; they aren't in the argument pstate here!
(defn filter-comments
  "Update :tokens to remove :eol-comment" ; ToDo: Someday store those comments!
  [pstate]
  (update pstate :tokens (fn [tkns] (filterv #(not (#{:EOL-comment :C-comment} (-> % :tkn :typ))) tkns))))

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
          (filter-comments ?ps)
          (recur
           (if (= (-> lex :tkn :typ) :Comment)
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
(def diag (atom nil))
(def report-pstate? "This is used for generating error messages. It is false for the exerciser." (atom false))

(defn ps-throw
  "Throw with a message naming the line and column where the parser was reading.
   Also (if throwing with report-pstate?=true) remove the :line-seq (a clj/java thing),
   which is unprintable. (Throws again if you try to print it.)"
  [pstate msg arg-map]
  (let [arg-strs (reduce-kv (fn [res k v] (-> res (conj (name k)) (conj v))) [] arg-map)
        report   (cl-format nil "(~A, ~A): ~A ~{~A: ~A~} ~%~A"
                            (:line pstate) (:col pstate) msg arg-strs
                            (if @report-pstate? (cl-format nil "pstate = ~A~%" pstate) ""))]
    (as-> pstate ?ps
      (dissoc ?ps :line-seq) ; So REPL won't freak out over the reader being closed...
      (do (reset! diag {:pstate ?ps :msg msg :arg-map arg-map}) ?ps)
      (throw (ex-info report arg-map)))))

(defn look
  "Sets a value in the :look map of pstate and might do some tokenizing in the process.
   n = 1 is one past :head, :tokens[0].
   Note the the value of (:look pstate) will be wrong if the call isn't 'fresh'."
  [pstate n] ; 2 of 3, refreshing tokens.
  (let [ps (refresh-tokens pstate n) ; no-op if (-> ps :tokens count) >= n.
        tokens (:tokens ps)
        cnt (count tokens)]
    (assoc-in ps [:look n] (if (> n cnt) ::eof (-> tokens (nth (dec n)) :tkn)))))

(defn ps-assert
  "A special s/assert that does ps-throw on an error."
  [ps]
  (try (s/assert ::ps ps)
       (catch #?(:clj Exception :cljs :default) e
         (ps-throw ps (str "pstate is invalid: " #?(:clj (.getMessage e) :cljs e))
                   {:tags (:tags ps)
                    :head (:head ps)
                    :tokens (:tokens ps)}))))

(defn explain-spec
  "Return a string explaining why the argument token failed the argument test."
  [spec token]
  (let [typs  (->> (s/explain-data spec token)
                   ::s/problems
                   (map #(-> % :path first name)))]
    (cl-format nil "Expected ~{~A~^, ~} or ~A. Got ~A." (butlast typs) (last typs) token)))

(defn spec-check
  "Return true if the argument token is s/valid? according to argument spec."
  [pstate spec token]
  (or (s/valid? spec token)
      (ps-throw pstate (explain-spec spec token) {})))

(defn match-head
  "Return true if token matches test, which is a string, character, fn or regex."
  [pstate test]
  (let [head (:head pstate)]
    (cond (= test ::pass) true,
          ;(spec? test)       (spec-check pstate test head)
          (= test head) true,
          (map? test)        (test head),
          (set? test)        (test head),
          (fn? test)         (test head),
          (util/regex? test) (re-matches test head),
          :else              (ps-throw pstate (cl-format nil "Expected a ~A token." test)
                                       {:test test :got head :tags (:tags pstate)}))))

(defn eat-token
  "Move head of :tokens to :head ('consuming' the old :head) With 2 args, test :head first."
  ([pstate] (eat-token pstate ::pass))
  ([pstate test]
   (when *debugging?* (log/debug (cl-format nil "~AEAT ~A  (type ~A)"
                                           (util/nspaces (* 3 (-> pstate :tags count dec)))
                                           (pp/write (:head pstate) :readably false :stream nil)
                                           #?(:clj (-> pstate :head util/class-name name)
                                              :cljs "unknown (cljs)"))))
   (match-head pstate test)
   ;; The actual work of eating a token
   (let [ps1 (if (-> pstate :tokens empty?) (refresh-tokens pstate) pstate) ; 3 of 3, refreshing tokens.
         next-up (-> ps1 :tokens first)]
     (as-> ps1 ?ps
       (update ?ps :line #(or (:line next-up) %))
       (update ?ps :col  #(or (:col  next-up) %))
       (assoc  ?ps :head  (if next-up (:tkn next-up) ::eof)) ; One of two places :head is set; the other is make-pstate.
       (update ?ps :tokens #(-> % rest vec))       ; 2 of 3, setting :tokens.
       (ps-assert ?ps)))))

(defn token-vec [ps] (into (-> ps :head vector) (mapv :tkn (:tokens ps))))

(def balanced-map "What balances with the opening syntax?" { \{ \}, \( \), \[ \]})
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

(defn store
  "This and recall are used to keep parsed content tucked away on the parse state object."
  ([ps key] (store ps key (:result ps)))
  ([ps key val] (assoc-in ps [:local 0 key] val)))

(defn recall
  "This and store are used to keep parsed content tucked away on the parse state object."
  [ps key]
  (get-in ps [:local 0 key]))

(defn make-pstate
  "Make a parse state map and start tokenizing."
  [reader-or-str]
  (as-> {:head nil ; In this order for easy debugging.
         :line 1   ; Will be updated by eat-token.
         :col  1   ; Will be updated by eat-token.
         :tags []
         :local []
         :look {}
         :tokens []
         :string-block ""
         :reader       #?(:clj reader-or-str :cljs nil)
         :line-seq     (util/ln-seq reader-or-str)
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
   If the text is intended to have JS escape, ',' in it, it has to be escaped!"
  ([str] (parse-string :ptag/primary str))
  ([tag str]
   (let [pstate (->> str tokenize make-pstate (parse tag))]
     (if (not= (:head pstate) ::eof)
       (throw (ex-info "Tokens remain:" {:tokens (:tokens pstate)}))
        #_(do (when *debugging?*
                (log/debug (cl-format nil "~%*** Tokens remain. pstate=~A ~%" pstate)))
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
#?(:clj
(defn parse-file
  "Parse a whole file given a filename string."
  [filename]
  (parse-string :ptag/primary (-> filename slurp esc-esc))))

(defn parse-list
  "Does parse parametrically for <open-char> ( <item> ( <char-sep> <item>)? )? <close-char>"
  ([pstate char-open char-close char-sep]
   (parse-list pstate char-open char-close char-sep :ptag/exp))
  ([pstate char-open char-close char-sep item-tag]
   (when *debugging?*
     (log/debug (cl-format nil ">>>>>>>>>>>>>> parse-list (~A) >>>>>>>>>>>>>>>>>>" item-tag)))
   (let [final-ps
         (as-> pstate ?ps
           (eat-token ?ps char-open)
           (loop [ps ?ps]
             (cond
               (= ::eof (:head ps))        (ps-throw ps "End of file parsing a list of" {:tag item-tag}),

               (= char-close (:head ps))   (as-> ps ?ps1
                                             (eat-token ?ps1)
                                             (assoc ?ps1 :result (get-in ?ps1 [:local 0 :collection/items]))),

               :else                       (as-> ps ?ps1
                                             (parse item-tag ?ps1)
                                             (update-in ?ps1 [:local 0 :collection/items] conj (:result ?ps1))
                                             (if (#{char-sep char-close} (:head ?ps1))
                                               ?ps1 ; Should either end here or continue with a separator.
                                               (ps-throw ?ps1 (cl-format nil "In a list, expected ~A or ~A." char-sep char-close)
                                                         {:got (:head ?ps1)}))
                                             (recur (cond-> ?ps1
                                                      (= char-sep (:head ?ps1)) (eat-token char-sep)))))))]
     (when *debugging?*
       (println "\nCollected" (:result final-ps))
       (log/debug (cl-format nil "<<<<<<<<<<<<<<<<<<<<< parse-list (~A) <<<<<<<<<<<<<<<<<<<<<<<" item-tag)))
     (reset! diag final-ps)
     final-ps)))

;;; ToDo: Just in-line these?
(defn jvar? [x]         (s/valid? ::Jvar x))
(defn qvar? [x]         (s/valid? ::Qvar x))
(defn pattern-role? [x] (s/valid? ::PatternRole x))
(defn field? [x]        (s/valid? ::Field x))
(defn regex? [x]        (s/valid? ::RegExp x))
(defn string-lit? [x]   (s/valid? ::StringLit x))
(defn literal? [tkn]
  (or (string-lit? tkn)
      (number? tkn)
      (#{:tk/true :tk/false} tkn)
      (regex? tkn)))

(s/def ::ps (s/keys :req-un [::tokens ::head]))
(s/def ::ps-done (s/and ::ps
                        #(= :ok (:parse-status %))
                        #(empty? (:tokens %))
                        #(= ::eof (:head %))))
(s/def ::tokens (s/and vector? (s/coll-of ::token)))
(s/def ::token  (s/and map? #(contains? % :tkn) #(-> % :tkn nil? not)))
(s/def ::head #(not (nil? %)))

(def bin-op-plus? "Looks weird; just used for assessing exp continuation below!"
  (merge binary-op? '{\. :op/get-step \[ :op/filter-step \{ :op/reduce-step}))

(s/def ::BinOpSeq (s/keys :req-un [::seq]))
;;;=============================== Grammar ===============================
;;; <exp> ::= <base-exp> (  <bin-op-continuation>  | '?' <conditional-tail> ) ?
(defparse :ptag/exp
  [ps]
  (let [base-ps (-> (parse :ptag/base-exp ps) (store :operand-1))]
    (cond (= \? (:head base-ps))
          (parse :ptag/conditional-tail base-ps :predicate (:result base-ps)),

          (-> base-ps :head bin-op-plus?)
          (as-> base-ps ?ps
            (parse :ptag/bin-op-continuation ?ps)
            (assoc ?ps :result {:typ :BinOpSeq
                                :seq (into (vector (recall ?ps :operand-1))
                                           (-> ?ps :result :op-operand-seq))}))
          :else base-ps)))

;;; ToDo: I think continuing from a reduce is not allowed in JSONata.
;;;       Also, user lands here when they forget a comma betwen objects, for example: "[{'hi' : 1} {'world': 2}]".
(s/def ::OpOperandSeq (s/keys :req-un [::op-operand-seq]))
(defparse :ptag/bin-op-continuation
  [ps]
  (loop [ps ps
         oseq []]
    (let [bin-op (-> ps :head bin-op-plus?)]
    (if (not bin-op)
      (assoc ps :result {:typ :OpOperandSeq :op-operand-seq oseq})
      (let [p (cond (#{\[ \}} (:head ps))          (as-> ps ?ps
                                                     (store ?ps :operator (-> ?ps :head bin-op-plus?))
                                                     (parse :ptag/base-exp ?ps :operand-2? true)),
                    (-> ps :head bin-op-plus?)     (as-> ps ?ps
                                                     (store ?ps :operator bin-op)
                                                     (eat-token ?ps)
                                                     (parse :ptag/base-exp ?ps :operand-2? true)),
                    :else                          (ps-throw ps "Expected continuation of path," {:got (:head ps)}))]
        (recur p (-> oseq (conj (recall p :operator)) (conj (:result p)))))))))

;;; ToDo:
;;;   - qvar here might make it permissive of nonsense. Needs thought.
;;;   - Likewise, :tk/key here is too permissive. express should use something other than :ptag/obj-exp and
;;;     that thing ought to reference :tk/key (so that it is not a base expression).
;;; <base-exp> ::= <delimited-exp> | (<builtin-un-op> <exp>) | <construct-def> | <fn-call> | <literal> | <jvar-decl> | <field> | <jvar> | <qvar>
(defparse :ptag/base-exp
  [ps & {:keys [operand-2?]}]
  (let [tkn  (:head ps)
        ps   (look ps 1)
        tkn2 (-> ps :look (get 1))]
    (cond (#{\{ \[ \(} tkn)                           (parse :ptag/delimited-exp ps :operand-2? operand-2?) ; <delimited-exp>
          (builtin-un-op tkn)                         (parse :ptag/unary-op-exp ps)  ; <unary-op-exp>
          (#{:tk/function :tk/query :tk/express
             :tk/rule :tk/key} tkn)                   (parse :ptag/construct-def ps) ; <construct-def>
          (and (= \( tkn2)
               (or (builtin-fns tkn)
                   (jvar? tkn)))                      (parse :ptag/fn-call ps)       ; <fn-call>
          (literal? tkn)                              (parse :ptag/literal ps)       ; <literal>
          (and (jvar? tkn) (= tkn2 ":="))             (parse :ptag/jvar-decl ps)     ; <jvar-decl>
          (or (jvar? tkn)
              (qvar? tkn)
              (field? tkn))                           (as-> ps ?ps                   ; <jvar>, <qvar> or `backquoted field`
                                                        (assoc ?ps :result tkn)
                                                        (eat-token ?ps))
          (symbol? tkn)                               (as-> ps ?ps                   ; <field> (not part of param-struct)
                                                        (assoc ?ps :result {:typ :Field :field-name (name tkn)})
                                                        (eat-token ?ps)),
          :else
          (ps-throw ps "Expected a unary-op, (, {, [, fn-call, literal, $id, or ?qvar."
                    {:got tkn}))))

;;;| syntax/op       | Example                                     | Comment                                         |
;;;|-----------------+---------------------------------------------+-------------------------------------------------|
;;;| Square / filter | Phone[type = 'mobile']                      | http://docs.jsonata.org/predicate               |
;;;| Parens / map    | Product.(price * quantity)                  | After the dot is an exp with or w/o the parens. |
;;;| Curly  / reduce | Product{`Product Name`: $.(Price*Quantity)} |                                                 |

;;;  But see $.{ in http://docs.jsonata.org/sorting-grouping.

;;; <delimited-exp> ::=  <primary> | <filter-exp> | <reduce-exp> | <range|array-exp> | <obj-exp>
(defparse :ptag/delimited-exp
  [ps & {:keys [operand-2?]}]
  (let [head (:head ps)]
    (if operand-2? ; http://docs.jsonata.org/path-operators
      (case head
        \(  (parse :ptag/primary    ps)
        \[  (parse :ptag/filter-exp ps) ; Includes aref.
        \{  (parse :ptag/reduce-exp ps))
      (case head
        \(  (parse :ptag/primary ps)
        \[  (parse :ptag/range|array-exp ps)
        \{  (parse :ptag/obj-exp ps)))))

(defn operand-exp? ; ToDo: :next-tkns is not used.
  "Return a map with keys :operand-tag and :next-tkns if it is possible to parse from the current state
   to one of #{:ptag/field :ptag/jvar :ptag/literal :ptag/fn-call :ptag/unary-op-exp}.
   :operand-tag is then set to one of those tags, and
   :next-tnks is a vector of the first two tokens that would be have been consumed when that expression is parsed.

   This is called by the <exp> grammar rule, so you can't use it to anticipate something that starts with an
   expression such as '<exp> ? <exp> : <exp>'; that would result in a non-consuming loop."
  [ps]
  (binding [*debugging?* false] ; I don't think we need to see this!
    (letfn [(try-tag [tag] (try (-> (parse tag ps) (look 1))
                                (catch #?(:clj Exception :cljs :default) _e nil)))]
      (some #(when-let [ps (try-tag %)]
               {:operand-tag %
                :next-tkns [(:head ps) (-> ps :look (get 1))]})
            [:ptag/field :ptag/jvar :ptag/literal :ptag/fn-call :ptag/unary-op-exp]))))

(defparse :ptag/field
  [ps]
  (cond (-> ps :head symbol?)  (as-> ps ?ps
                                 (assoc ?ps :result {:typ :Field :field-name (?ps :head name)})
                                 (eat-token ?ps))
        (-> ps :head field?)   (as-> ps ?ps
                                 (assoc ?ps :result (:head ?ps))
                                 (eat-token ?ps))
        :else                  (ps-throw ps "expected a field" {:got (:head ps)})))

(defparse :ptag/param
  [ps]
  (as-> ps ?ps
    (assoc ?ps :result (:head ?ps))
    (eat-token ?ps jvar?)))

(def ^:dynamic in-express?
  "When true, any :ptag/obj expression encountered will create a ExpressMap, not ObjExp."
  nil)
;;; ToDo: Verify that no distinction in syntax between reduce and obj construction.
;;;       Keep the distinction in structures created, however!
(s/def ::ReduceExp  (s/keys :req-un [::operand ::kv-pairs]))
(s/def ::ObjExp     (s/keys :req-un [::kv-pairs]))
(s/def ::ExpressMap (s/keys :req-un [::kv-pairs]))
(defparse :ptag/obj-exp
  [ps]
  (as-> ps ?ps
    (parse-list ?ps \{ \} \, :ptag/obj-kv-pair) ; Operand added in :ptag/delimited-exp
    (if in-express?
      (assoc ?ps :result {:typ :ExpressMap :kv-pairs (:result ?ps)})
      (assoc ?ps :result {:typ :ObjExp     :kv-pairs (:result ?ps)}))))

;;; <reduce-exp> ::= '{' <obj-kv-pair>* '}'
(defparse :ptag/reduce-exp
  [ps]
  (as-> ps ?ps
    (parse-list ?ps \{ \} \, :ptag/obj-kv-pair) ; Operand will be added in :ptag/delimited-exp
    (assoc ?ps :result {:typ :ReduceExp :kv-pairs (:result ?ps)})))

;;; ToDo: Perhaps for express body, key could be any expression?
(s/def ::obj-key (s/or :string         string?
                       :variable       #(= (:typ %) :Jvar)
                       :query-variable #(= (:typ %) :Qvar)))
;;; <obj-key> ::== qvar | base-exp
(defparse :ptag/obj-key
  [pstate]
    (if (-> pstate :head qvar?)
      (as-> pstate ?ps
        (store ?ps :qvar (:head ?ps))
        (eat-token ?ps)
        (assoc ?ps :result (recall ?ps :qvar)))
      (parse :ptag/exp pstate))) ; ToDo: I wanted :ptag/base-exp here. This seems too permissive!

;;; <map-pair> ::=  ( <string> | <qvar> ) ":" <exp>
(s/def ::KVPair (s/keys :req-un [::key ::val]))
(s/def ::ExpressKVPair (s/keys :req-un [::key ::val]))
(defparse :ptag/obj-kv-pair
  [pstate]
  (as-> pstate ?ps
    (parse :ptag/obj-key ?ps)
    (store ?ps :key)
    (eat-token ?ps \:)
    (parse :ptag/exp ?ps)
    (assoc ?ps :result {:typ (if in-express? :ExpressKVPair :KVPair)
                        :key (recall ?ps :key)
                        :val (:result ?ps)})))

(s/def ::RangeExp (s/keys :req-un [::start ::stop]))
;;; <range|array-exp> ::=  <range-exp> | <array>
(defparse :ptag/range|array-exp
  [ps]
    (let [tvec (token-vec ps)
          close-pos (when (= (first tvec) \[) (find-balanced-pos tvec \]))
          range-pos (when close-pos (find-token tvec ".." :stop-pos close-pos))]
    (if (and close-pos range-pos (< range-pos close-pos)) ; ToDo Heuristic! (Replace with try?)
      (as-> ps ?ps
        (eat-token ?ps \[)
        (parse :ptag/exp ?ps)
        (store ?ps :start)
        (eat-token ?ps "..")
        (parse :ptag/exp ?ps)
        (store ?ps :stop)
        (eat-token ?ps \])
        (assoc ?ps :result {:typ :RangeExp
                            :start (recall ?ps :start)
                            :stop (recall ?ps :stop)}))
      (parse :ptag/array ps))))

;;; <conditional-tail> ::=  <exp> ':' <exp>
(s/def ::ConditionalExp (s/keys :req-un [::predicate ::exp1 ::exp2]))
(defparse :ptag/conditional-tail
  [ps & {:keys [predicate]}]
  (as-> ps ?ps
    (eat-token ?ps \?)
    (parse :ptag/exp ?ps)
    (store ?ps :then)
    (eat-token ?ps \:)
    (parse :ptag/exp ?ps)
    (assoc ?ps :result {:typ :ConditionalExp
                        :predicate predicate
                        :exp1 (recall ?ps :then)
                        :exp2 (:result ?ps)})))

;;; Note that JSONata allows a final ; before the closing paren; parse-list tolerates that too.
;;; <primary>    := '(' ( <exp> (';' <exp>)* )? ;? ')'
(s/def ::Primary (s/keys :req-un [::exps]))
(defparse :ptag/primary
  [ps]
  (as-> ps ?ps
    (parse-list ?ps \( \) \; :ptag/exp)
    (assoc ?ps :result {:typ :Primary :exps (:result ?ps)})))

;;; <filter-exp> := '[' <exp> ']'
(s/def ::ApplyFilter (s/keys :req-un [::body])) ; This accommodates aref expressions too.
(defparse :ptag/filter-exp
  [ps]
  (as-> ps ?ps
    (eat-token ?ps \[)
    (parse :ptag/exp ?ps)
    (eat-token ?ps \])
    (assoc ?ps :result {:typ :ApplyFilter :body (:result ?ps)})))

;;; jvar-decl ::= <jvar> ':=' <exp>
(s/def ::JvarDecl (s/keys :req-un [::var ::init-val]))
(defparse :ptag/jvar-decl
  [pstate]
  (as-> pstate ?ps
    (if (-> ?ps :head builtin-fns)
       (ps-throw ?ps "Attempting to rebind a built-in function."
                 {:built-in-fn (:head ?ps)})
       ?ps)
    (parse :ptag/jvar ?ps)
    (store ?ps :jvar)
    (eat-token ?ps ":=")
    (parse :ptag/exp ?ps)
    (store ?ps :init-val)
    (assoc ?ps :result {:typ :JvarDecl
                        :var (recall ?ps :jvar)
                        :init-val (recall ?ps :init-val)})))

;;; So far, just used for exerciser user-data.
(defparse :ptag/jvar-decls
  [pstate]
  (parse-list pstate \( \) \; :ptag/jvar-decl))

;;; <unary-exp> ::= <unary-operator> <exp>
(s/def ::UniOpExp (s/keys :req-un [::uni-op ::exp]))
(defparse :ptag/unary-op-exp
  [ps]
    (as-> ps ?ps
      (store ?ps :op (:head ?ps))
      (eat-token ?ps builtin-un-op)
      (let [{:keys [operand-tag]} (operand-exp? ?ps)] ; ToDo was operand-exp-no-operator?
        (as-> ?ps ?ps1
        (parse operand-tag ?ps1)
        (assoc ?ps1 :result {:typ :UniOpExp
                             :uni-op (recall ?ps1 :op)
                             :exp (:result ?ps1)})))))

;;;----- 'atomic' expressions, these are useful for parse-list etc. -------
(defparse :ptag/jvar
  [ps]
  (as-> ps ?ps
    (assoc ?ps :result (:head ?ps))
    (eat-token ?ps jvar?)))

(defparse :ptag/string
  [ps]
  (let [tkn (:head ps)]
    (if (string-lit? tkn)
      (-> ps (assoc :result (:value tkn)) eat-token)
      (ps-throw ps "expected a string literal" {:got tkn}))))

;;; <literal> ::= string | number | 'true' | 'false' | regex | <obj> | <array>
(defparse :ptag/literal
  [ps]
  (reset! diag {:ps ps})
  (let [tkn (:head ps)]
    (cond (string-lit? tkn)               (-> ps (assoc :result (:value tkn)) eat-token)
          (literal? tkn)                  (-> ps (assoc :result tkn) eat-token),
          (= tkn \{)                      (parse :ptag/obj-exp ps),
          (= tkn \[)                      (parse :ptag/array ps),
          :else (ps-throw ps "expected a literal string, number, 'true', 'false' regex, obj, range, or array."
                          {:got tkn}))))

(s/def ::Array (s/keys :req-un [::exprs]))
(defparse :ptag/array
  [ps]
  (as-> ps ?ps
    (parse-list ?ps \[ \] \, :ptag/exp)
    (assoc ?ps :result {:typ :Array :exprs (:result ?ps)})))

;;; ToDo: This is added because query functions can have pattern roles as arguments. Keep?
;;;       If this doesn't work out exactly, it may be possible to have a separate kind of function???
;;;       That would have to be caught later (maybe rewrite, maybe runtime) using metadata, however.
;;; fn-arg ::== <exp> | <query-role-literal>
(defparse :ptag/fn-arg
  [ps]
  (if (-> ps :head pattern-role?)
    (parse :ptag/query-role-literal ps)
    (parse :ptag/exp ps)))

;;; fn-call ::=  <jvar> '(' <exp>? (',' <exp>)* ')'
(s/def ::FnCall (s/keys :req-un [::fn-name ::args]))
(defparse :ptag/fn-call
  [ps]
    (as-> ps ?ps
      (store ?ps :fn-name (:head ?ps))
      (eat-token ?ps jvar?)
      (parse-list ?ps \( \) \, :ptag/fn-arg) ; ToDo: was :ptag/exp.
      (assoc ?ps :result {:typ :FnCall
                          :fn-name (-> ?ps (recall :fn-name) :jvar-name)
                          :args (:result ?ps)})))

;;; <query-patterns> ::= <query-pattern>+
(defparse :ptag/query-patterns
  [ps]
  (let [ps-one (parse :ptag/query-pattern ps)]
    (loop [result (vector (:result ps-one))
           ps ps-one]
      (if (not= \[ (:head ps))
        (assoc ps :result result)
        (let [ps (parse :ptag/query-pattern ps)]
          (recur (conj result (:result ps))
                 ps))))))

;;; <query-pattern> :: <q-pattern-tuple> | <q-pattern-pred>
(defparse :ptag/query-pattern
  [ps]
  (let [ps   (look ps 1)
        tkn2 (-> ps :look (get 1))]
    (if (= tkn2 \()
      (parse :ptag/q-pattern-pred  ps)
      (parse :ptag/q-pattern-tuple ps))))

;;; <q-pattern-pred> :: '[' '(' <query-predicate> ')' ']'
(s/def ::QueryPred (s/keys :req-un [::exp]))
(defparse :ptag/q-pattern-pred
  [ps]
  (as-> ps ?ps
    (eat-token ?ps \[)
    (eat-token ?ps \()
    (parse :ptag/fn-call ?ps)
    (eat-token ?ps \))
    (eat-token ?ps \])
    (assoc ?ps :result {:typ :QueryPred :exp (:result ?ps)})))

;;; ToDo: So far, this is just for specifying arguments to a parametric query function,
;;;       (It is being added to function call expressions.)
;;;       It could be use on :ptag/q-pattern-tuple, if that works out.
(defparse :ptag/query-role-literal
  [ps]
  (as-> ps ?ps
    (assoc ?ps :result (:head ?ps))
    (eat-token ?ps #(pattern-role? %))))

;;; <q-pattern-tuple> :: '['         <QueryVar> <QueryRole> (<QueryVar> | <exp>) ']' |
;;;                      '[' <DBVar> <QueryVar> <QueryRole> (<QueryVar> | <exp>) ']'
(s/def ::QueryPattern (s/keys :req-un [::ent ::rel ::val] :opt-un [::db]))
(defparse :ptag/q-pattern-tuple
  [ps]
  (as-> ps ?ps
    (eat-token ?ps \[)
    (if (-> ?ps :head jvar?)
      (as-> ?ps ?ps1
        (store ?ps1 :db (:head ?ps1))
        (eat-token ?ps1))
      ?ps)
    (store ?ps :ent (:head ?ps))
    (eat-token ?ps qvar?)
    (store ?ps :role (:head ?ps))
    (eat-token ?ps #(or (pattern-role? %) (qvar? %)))
    (if (qvar? (:head ?ps))
      (as-> ?ps ?ps1
          (store ?ps1 :data (:head ?ps1))
          (eat-token ?ps1))
      (as-> ?ps ?ps1
          (parse :ptag/exp ?ps1)
          (store ?ps1 :data)))
    (eat-token ?ps \])
    (assoc ?ps :result {:typ :QueryPattern
                        :db  (recall ?ps :db)
                        :ent (recall ?ps :ent)
                        :rel (recall ?ps :role)
                        :val (recall ?ps :data)})))

;;; <fn-def> ::= 'function' '(' <jvar>? [',' <jvar>]* ')' '{' <exp> '}'
(s/def ::FnDef (s/keys :req-un [::vars ::body]))
(defparse :ptag/fn-def
  [ps]
  (as-> ps ?ps
    (eat-token ?ps :tk/function)
    (parse-list ?ps \( \) \, :ptag/jvar)
    (store ?ps :jvars)
    (eat-token ?ps \{)
    (parse :ptag/exp ?ps)
    (store ?ps :body)
    (eat-token ?ps \})
    (assoc ?ps :result {:typ :FnDef
                        :vars (recall ?ps :jvars)
                        :body (recall ?ps :body)})))

#_(defparse :ptag/param-or-options-map
  [ps]
  (if (= "<|" (:tkn ps))
    (parse :ptag/options-map ps)
    (parse :ptag/jvar ps)))

;;; ToDo: Is this really what I want? An options map stuck in with formal parameters???
;;; <query-def> ::= 'query (  '(' <jvar>? [',' <jvar|options>]* ')' )? '{' ( <query-patterns> | <$qIdent call> ) '}'
(s/def ::QueryDef (s/keys :req-un [::params ::patterns|qcall]))
(defparse :ptag/query-def
  [ps]
  (as-> ps ?ps
    (eat-token ?ps :tk/query)
    (if (= \( (:head ?ps))
      (as-> ?ps ?ps1
        (parse-list ?ps1 \( \) \, :ptag/jvar|options) ; was :ptag/jvar.
        (store ?ps1 :params))
      ?ps)
    (eat-token ?ps \{)
    (if (= "$qIdent" (-> ?ps :head :jvar-name))
        (parse :ptag/fn-call ?ps)
        (parse :ptag/query-patterns ?ps))
    (eat-token ?ps \})
    (assoc ?ps :result {:typ :QueryDef
                        :params   (->> (recall ?ps :params) (filterv jvar?))
                        :options  (->> (recall ?ps :params) (filter (complement jvar?)) first)
                        :patterns|qcall (:result ?ps)})))


;;; ToDo: We don't care where you put the options.
;;;       We don't care whether there is more than one options map.
;;;       Should we?
(defparse :ptag/jvar|options
  [ps]
  (if (-> ps :head jvar?)
    (as-> ps ?ps
      (assoc ?ps :result (:head ?ps))
      (eat-token ?ps))
    (parse :ptag/options-map ps)))

;;; ToDo: These aren't jvars, probably want some sort of optional parameter syntax.
;;; <express-def> ::= 'express' ( '(' <jvar>? [',' <jvar>]* ')' )? '{' <exp> '}'
(s/def ::ExpressDef (s/keys :req-un [::params ::body]))
(defparse :ptag/express-def
  [ps]
  (binding [in-express? true]
    (as-> ps ?ps
      (eat-token ?ps :tk/express)
      (if (= \( (:head ?ps))
        (as-> ?ps ?ps1
          (parse-list ?ps1 \( \) \, :ptag/jvar|options)
          (store ?ps1 :params))
        ?ps)
      (eat-token ?ps \{)
      (if (= "$eIdent" (-> ?ps :head :jvar-name))
        (parse :ptag/fn-call ?ps)
        (parse :ptag/obj-exp ?ps))
      (store ?ps :body)
      (eat-token ?ps \})
      (assoc ?ps :result {:typ    :ExpressDef
                          :params (recall ?ps :params)
                          :body   (recall ?ps :body)}))))

(defparse :ptag/rule-def
  [ps]
  (as-> ps ?ps
    (eat-token ?ps :tk/rule)
    (eat-token ?ps \{)
    (parse :ptag/rule-head ?ps)
    (store ?ps :rule-head)
    (loop [ps ?ps
           clauses []]
      (if (not= \[ (:head ps))
        (-> ps (store :clauses clauses) (eat-token \}))
        (as-> ps ?ps
          (parse :ptag/rule-clause ?ps)
          (recur ?ps
                 (conj clauses (:result ?ps))))))
    (assoc ?ps :result {:typ :RuleDef
                        :rule-head (recall ?ps :rule-head)
                        :clauses (recall ?ps :clauses)})))

(defparse :ptag/rule-head
  [ps]
  (as-> ps ?ps
    (eat-token ?ps \()
    (store ?ps :predicate (:head ?ps))
    (eat-token ?ps symbol?)
    (loop [ps ?ps
           forms []]
      (if (= \) (:head ps))
        (-> ps (store :args forms) eat-token)
        (let [qvar (:head ps)]
          (recur (eat-token ps qvar?)
                 (conj forms qvar)))))
    (assoc ?ps :result {:typ :RuleHead
                        :predicate (recall ?ps :predicate)
                        :args (recall ?ps :args)})))

(defparse :ptag/rule-clause
  [ps]
  (parse :ptag/q-pattern-tuple ps)) ; ToDo: Does datalog allow anything other than a triple here?

(s/def ::ImmediateUse (s/keys :req-un [::def ::args]))
;;; <construct-def> ::= ( <fn-def> | <query-def> | <express-def> )( '(' <exp>* ')' )?
(defparse :ptag/construct-def
  [ps]
  (let [ps (case (:head ps)
             :tk/function  (parse :ptag/fn-def ps),         ; <fn-def>
             :tk/query     (parse :ptag/query-def ps),      ; <query-def>
             :tk/express   (parse :ptag/express-def ps)     ; <express-def>
             :tk/rule      (parse :ptag/rule-def ps)        ; <rule-def>
             :tk/key       (parse :ptag/key-def ps))]       ; <key-def>
    (if (and (= \( (:head ps)) (#{:FnDef :QueryDef} (-> ps :result :typ)))
      ;; This part to wrap it in a ImmediateUse
      (as-> ps ?ps
        (store ?ps :def)
        (parse-list ?ps \( \) \, :ptag/exp)
        (store ?ps :args)
        (assoc ?ps :result {:typ  :ImmediateUse
                            :def  (recall ?ps :def)
                            :args (recall ?ps :args)}))
      ;; This if it is just a definition (which will be assigned to a $id).
      ps)))

;;; <key-def> ::= 'key' '(' qvar ')'
(defparse :ptag/key-def
  [ps]
  (as-> ps ?ps
    (eat-token ?ps :tk/key)
    (eat-token ?ps \()
    (store ?ps :qvar (:head ?ps))
    (eat-token ?ps qvar?)
    (eat-token ?ps \))
    (assoc ?ps :result {:typ :KeyExp
                        :qvar (recall ?ps :qvar)})))

;;; The next four 'options' rules are just for query and express keyword parameters (so far).
(s/def ::OptionsMap (s/keys :req-un [::kv-pairs]))
;;; <options-struct> '<|' <options-kv-pair>* '|>'
(defparse :ptag/options-map
  [ps]
  (as-> ps ?ps
    (parse-list ?ps "<|" "|>" \, :ptag/option-kv-pair)
    (assoc ?ps :result {:typ :OptionsMap :kv-pairs (:result ?ps)})))

(s/def ::OptionKeywordPair (s/keys :req-un [::key ::val]))
;;; <options-kv-pair> ::= <symbol> ':' <option-val>
(defparse :ptag/option-kv-pair
  [ps]
  (as-> ps ?ps
    (store ?ps :key (:head ?ps))
    (eat-token ?ps symbol?)
    (eat-token ?ps \:)
    (parse :ptag/option-val ?ps)
    (store ?ps :val)
    (assoc ?ps :result {:typ :OptionKeywordPair
                        :key (recall ?ps :key)
                        :val (recall ?ps :val)})))

(defn oval? [obj]
  (or (symbol? obj) (qvar? obj) (#{:tk/true :tk/false} obj)))

(s/def ::OptionVal (s/keys :req-un [::exp]))
;;; <option-val> ::= <option-val-atom> | '[' <option-val-atom> ( ',' <option-val-atom> )* ']'
(defparse :ptag/option-val
  [ps]
  (let [tkn (:head ps)]
    (if (oval? tkn)
      (as-> ps ?ps
        (assoc ?ps :result tkn)
        (eat-token ?ps))
      (parse-list ps \[ \] \, :ptag/option-val-atom))))

;;; <option-val-atom> ::= <symbol> | <qvar> | <boolean>
(defparse :ptag/option-val-atom
  [ps]
  (let [tkn (:head ps)]
    (if (oval? tkn)
      (as-> ps ?ps
        (assoc ?ps :result tkn)
        (eat-token ?ps ::pass))
      (ps-throw ps "Expected a symbol, qvar or boolean." {:got tkn}))))
