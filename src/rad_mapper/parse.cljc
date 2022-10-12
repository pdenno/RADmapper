(ns rad-mapper.parse
  "Parse the JSONata-like message mapping language."
  (:require
   [clojure.pprint :as pp :refer [cl-format]]
   [clojure.string :as str :refer [index-of]]
   [clojure.set    :as set]
   [clojure.spec.alpha :as s]
   [failjure.core      :as fj]
   [rad-mapper.util :as util])
  #?(:cljs (:require-macros [rad-mapper.parse :refer [defparse defparse-auto]])))

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
;;;              For example use, see :ptag/MapSpec and parse-list.

;;; ToDo: Rethink the lexer/parser dichotomy. See Lezer, for example. The continue-tag stuff and regex is pretty bad!

(def ^:dynamic *debugging?* false)
(util/config-log (if *debugging?* :debug :info))

(def block-size "Number of lines to tokenize together. Light testing suggests 5 is about fastest." 5)

;;; ============ Tokenizer ===============================================================
(def keywords #{"express" "false" "function" "query" "true"})
(def ^:private syntactic? ; chars that are valid tokens in themselves.
  #{"[", "]", "(", ")", "{", "}", "=", ",", ".", ":", ";", "*", "+", "/", "-", "<", ">", "%", "&", "\\", "?" "`"})

(def ^:private long-syntactic? ; chars that COULD start a multi-character syntactic elements.
  #{"<", ">", "=", ".", ":", "/", "'", "?", "~", "!"}) ; Don't put eol-comment (//) here. \/ is for regex vs divide.

(def other? #{"(" ")" "{" "}" "[" "]" ":" "," ";"})

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
(def object-fns   (fn-maps ["$assert" "$each" "$error" "$keys" "$lookup" "$merge" "$sift" "$spread" "$type"]))
(def datetime-fns (fn-maps ["$fromMillis" "$millis" "$now" "$toMillis"]))
(def higher-fns   (fn-maps ["$filter" "$map" "$reduce" "$sift" "$single"]))
;;; Non-JSONata functions
(def file-fns     (fn-maps ["$read" "$readSpreadsheet"]))

(def builtin-fns (merge numeric-fns agg-fns boolean-fns array-fns string-fns object-fns datetime-fns higher-fns file-fns))
(def builtin? (-> builtin-fns keys (into ["$$" "$"]) set))
(def builtin-un-op #{"+", "-" :not})

;;; Binary operators. [+ - * / < > <= >= =]
(def numeric-operators    '{"%" bi/%, "*" bi/multiply, "+" bi/add, "-" bi/subtract, "/" bi/div}) ; :range is not one of these.
(def comparison-operators '{"<=" bi/lteq, ">=" bi/gteq, "!=" bi/!=, "<" bi/lt, "=" bi/eq, ">" bi/gt "in" bi/in})
(def boolean-operators    '{"and" and "or" or})
(def string-operators     '{"&" bi/concat})
;;; ToDo: Fix this; the apply things don't belong here. "." is a binary operator, no?
(def other-operators      '{"." bi/get-step :apply-filter bi/filter-step :apply-reduce bi/reduce-step "~>" bi/thread})
(def non-binary-op? '{".." bi/range "?" "?" ":=" bi/assign})

(def binary-op? (merge numeric-operators comparison-operators boolean-operators string-operators other-operators))

(def str2tkn-map (merge binary-op? non-binary-op?))

(defn str2tkn
  "JS doesn't have a character type, thus it is at least incumbent upon us to lex
   single character tokens as their corresponding tkn symbol.
   This is used for all operators"
  [s]
  (or (other? s)
      (let [res (get str2tkn-map s :not-found)]
        (if (= :not-found res)
          (throw (ex-info "Unknown token:" {:token s}))
          res))))

(s/def ::Jvar (s/keys :req-un [::jvar-name ::special?]))
(s/def ::Qvar (s/keys :req-un [::qvar-name]))
(s/def ::Field (s/keys :req-un [::field-name])) ; Used for fields (e.g. the a in $.a, and function params
(s/def ::TripleRole (s/keys :req-un [::role-name]))
(s/def ::Comment (s/keys :req-un [::text]))
(s/def ::RegExp (s/keys :req-un [::base ::flags]))
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
                    out "/"]
               (cond done? out
                     (> cnt in-len) nil
                     (empty? in) nil
                     :else (recur (inc cnt)
                                  (if (str/starts-with? in "\\") (subs in 2) (subs in 1))
                                  (str/starts-with? in "/")
                                  (if (str/starts-with? in "\\") (str out "\\" (subs in 1 2)) (str out (subs in 0 1))))))]
    (when (and base (not (index-of base new-line)))
      (let [flags (or (-> (re-matches #"(?s)([i,m,u,g,s,y]{1,6}).*" (subs st (count base))) second) "") ; Was (?sm). Does that make sense?
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
      {:raw "/" :tkn 'bi/divide}))

(defn single-quoted-string
  "Return a token map for a single-quoted string.
   Note that you get double-quoted strings for free from the Clojure reader."
  [s]
  (loop [chars (rest s)
         raw "'"
         res ""]
    (cond (empty? chars) (throw (ex-info "unbalanced single-quoted string" {:string s}))
          (= "'" (-> chars first str)) {:raw (str raw "'") :tkn res}
          (and (= "\\" (-> chars first str)) (= "'" (-> chars second str)))
          (recur (-> chars rest rest)
                 (str raw "\\'")
                 (str res "'"))
          :else (recur (rest chars)
                       (str raw (-> chars first str))
                       (str res (-> chars first str))))))
(defn read-qvar
  "read a query var"
  [st]
  (let [s (-> st str/split-lines first)]
    (if-let [[_ matched] (re-matches #"(\?[a-z,A-Z][a-zA-Z0-9\-\_]*).*" s)]
      {:raw matched :tkn {:typ :Qvar :qvar-name matched}}
      (fj/fail  "String does not start a legal query variable: %s" s))))

(defn read-triple-role
  "read a triple role"
  [st]
  (let [s (-> st str/split-lines first)]
    ;; ToDo: Only one '/' allowed!
    (if-let [[_ matched] (re-matches #"(\:[a-zA-Z][a-zA-Z0-9/\-\_]*).*" s)]
      (if (or (> (-> (for [x matched :when (= x  \/)] x) count) 1)
              (= \/ (nth matched (-> matched count dec))))
        (fj/fail "String does not start a legal triple role: %s" s)
        {:raw matched :tkn {:typ :TripleRole :role-name (util/read-str matched)}})
      (fj/fail "String does not start a legal triple role: %s" st))))

(defn read-long-syntactic
  "Return a map containing a :tkn and :raw string for 'long syntactic' lexemes,
   which include arbitray query vars and roles too."
  [st ws]
  (let [len (count st)
        c0  (nth st 0)
        c1  (and (> len 1) (nth st 1))]
    (when-let [res (cond (and (= c0 \/) (= c1 \/)) {:raw "//" :tkn :eol-comment},
                         (= c0 \/) (regex-or-divide st),
                         (= c0 \') (single-quoted-string st),
                         (and (= c0 \?) (re-matches #"[a-zA-Z]" (str c1))) (read-qvar st),
                         (and (= c0 \:) (re-matches #"[a-zA-Z]" (str c1))) (read-triple-role st),
                         (and (= c0 \:) (= c1 \=)) {:raw ":="},
                         (and (= c0 \<) (= c1 \=)) {:raw "<="},
                         (and (= c0 \>) (= c1 \=)) {:raw ">="},
                         (and (= c0 \=) (= c1 \=)) {:raw "=="},
                         (and (= c0 \.) (= c1 \.)) {:raw ".."},
                         (and (= c0 \!) (= c1 \=)) {:raw "!="},
                         (and (= c0 \~) (= c1 \>)) {:raw "~>"})]
      (cond-> res
        (#{":=" "<=" ">=" "==" ".." "!=" "~>"} (:raw res)) (assoc :tkn (str2tkn (:raw res)))
        true (assoc :ws ws)))))

(defn position-break
  "Return the first position in s containing a syntactic character or ws.
   Return nil if it contains none. The purpose is tokenize correctly things like 'a*b'
   where there is no intervening stuff. That said, 'a ?b : c' isn't a valid conditional
   expression owing to qvars."
  [s]
  (let [len (count s)]
    (loop [n 0]
      (let [c (str (get s n))]
        (cond
          (= len n) nil
          (long-syntactic? c) n
          (syntactic? c) n
          (#{\space \tab \newline} c) n
          :else (recur (inc n)))))))

(defn whitesp
  "Evaluates to whitespace at head of string or empty string if none."
  [s] ; https://stackoverflow.com/questions/15020669/clojure-multiline-regular-expression
  (if s (or (nth (re-matches #"(?s)(\s+).*$" s) 1) "") ""))

;;; ToDo: See split-at.
(defn get-more
  "Update :string-block and :line-seq by getting more lines from the line-seq lazy seq."
  [pstate]
  (as-> pstate ?ps
    (assoc  ?ps :string-block (->> ?ps :line-seq (take block-size) (map #(str % "\n")) (apply str)))
    (update ?ps :line-seq #(drop block-size %))))

(def ^:dynamic *debugging-tokenizer?* false)

;;; ToDo:  This is going to need work for multi-line tokens such as comments and strings.
;;; https://www.regular-expressions.info/modifiers.html (?s) allows  .* to match all characters including line breaks.
(defn one-token-from-string
  "Return a map with keys :ws, :raw and :tkn from the front of the argument string."
  [string-block line] ; line is just or error reporting.
  (let [ws (whitesp string-block)
        s (subs string-block (count ws))
        c (-> s first str)
        result
        (if (empty? s)
          {:ws ws :raw "" :tkn ::end-of-block},  ; Lazily pulling lines from line-seq; need more.
          (or  (and (empty? s) {:ws ws :raw "" :tkn ::eof})            ; EOF
               (when-let [[_ cm] (re-matches #"(?s)(/\*.*\*/).*" s)]   ; comment JS has problems with #"(?s)(\/\*(\*(?!\/)|[^*])*\*\/).*"
                 {:ws ws :raw cm :tkn {:typ :Comment :text cm}})
               (and (long-syntactic? c) (read-long-syntactic s ws))     ; /regex-pattern/ ++, <=, == etc.
               (when-let [[_ num] (re-matches #"(?s)(\d+(\.\d+(e[+-]?\d+)?)?).*" s)]
                 {:ws ws :raw num :tkn (util/read-str num)}),          ; number
               (when-let [[_ st] (re-matches #"(?s)(\"[^\"]*\").*" s)] ; string literal
                 {:ws ws :raw st :tkn (util/read-str st)})
               (when-let [[_ st] (re-matches #"(?s)(\`[^\`]*\`).*" s)] ; backquoted field
                 {:ws ws :raw st :tkn {:typ :Field :field-name st}})
               (and (syntactic? c) {:ws ws :raw (str c) :tkn (-> c str str2tkn)})          ; literal syntactic char.
               (let [pos (position-break s)
                     word (-> (subs s 0 (or pos (count s))) str/trim)]
                 (or ; We don't check for "builtin-fns"; as tokens they are just jvars.
                  (and (keywords word)    {:ws ws :raw word :tkn (keyword word)})
                  (when-let [[_ id] (re-matches #"^([a-zA-Z0-9\_]+).*" word)]              ; field or symbol in params map
                    {:ws ws :raw id :tkn (util/read-str id)})
                  (when-let [[_ id] (re-matches #"^(\$[a-zA-Z][A-Za-z0-9\_]*).*" word)]    ; jvar
                    {:ws ws :raw id :tkn {:typ :Jvar :jvar-name id}})
                  (when-let [[_ id] (re-matches #"^(\${1,3}).*" word)]                     ; $, $$, $$$.
                    {:ws ws :raw id :tkn {:typ :Jvar :jvar-name id :special? true}})
                  (when-let [[_ id] (re-matches #"^(:[a-zA-Z][a-zA-Z0-9\-\_]*).*" word)]   ; triple role
                    {:ws ws :raw id :tkn {:typ :TripleRole :role-name id}})))
               (throw (ex-info "Char starts no known token:" {:raw c :line line}))))]
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
(defn line-msg
  "Return a string 'Line <n>: ' or 'Line <n>: <msg', depending on args."
  ([pstate] (line-msg pstate ""))
  ([pstate msg & args]
   (if (not-empty args)
     (cl-format nil "Line ~A: ~A ~{~A~^, ~}~% ~A" (-> pstate :tokens first :line) msg args pstate)
     (cl-format nil "Line ~A: ~A~% ~A"            (-> pstate :tokens first :line) msg pstate))))

(def diag (atom nil))

(defn ps-throw
  "A special throw to eliminate to clean up line-seq and "
  [pstate msg data]
  (as-> pstate ?ps
    (dissoc ?ps :line-seq) ; So REPL won't freak out over the reader being closed...
    (reset! diag {:pstate ?ps :msg msg :data data})
    (throw (ex-info (line-msg ?ps msg) data))))

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

(defn match-head
  "Return true if token matches test, which is a string, character, fn or regex."
  [pstate test]
  (let [head (:head pstate)]
    (cond (= test ::pass) true,
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
   (when *debugging?* (cl-format *out* "~AEAT ~A  (type ~A)~%"
                                 (util/nspaces (* 3 (-> pstate :tags count dec)))
                                 (pp/write (:head pstate) :readably false :stream nil)
                                 (-> pstate :head util/class-name name)))
   (match-head pstate test)
   ;; The actual work of eating a token
   (let [ps1 (if (-> pstate :tokens empty?) (refresh-tokens pstate) pstate) ; 3 of 3, refreshing tokens.
         next-up (-> ps1 :tokens first)]
     #_(cl-format *out* "~%Eating ~A, next-up = ~A" (:head ps1) (:tkn next-up))
     (as-> ps1 ?ps
       (assoc ?ps :head (if next-up (:tkn next-up) ::eof)) ; One of two places :head is set; the other is make-pstate.
       (assoc ?ps :tokens (-> ?ps :tokens rest vec))       ; 2 of 3, setting :tokens.
       (ps-assert ?ps)))))

(defn token-vec [ps] (into (-> ps :head vector) (mapv :tkn (:tokens ps))))

(def balanced-map "What balances with the opening syntax?" { "{" "}", "(" ")", "[" "]", :2d-array-open :2d-array-close})
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
         (eat-token ~test))))

(defn store
  "This and recall are used to keep parsed content tucked away on the parse state object."
  ([ps key] (store ps key (:result ps)))
  ([ps key val] (assoc-in ps [:local 0 key] val)))

(defn recall
  "This and store are used to keep parsed content tucked away on the parse state object."
  [ps key]
  (-> ps :local first key))

(defn parse-dispatch [tag & _] tag)

(defmulti parse #'parse-dispatch)

(defn make-pstate
  "Make a parse state map and start tokenizing."
  [reader-or-str]
  (as-> {:head nil ; In this order for easy debugging.
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
       (fj/fail "Tokens remain: %s." (:tokens pstate))
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
     (cl-format *out* "~%>>>>>>>>>>>>>> parse-list (~A) >>>>>>>>>>>>>>>>>>" item-tag))
   (let [final-ps
         (as-> pstate ?ps
           (eat-token ?ps char-open)
           (assoc-in ?ps [:local 0 :items] [])
           (loop [ps ?ps]
             (cond
               (= ::eof (:head ps))
               (ps-throw ps "parsing a list" {:tag item-tag}),
               (= char-close (:head ps))
               (as-> ps ?ps1
                 (eat-token ?ps1)
                 (assoc ?ps1 :result (recall ?ps1 :items))),
               :else
               (as-> ps ?ps1
                 (parse item-tag ?ps1)
                 (update-in ?ps1 [:local 0 :items] conj (:result ?ps1))
                 (recur (cond-> ?ps1 (= char-sep (:head ?ps1)) (eat-token char-sep)))))))]
     (when *debugging?*
       (println "\nCollected" (:result final-ps))
       (cl-format *out* "\n<<<<<<<<<<<<<<<<<<<<< parse-list (~A) <<<<<<<<<<<<<<<<<<<<<<<" item-tag))
     final-ps)))

;;; ToDo I'm not sure I use any of these autos!
;;; <builtin-num-bin-op> ::= + | - | * | / | div | mod ; ToDo: div and mod are grammar things?
(def builtin-num-bin-op '#{bi/add bi/subtract bi/multiply bi/divide bi/% :div :mod})
(defparse-auto :ptag/builtin-num-bin-op builtin-num-bin-op)

;;; :range is NOT a bin-op.
;;;  <builtin-bin-op> ::= . | & | < | > | <= | >= | == | = | != | and | or
(def builtin-bin-op
          ; \.  \<     \>   :<=    :>= :== \=    :not=    \&      :and :or
  (into '#{bi/get-step bi/lt bi/gt bi/le bi/ge :== bi/eq bi/neq bi/concat :and :or}
        builtin-num-bin-op))

(defparse-auto :ptag/builtin-bin-op builtin-bin-op)

;;; <builtin-un-op> ::= "not" | "+" | "-"
(defparse-auto :ptag/builtin-un-op builtin-un-op)

;;; <builtin-op> ::= <builtin-bin-op> | <builtin-un-op>
(def builtin-op (set/union builtin-bin-op builtin-un-op))
(defparse-auto :ptag/builtin-op builtin-op)
(defparse-auto :ptag/builtin-fn builtin-fns)

(defn jvar? [x]        (= (:typ x) :Jvar))
(defn qvar? [x]        (= (:typ x) :Qvar))
(defn triple-role? [x] (= (:typ x) :TripleRole))
(defn field? [x]       (= (:typ x) :Field))
(defn regex? [x]       (= (:typ x) :RegExp))
(defn literal? [tkn]
  (or (string? tkn)
      (number? tkn)
      (#{:true :false} tkn)
      (regex? tkn)))

(s/def ::ps  (s/keys :req-un [::tokens ::head]))
(s/def ::tokens (s/and vector? (s/coll-of ::token)))
(s/def ::token  (s/and map? #(contains? % :tkn) #(-> % :tkn nil? not)))
(s/def ::head #(not (nil? %)))

(def bin-op-plus? (-> (merge binary-op? '{"[" "[" "{" "{"}) vals set))

(s/def ::BinOpSeq (s/keys :req-un [::seq]))
;;;=============================== Grammar ===============================
;;; <exp> ::= <base-exp> (  <bin-op-continuation>  | '?' <conditional-tail> ) ?
(defparse :ptag/exp
  [ps]
  (let [base-ps (-> (parse :ptag/base-exp ps) (store :operand-1))]
    (cond (= "?" (:head base-ps))
          (parse :ptag/conditional-tail base-ps :predicate (:result base-ps)),

          (-> base-ps :head bin-op-plus?)
          (as-> base-ps ?ps
            (parse :ptag/bin-op-continuation ?ps)
            (assoc ?ps :result {:typ :BinOpSeq
                                :seq (into (vector (recall ?ps :operand-1))
                                           (-> ?ps :result :op-operand-seq))}))
          :else base-ps)))

(s/def ::OpOperandSeq (s/keys :req-un [::op-operand-seq]))
(defparse :ptag/bin-op-continuation
  [ps]
  (loop [ps ps
         oseq []]
    (let [bin-op (-> ps :head bin-op-plus?)]
    (if (not bin-op)
      (assoc ps :result {:typ :OpOperandSeq :op-operand-seq oseq})
      (let [p (if (#{"[" "}"} (:head ps))
                (as-> ps ?ps
                  (store ?ps :operator (-> ?ps :head bin-op-plus?))
                  (parse :ptag/base-exp ?ps :operand-2? true))
                (as-> ps ?ps
                  (store ?ps :operator bin-op)
                  (eat-token ?ps) ; ToDo: I think this is necessarily \.
                  (parse :ptag/base-exp ?ps :operand-2? true)))]
        (recur p (-> oseq (conj (recall p :operator)) (conj (:result p)))))))))

;;; ToDo: qvar here might make it permissive of nonsense. Needs thought.
;;; <base-exp> ::= <delimited-exp> | (<builtin-un-op> <exp>) | <construct-def> | <fn-call> | <literal> | <jvar-decl> | <field> | <jvar> | <qvar>
(defparse :ptag/base-exp
  [ps & {:keys [operand-2?]}]
  (let [tkn  (:head ps)
        ps   (look ps 1)
        tkn2 (-> ps :look (get 1))]
    (cond (#{"{" "[" "("} tkn)                  (parse :ptag/delimited-exp ps :operand-2? operand-2?) ; <delimited-exp>
          (builtin-un-op tkn)                   (parse :ptag/unary-op-exp ps)  ; <unary-op-exp>
          (#{:function :query :express} tkn)    (parse :ptag/construct-def ps) ; <construct-def>
          (and (= "(" tkn2)
               (or (builtin-fns tkn)
                   (jvar? tkn)))                (parse :ptag/fn-call ps)       ; <fn-call>
          (literal? tkn)                        (parse :ptag/literal ps)       ; <literal>
          (and (jvar? tkn) (= tkn2 'bi/assign)) (parse :ptag/jvar-decl ps)     ; <jvar-decl>
          (or (jvar? tkn)
              (qvar? tkn)
              (field? tkn))                   (as-> ps ?ps                   ; <jvar>, <qvar> or `backquoted field`
                                                (assoc ?ps :result tkn)
                                                (eat-token ?ps))
          (symbol? tkn)                       (as-> ps ?ps                   ; <field> (not part of param-struct)
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
        "("  (parse :ptag/primary    ps)
        "["  (parse :ptag/filter-exp ps) ; Includes aref.
        "{"  (parse :ptag/reduce-exp ps))
      (case head
        "("  (parse :ptag/primary ps)
        "["  (parse :ptag/range|array-exp ps)
        "{"  (parse :ptag/obj-exp ps)))))

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
        :else                  (ps-throw ps "expected a path field" {:got (:head ps)})))

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
    (parse-list ?ps "{" "}" "," :ptag/obj-kv-pair) ; Operand added in :ptag/delimited-exp
    (if in-express?
      (assoc ?ps :result {:typ :ExpressMap :kv-pairs (:result ?ps)})
      (assoc ?ps :result {:typ :ObjExp     :kv-pairs (:result ?ps)}))))

;;; <reduce-exp> ::= '{' <obj-kv-pair>* '}'
(defparse :ptag/reduce-exp
  [ps]
  (as-> ps ?ps
    (parse-list ?ps "{" "}" "," :ptag/obj-kv-pair) ; Operand will be added in :ptag/delimited-exp
    (assoc ?ps :result {:typ :ReduceExp :kv-pairs (:result ?ps)})))

;;; ToDo: Perhaps for express body, key could be any expression?
;;; <map-pair> ::=  ( <string> | <qvar> ) ":" <exp>
(s/def ::KVPair (s/keys :req-un [::key ::val]))
(s/def ::ExpressKVPair (s/keys :req-un [::key ::val]))
(defparse :ptag/obj-kv-pair
  [pstate]
  (as-> pstate ?ps
    (if (-> ?ps :head qvar?)
      (as-> ?ps ?ps1
        (store ?ps1 :key (:head ?ps))
        (eat-token ?ps1))
      (as-> ?ps ?ps1
        (parse :ptag/string ?ps1)
        (store ?ps1 :key)))
    (eat-token ?ps ":")
    (parse :ptag/exp ?ps)
    (assoc ?ps :result {:typ (if in-express? :ExpressKVPair :KVPair)
                        :key (recall ?ps :key)
                        :val (:result ?ps)})))

(s/def ::RangeExp (s/keys :req-un [::start ::stop]))
;;; <range|array-exp> ::=  <range-exp> | <array>
(defparse :ptag/range|array-exp
  [ps]
    (let [tvec (token-vec ps)
          close-pos (when (= (first tvec) "[") (find-balanced-pos tvec "]"))
          range-pos (when close-pos (find-token tvec 'bi/range :stop-pos close-pos))]
    (if (and close-pos range-pos (< range-pos close-pos)) ; ToDo Heuristic! (Replace with try?)
      (as-> ps ?ps
        (eat-token ?ps "[")
        (parse :ptag/exp ?ps)
        (store ?ps :start)
        (eat-token ?ps 'bi/range)
        (parse :ptag/exp ?ps)
        (store ?ps :stop)
        (eat-token ?ps "]")
        (assoc ?ps :result {:typ :RangeExp
                            :start (recall ?ps :start)
                            :stop (recall ?ps :stop)}))
      (parse :ptag/array ps))))

;;; <conditional-tail> ::=  <exp> ':' <exp>
(s/def ::ConditionalExp (s/keys :req-un [::predicate ::exp1 ::exp2]))
(defparse :ptag/conditional-tail
  [ps & {:keys [predicate]}]
  (as-> ps ?ps
    (eat-token ?ps "?")
    (parse :ptag/exp ?ps)
    (store ?ps :then)
    (eat-token ?ps ":")
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
    (parse-list ?ps "(" ")" ";" :ptag/exp)
    (assoc ?ps :result {:typ :Primary :exps (:result ?ps)})))

;;; <filter-exp> := '[' <exp> ']'
(s/def ::ApplyFilter (s/keys :req-un [::body])) ; This accommodates aref expressions too.
(defparse :ptag/filter-exp
  [ps]
  (as-> ps ?ps
    (eat-token ?ps "[")
    (parse :ptag/exp ?ps)
    (eat-token ?ps "]")
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
    (eat-token ?ps 'bi/assign)
    (parse :ptag/exp ?ps)
    (store ?ps :init-val)
    (assoc ?ps :result {:typ :JvarDecl
                        :var (recall ?ps :jvar)
                        :init-val (recall ?ps :init-val)})))

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
    (if (string? tkn)
      (-> ps (assoc :result tkn) eat-token)
      (ps-throw ps "expected a string literal" {:got tkn}))))

;;; <literal> ::= string | number | 'true' | 'false' | regex | <obj> | <array>
(defparse :ptag/literal
  [ps]
  (let [tkn (:head ps)]
    (cond (literal? tkn) (-> ps (assoc :result tkn) eat-token), ; :true and :false will be rewritten
          (= tkn "{")     (parse :ptag/obj-exp ps),
          (= tkn "[")     (parse :ptag/array ps),
          :else (ps-throw ps "expected a literal string, number, 'true', 'false' regex, obj, range, or array."
                          {:got tkn}))))

(s/def ::Array (s/keys :req-un [::exprs]))
(defparse :ptag/array
  [ps]
  (as-> ps ?ps
    (parse-list ?ps "[" "]" "," :ptag/exp)
    (assoc ?ps :result {:typ :Array :exprs (:result ?ps)})))

;;; fn-call ::=  <jvar> '(' <exp>? (',' <exp>)* ')'
(s/def ::FnCall (s/keys :req-un [::fn-name ::args]))
(defparse :ptag/fn-call
  [ps]
    (as-> ps ?ps
      (store ?ps :fn-name (:head ?ps))
      (eat-token ?ps jvar?)
      (parse-list ?ps "(" ")" "," :ptag/exp)
      (assoc ?ps :result {:typ :FnCall
                          :fn-name (-> ?ps (recall :fn-name) :jvar-name)
                          :args (:result ?ps)})))

;;; <query-patterns> ::= <q-pattern>+
(defparse :ptag/query-patterns
  [ps]
  (let [ps-one (parse :ptag/q-pattern ps)]
    (loop [result (vector (:result ps-one))
           ps ps-one]
      (if (not= "[" (:head ps))
        (assoc ps :result result)
        (let [ps (parse :ptag/q-pattern ps)]
          (recur (conj result (:result ps))
                 ps))))))

;;; <q-pattern> :: <q-pattern-triple> | <q-pattern-pred>
(defparse :ptag/q-pattern
  [ps]
  (let [ps   (look ps 1)
        tkn2 (-> ps :look (get 1))]
    (if (= tkn2 "(")
      (parse :ptag/q-pattern-pred ps)
      (parse :ptag/q-pattern-triple ps))))

;;; <q-pattern-pred> :: '[' '(' <query-predicate> ')' ']'
(s/def ::QueryPred (s/keys :req-un [::exp]))
(defparse :ptag/q-pattern-pred
  [ps]
  (as-> ps ?ps
    (eat-token ?ps "[")
    (eat-token ?ps "(")
    (parse :ptag/fn-call ?ps)
    (eat-token ?ps ")")
    (eat-token ?ps "]")
    (assoc ?ps :result {:typ :QueryPred :exp (:result ?ps)})))

;;; <q-pattern-triple> :: '[' <QueryVar> <TripleRole> (<QueryVar> | <exp>) ']'
(s/def ::QueryTriple (s/keys :req-un [::ent ::rel ::val-exp]))
(defparse :ptag/q-pattern-triple
  [ps]
  (as-> ps ?ps
    (eat-token ?ps "[")
    (store ?ps :ent (:head ?ps))
    (eat-token ?ps qvar?)
    (store ?ps :role (:head ?ps))
    (eat-token ?ps #(or (triple-role? %) (qvar? %)))
    (if (qvar? (:head ?ps))
      (as-> ?ps ?ps1
          (store ?ps1 :third (:head ?ps1))
          (eat-token ?ps1))
      (as-> ?ps ?ps1
          (parse :ptag/exp ?ps1)
          (store ?ps1 :third)))
    (eat-token ?ps "]")
    (assoc ?ps :result {:typ :QueryTriple
                        :ent (recall ?ps :ent)
                        :rel (recall ?ps :role)
                        :val-exp (recall ?ps :third)})))

;;; <fn-def> ::= 'function' '(' <jvar>? [',' <jvar>]* ')' '{' <exp> '}'
(s/def ::FnDef (s/keys :req-un [::vars ::body]))
(defparse :ptag/fn-def
  [ps]
  (as-> ps ?ps
    (eat-token ?ps :function)
    (parse-list ?ps "(" ")" "," :ptag/jvar)
    (store ?ps :jvars)
    (eat-token ?ps "{")
    (parse :ptag/exp ?ps)
    (store ?ps :body)
    (eat-token ?ps "}")
    (assoc ?ps :result {:typ :FnDef
                        :vars (recall ?ps :jvars)
                        :body (recall ?ps :body)})))

;;; <query-def> ::= 'query '(' <jvar>? [',' <jvar|options>]* ')' '{' <query-patterns> '}'
(s/def ::QueryDef (s/keys :req-un [::params ::triples]))
(defparse :ptag/query-def
  [ps]
  (as-> ps ?ps
    (eat-token ?ps :query)
    (parse-list ?ps "(" ")" "," :ptag/jvar)
    (store ?ps :params)
    (eat-token ?ps "{")
    (parse :ptag/query-patterns ?ps)
    (eat-token ?ps "}")
    (assoc ?ps :result {:typ :QueryDef
                        :params (recall ?ps :params)
                        :triples (:result ?ps)})))

;;; ToDo: I don't care where you put the options.
;;;       I don't care whether there is more than one options map.
;;;       Should I?
(defparse :ptag/jvar|options
  [ps]
  (if (-> ps :head jvar?)
    (as-> ps ?ps
      (assoc ?ps :result (:head ?ps))
      (eat-token ?ps))
    (parse :ptag/options-map ps)))

;;; ToDo: These aren't jvars, probably want some sort of optional parameter syntax.
;;; <express-def> ::= 'express' '(' <jvar>? [',' <jvar>]* ')' '{' <exp> '}'
(s/def ::ExpressDef (s/keys :req-un [::params ::body]))
(defparse :ptag/express-def
  [ps]
  (binding [in-express? true]
    (as-> ps ?ps
      (eat-token ?ps :express)
      (parse-list ?ps "(" ")" "," :ptag/jvar|options)
      (store ?ps :params)
      (eat-token ?ps "{")
      (parse :ptag/obj-exp ?ps)
      (store ?ps :body)
      (eat-token ?ps "}")
      (assoc ?ps :result {:typ :ExpressDef
                          :params (recall ?ps :params)
                          :body (recall ?ps :body)}))))

(s/def ::ImmediateUse (s/keys :req-un [::def ::args]))
;;; <construct-def> ::= ( <fn-def> | <query-def> | <express-def> )( '(' <exp>* ')' )?
(defparse :ptag/construct-def
  [ps]
  (let [ps (case (:head ps)
             :function  (parse :ptag/fn-def ps),         ; <fn-def>
             :query     (parse :ptag/query-def ps),      ; <query-def>
             :express   (parse :ptag/express-def ps))]   ; <express-def>
    (if (and (= "(" (:head ps)) (#{:FnDef :QueryDef} (-> ps :result :typ)))
      ;; This part to wrap it in a ImmediateUse
      (as-> ps ?ps
        (store ?ps :def)
        (parse-list ?ps "(" ")" "," :ptag/exp)
        (store ?ps :args)
        (assoc ?ps :result {:typ :ImmediateUse
                            :def (recall ?ps :def)
                            :args (recall ?ps :args)}))
      ;; This if it is just a definition (which will be assigned to a $id).
      ps)))

;;; The next four 'options' rules are just for query and express keyword parameters (so far).
(s/def ::OptionsMap (s/keys :req-un [::kv-pairs]))
;;; <options-struct> '{' <options-kv-pair>* '}'
(defparse :ptag/options-map
  [ps]
  (as-> ps ?ps
    (parse-list ?ps "{" "}" "," :ptag/option-kv-pair)
    (assoc ?ps :result {:typ :OptionsMap :kv-pairs (:result ?ps)})))

(s/def ::OptionKeywordPair (s/keys :req-un [::key ::val]))
;;; <options-kv-pair> ::= <symbol> ':' <option-val>
(defparse :ptag/option-kv-pair
  [ps]
  (as-> ps ?ps
    (store ?ps :key (:head ?ps))
    (eat-token ?ps symbol?)
    (eat-token ?ps ":")
    (parse :ptag/option-val ?ps)
    (store ?ps :val)
    (assoc ?ps :result {:typ :OptionKeywordPair
                        :key (recall ?ps :key)
                        :val (recall ?ps :val)})))

(defn oval? [obj]
  (or (symbol? obj) (qvar? obj) (#{:true :false} obj)))

(s/def ::OptionVal (s/keys :req-un [::exp]))
;;; <option-val> ::= <option-val-atom> | '[' <option-val-atom> ( ',' <option-val-atom> )* ']'
(defparse :ptag/option-val
  [ps]
  (let [tkn (:head ps)]
    (if (oval? tkn)
      (as-> ps ?ps
        (assoc ?ps :result tkn)
        (eat-token ?ps))
      (parse-list ps "[" "]" "," :ptag/option-val-atom))))

;;; <option-val-atom> ::= <symbol> | <qvar> | <boolean>
(defparse :ptag/option-val-atom
  [ps]
  (let [tkn (:head ps)]
    (if (oval? tkn)
      (as-> ps ?ps
        (assoc ?ps :result tkn)
        (eat-token ?ps))
      (ps-throw ps "Expected a symbol, qvar or boolean." {:got tkn}))))
