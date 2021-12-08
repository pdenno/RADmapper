(ns pdenno.rad-mapper.parse
  "Parse the JSONata-like message mapping language."
  (:require
   [pdenno.rad-mapper.util :as util]
   [clojure.pprint :refer (cl-format)]
   [clojure.string :as str]
   [clojure.set    :as sets]
   [clojure.spec.alpha :as s]))

;;; The 'defparse' parsing functions pass around complete state. 
;;; The 'parse state' (AKA pstate) is a map with keys:
;;;   :result  - the parse structure from the most recent call to (parse :<some-rule-tag> pstate)
;;;   :tokens  - tokenized content that needs to be parsed into :model. First on this vector is also :tkn.
;;;   :stack   - a stack of tags indicating where in the grammar it is parsing (used for debugging)
;;;   :tkn     - current token, not yet consumed. It is also the first token on :tokens. 
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
;;;   2) Simplify JaVar. We don't care about bound? ??? Or is there useful checking to be done there?
;;;   3) Perhaps maintaining record types for JaVar and JaField was a waste of time?

(def ^:dynamic *debugging?* false)
(def diag (atom nil))

;;; ============ Tokenizer ===============================================================
(def keywords-basic
  #{"alias" "and" "else" "elseif" "endif" "false" "for" "function" "if" "in" "int" "library" "list" "metadata"
    "of" "or" "return" "source" "string" "target" "then" "transform" "true" "where"})

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
(def extended-fns '{"$readFile" bi/$readFile, "$readSpreadsheet" bi/$readSpreadsheet})

(def builtin-fns (merge numeric-fns agg-fns boolean-fns array-fns string-fns object-fns datetime-fns higher-fns extended-fns))
(def keywords (sets/union keywords-basic (-> builtin-fns vals set))) ; ToDo necessary? Why not just let it be = keywords-basic?
(def builtin-type #{:int :string})

(def numeric-operators '{\% bi/%, \* bi/*, \+ bi/+, \- bi/-, \/ bi//}) ; :range is not one of these.
(def comparison-operators '{:<= <=, :>= >=, :!= not=, \< <, \= =, \> >, "in" bi/in})
(def boolean-operators '{:and and :or or})
(def string-operators '{\& bi/&})
(def path-operator '{\. bi/access})
;;; ToDo Re: binary-op? see also http://docs.jsonata.org/other-operators; I'm not doing everything yet.
(def binary-op? (merge numeric-operators comparison-operators boolean-operators string-operators path-operator))

(def ^:private syntactic ; chars that are valid tokens in themselves. 
  #{\[, \], \(, \), \{, \}, \=, \,, \., \:, \;, \*, \+, \/, \-, \<, \>, \%, \&, \\, \?})

(def ^:private long-syntactic ; chars that COULD start a multi-character syntactic elements. 
  #{\<, \>, \=, \., \:, \/, \'}) ; Don't put eol-comment (//) here. \/ is for regex vs divide.

(defrecord JaVar [var-name bound?])
(defrecord JaField [field-name])
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
          
;;; ToDo multi-line comment (e.g. /* ... */ would go in here, sort of. 
(defn read-long-syntactic [st ws]
  (let [len (count st)
        c0  (nth st 0)
        c1  (and (> len 1) (nth st 1))]
    (when-let [result (cond (and (= c0 \/) (= c1 \/)) {:raw "//" :tkn :eol-comment}
                            (= c0 \/) (regex-or-divide st)
                            (= c0 \') (single-quoted-string st)
                            (and (= c0 \<) (= c1 \=)) {:raw "<=" :tkn :<=}
                            (and (= c0 \>) (= c1 \=)) {:raw ">=" :tkn :>=}
                            (and (= c0 \=) (= c1 \=)) {:raw "==" :tkn :==}
                            (and (= c0 \:) (= c1 \=)) {:raw ":=" :tkn :binding}
                            (and (= c0 \.) (= c1 \.)) {:raw ".." :tkn :range}
                            (and (= c0 \!) (= c1 \=)) {:raw "!=" :tkn :!=})]
      (assoc result :ws ws))))

(defn position-break 
  "Return the first position in s containing a syntactic character, ws,
   or nil if it contains none."
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

;;; https://www.regular-expressions.info/modifiers.html (?s) allows  .* to match all characters including line breaks. 
(defn token-from-string
  "Return a map with keys :ws, :raw and :tkn from the front of the argument string."
  [stream line]
  (let [ws (whitesp stream)
        s (subs stream (count ws))
        c (first s)]
    ;(cl-format *out* "~%ws = ~S~%c = ~S~%STREAM = ~S" ws c stream)
    (or  (and (empty? s) {:ws ws :raw "" :tkn :eof})                    ; EOF
         (when-let [[_ cm] (re-matches #"(?s)(\/\/[^\n]*).*" s)]        ; EOL comment
           {:ws ws :raw cm :tkn (->JaEOLcomment cm)})
         (and (long-syntactic c) (read-long-syntactic s ws))    ; /regex-pattern/ ++, <=, == etc. 
         (and (syntactic c) {:ws ws :raw (str c) :tkn c})            ; literal syntactic char.
         (when-let [[_ num] (re-matches #"(?s)(\d+(\.\d+(e[+-]?\d+)?)?).*" s)]
           {:ws ws :raw num :tkn (read-string num)}),                   ; number
         (when-let [[_ st] (re-matches #"(?s)(\"[^\"]*\").*" s)]        ; string literal
           {:ws ws :raw st :tkn (read-string st)})
         (let [pos (position-break s)
               word (subs s 0 (or pos (count s)))]
           (or
            ;(and (builtin-fns word) {:ws ws :raw word :tkn word})
            (and (keywords word) {:ws ws :raw word :tkn (keyword word)})
            (when-let [[_ id] (re-matches #"^([\$,a-zA-Z][A-Za-z0-9\_\?]*).*" word)] ; two types of 'identiers': fields, $ids
              (if (str/starts-with? id "$")
                {:ws ws :raw id :tkn (->JaVar id nil)}
                {:ws ws :raw id :tkn (->JaField id)}))))
         (throw (ex-info "Char starts no known token: " {:raw c :line line})))))

(defn tokenize
  "Return a vector of tokens. A token is a map with keys :tkn, :line :col."
  [in-string]
  (loop [s in-string
         tkns []
         line 1
         col 1]
    (let [lex (token-from-string s line) ; Returns a map with keys :ws :raw and :tkn.
          new-lines (count (re-seq #"\n" (:ws lex))) ; :ws is in front of token. 
          col (if (> new-lines 0)
                (- (count (:ws lex)) (str/last-index-of (:ws lex) "\n"))
                (+ (count (:ws lex)) col))]
      (if (= :eof (:tkn lex))
        (conj tkns {:tkn :eof :line line :col col})
        (recur
         (subs s (+ (count (:raw lex)) (count (:ws lex))))
         (conj tkns {:tkn (:tkn lex) :line (+ line new-lines) :col col})
         (+ line new-lines)
         (+ col (count (:raw lex))))))))

;;; ============ Parser Utilities ============================================================
(defn look
  "Returns a token, not the pstate."
  [pstate n]
  (if (>= n (count (:tokens pstate)))
    :eof
    (-> (nth (:tokens pstate) n) :tkn)))

(defn match-tkn
  "Return true if token matches test, which is a string, character, fn or regex."
  [test tkn]
  (cond (= test tkn) true
        (map? test) (test tkn)
        (set? test) (test tkn)
        (fn? test) (test tkn)
        (instance? java.util.regex.Pattern test) (re-matches test tkn)
        :else false))

(defn eat-token-aux
  "The actual work of eating a token."
  [pstate]
  (let [next-up (-> pstate :tokens second)]
    (-> pstate
        (assoc :tkn  (or (:tkn next-up) :eof))
        (assoc :line (:line next-up))
        (assoc :col  (:col next-up))
        (assoc :tokens (-> pstate :tokens rest vec)))))

(defn eat-token
  "Move head of :tokens to :tkn ('consuming' the old :tkn) With 2 args, test :tkn first."
  ([pstate] (eat-token-aux pstate))
  ([pstate test]
   (when *debugging?* (cl-format *out* "~%~AEAT ~A" (util/nspaces (* 3 (-> pstate :stack count))) (:tkn pstate)))
   (if (match-tkn test (:tkn pstate))
       (eat-token-aux pstate)
       (throw (ex-info "eat-token test failed" {:test test :tkn (:tkn pstate) :pstate pstate})))))

(defn token-vec [pstate] (mapv :tkn (:tokens pstate)))

(def balanced-map "What balances with the opening syntax?" { \{ \}, \( \), \[ \] :2d-array-open :2d-array-close})
(def balanced-inv (sets/map-invert balanced-map))
  
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
     (when *debugging?* (cl-format *out* "~%~A==> ~A" (util/nspaces (* 3 (-> ~pstate :stack count))) ~tag))
     (if (<= (:call-count ~pstate) (:max-calls ~pstate))
       (as-> ~pstate ~pstate
         (update ~pstate :call-count inc)
         (update ~pstate :stack conj ~tag)
         (update ~pstate :local #(into [{:locals-for ~tag}] %))
         ~@body
         (if (not-empty (:stack ~pstate)) (update ~pstate :stack pop) ~pstate)
         (update ~pstate :local #(vec (rest %)))
         (do (when *debugging?* (cl-format *out* "~%~A<-- ~A   ~S"
                                          (util/nspaces (* 3 (-> ~pstate :stack count)))
                                          ~tag
                                          (:result ~pstate)))
             ~pstate))
       (throw (ex-info "Exceeded parse call-count (Bug in a defparse?)." {:pstate ~pstate})))))

;;; Abbreviated for simple forms such as builtins. 
(defmacro defparse-auto [tag test]
  `(defparse ~tag
     [pstate#]
     (-> pstate#
         (assoc :result (:tkn pstate#))
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
  "Make a parse state map from tokens, includes separating comments from code."
  [tokens+comments]
  (let [tokens   (remove #(instance? JaEOLcomment (:tkn %)) tokens+comments)
        comments (filter #(instance? JaEOLcomment (:tkn %)) tokens+comments)]
  {:tokens (vec tokens)
   :tkn (-> tokens first :tkn)
   :stack []
   :local []
   :call-count 0
   :max-calls (* (count tokens) 20)
   :comments comments}))

(defn parse-string
  "Toplevel parsing function.
  NB: This function is typically used for debugging with a literal string argument.
  If the text is intended to have JS escape, \\, in it, it has to be escaped!"
  ([str] (parse-string :ptag/CodeBlock str))
  ([tag str]
   (let [pstate (->> str tokenize make-pstate (parse tag))]
     (if (not= (:tkn pstate) :eof)
       (do (when *debugging?* (cl-format *out* "~2%*** Tokens remain. pstate=~A ~%" pstate))
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
  (parse-string :ptag/CodeBlock (-> filename slurp esc-esc)))

(defn parse-ok?
  "Return true if the string parses okay."
  [tag text]
  (as-> (parse-string tag text) ?pstate
    (and (= :eof (-> ?pstate :tokens first :tkn))
         (or (not (contains? (s/registry) tag))
             (s/valid? tag (:result ?pstate))))))

;;;========================= Implementation of Grammar ==========================
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
               (= :eof (:tkn ps))
               (throw (ex-info "parsing a list" {:tag parse-tag :pstate ps})),
               (= char-close (:tkn ps))
               (as-> ps ?ps1
                 (eat-token ?ps1)
                 (assoc ?ps1 :result (recall ?ps1 :items))),
               :else
               (as-> ps ?ps1
                 (parse parse-tag ?ps1)
                 (update-in ?ps1 [:local 0 :items] conj (:result ?ps1))
                 (recur (cond-> ?ps1 (= char-sep (:tkn ?ps1)) (eat-token char-sep)))))))]
     (when *debugging?*
       (println "\nCollected" (:result final-ps))
       (println "\n<<<<<<<<<<<<<<<<<<<<< parse-list <<<<<<<<<<<<<<<<<<<<<<<"))
     final-ps)))

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
              (= :eof (:tkn ps))  (throw (ex-info "parsing a terminated list" {:tag parse-tag :pstate ps}))
              (term-fn (:tkn ps)) (assoc ps :result (recall ps :items)),
              :else
              (as-> ps ?ps
                (parse parse-tag ?ps)
                (update-in ?ps [:local 0 :items] conj (:result ?ps))
                (recur (cond-> ?ps (sep-fn (:tkn ?ps)) (eat-token #{\,\;})))))))]
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

;;; <builtin-num-un-op> ::= + | -
(def builtin-num-un-op #{\+, \-})
(defparse-auto :ptag/builtin-num-un-op builtin-num-un-op)

;;; <builtin-un-op> ::= "not" | <builtin-num-un-op>
(def builtin-un-op (conj builtin-num-un-op :not))
(defparse-auto :ptag/builtin-un-op builtin-un-op)

;;; <builtin-op> ::= <builtin-bin-op> | <builtin-un-op>
(def builtin-op (sets/union builtin-bin-op builtin-un-op))
(defparse-auto :ptag/builtin-op builtin-op)
(defparse-auto :ptag/builtin-fn builtin-fns)

(defn id?    [x] (instance? JaVar x))
(defn field? [x] (instance? JaField x))

;;;=============================== Grammar ===============================
(s/def ::CodeBlock (s/keys :req-un [::body]))
;;; <code-block> ::= ( 'transform' <var> '{' <preamble> '}' )? ( '('  <global-params>* <obj>* ')' | <obj>* )
(defrecord JaCodeBlock [name preamble bound-vars body])
(defparse :ptag/CodeBlock ; top-level grammar element. 
  [pstate]
  (let [has-bindings? (atom nil)] ; <exp> wrapped in parentheses could just be a primary. 
    (as-> pstate ?ps
      (if (= :transform (:tkn ?ps))
        (as-> ?ps ?ps1
          (eat-token ?ps1 :transform)
          (parse :ptag/var ?ps1)
          (store ?ps1 :trans-name)
          (eat-token ?ps1 \{)
          (parse :ptag/preamble ?ps1)
          (store ?ps1 :preamble)
          (eat-token ?ps1 \})
          (eat-token ?ps1 \;))
        ?ps)
      (assoc-in ?ps [:local 0 :bound-vars] [])
      (if (= \( (:tkn ?ps)) ; "let body" or primary
        (let [whole-tvec (token-vec ?ps)]
          (if-let [balance-pos (find-balanced-pos whole-tvec \))]
            (let [tvec (subvec whole-tvec 0 (inc balance-pos))]
               (loop [ps (eat-token ?ps \()
                      tvec tvec]
                      (cond (empty? tvec) ps
                            (not (find-token tvec :binding :stop-pos (min balance-pos (-> tvec count dec)))) ps
                            :else (let [new-ps (as-> ps ?ps2
                                                 (parse :ptag/var-decl ?ps2)
                                                 (update-in ?ps2 [:local 0 :bound-vars] conj (:result ?ps2))
                                                 (assoc ?ps2 :bound-vars (-> ?ps2 :local first :bound-vars))) ; update for immediate use.
                                        new-tvec (subvec tvec (- (count tvec) (-> new-ps :tokens count)))]
                                    (reset! has-bindings? true)
                                    (recur new-ps new-tvec)))))
            (throw (ex-info "Unbalanced open parenthesis" {:pstate ?ps}))))
        ?ps)
      (parse :ptag/exp ?ps)
      (store ?ps :body)
      (if @has-bindings? (eat-token ?ps \)) ?ps)
      (assoc ?ps :result (->JaCodeBlock (recall ?ps :trans-name)
                                        (recall ?ps :preamble)
                                        (recall ?ps :bound-vars)
                                        (recall ?ps :body))))))

;;; id-type-pair ::= <$id> ':=' <exp> ';'
(defrecord JaVarDecl [var init-val])
(defparse :ptag/var-decl
  [pstate]
  (as-> pstate ?ps
    (parse :ptag/var ?ps)
    (store ?ps :param-id)
    (if (builtin-fns (recall ?ps :param-id))
      ;; ToDo Needs work. This won't be seen currently. Instead get an error "expecting an $id"
      (throw (ex-info (str "Attempting to rebind the built-in fn " (recall ?ps :param-id)) {}))
      ?ps)
    (eat-token ?ps :binding)
    (parse :ptag/exp ?ps)
    (store ?ps :init-val)
    (eat-token ?ps \;)
    (assoc ?ps :result (->JaVarDecl (recall ?ps :param-id)
                                    (recall ?ps :init-val)))))

(def preamble-key? #{:source :target :library :metadata})
(defparse :ptag/preamble
  [pstate]
  (as-> pstate ?ps
    (assoc-in ?ps [:local 0 :preamble-decls] [])
    (loop [ps ?ps]
      (if (not (preamble-key? (:tkn ps)))
        ps
        (recur
         (as-> ps ?ps1
           (cond (= (:tkn ps) :source)
                 (parse :ptag/source ?ps1)
                 (= (:tkn ps) :target)
                 (parse :ptag/target ?ps1)
                 (= (:tkn ps) :library)
                 (parse :ptag/library ?ps1)
                 (= (:tkn ps) :metadata)
                 (parse :ptag/metadata ?ps1))
           (update-in ?ps1 [:local 0 :preamble-decls] conj (:result ?ps1))))))
    (assoc ?ps :result (recall ?ps :preamble-decls))))

(defrecord JaSource [uri source-alias])
(defparse :ptag/source
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :source)
    (store ?ps :uri :tkn)
    (eat-token ?ps string?)
    (eat-token ?ps \:)
    (parse :ptag/var ?ps)
    (eat-token ?ps \;)
    (assoc ?ps :result (->JaSource (recall ?ps :uri) (:result ?ps)))))

(defrecord JaTarget [uri target-alias])
(defparse :ptag/target
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :target)
    (store ?ps :uri :tkn)
    (eat-token ?ps string?)
    (eat-token ?ps \:)
    (parse :ptag/var ?ps)
    (eat-token ?ps \;)
    (assoc ?ps :result (->JaTarget (recall ?ps :uri) (:result ?ps)))))

(defrecord JaLibrary [uri library-alias])
(defparse :ptag/library
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :library)
    (store ?ps :uri :tkn)
    (eat-token ?ps string?)
    (eat-token ?ps \:)
    (parse :ptag/var ?ps)
    (eat-token ?ps \;)
    (assoc ?ps :result (->JaLibrary (recall ?ps :uri) (:result ?ps)))))

(defrecord JaMetadata [metadata-map])
(defparse :ptag/metadata
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :metadata)
    (parse-list ?ps \{ \} \, :ptag/map-pair)
    (assoc ?ps :result (->JaMetadata (:result ?ps)))))

(defrecord JaVarType [type-name])
(defparse :ptag/type ; ToDo Needs work. Is there even such a thing as a type decl in JSONata?
  [pstate]
  (as-> pstate ?ps
      (assoc ?ps :result (->JaVarType (:tkn ?ps)))
      (eat-token ?ps #(or (builtin-type %) (id? %)))))

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
  [var tags]
  `(or ~@(map (fn [tag]
                `(try (let [ps-done# (parse ~tag ~var)
                            tkn# [(:tkn ps-done#) (look ps-done# 1)]]
                        {:operand-tag ~tag :next-tkns tkn#})
                      (catch Exception _e# nil)))
              tags)))

(defn operand-exp?
  "Return one of [:ptag/field :ptag/var :ptag/literal :ptag/fn-call :ptag/delimited] if the front
   of the token stack is something that could be continued as a binary expression followed by a binary operator.
   This is called by the <exp> grammar rule, so you can't use it to anticipate something that starts with an
   expression such as '<exp> ? <exp> : <exp>'. That would result in a non-consuming loop."
  [ps]
  (binding [*debugging?* false] ; I don't think we need to see this!
    (continue-mac ps [:ptag/field :ptag/var :ptag/literal :ptag/fn-call #_:ptag/paren-delimited-exp :ptag/unary-op-exp])))

#_(defn operand-exp-no-operator?
  "Return one of [:ptag/field :ptag/var :ptag/literal :ptag/fn-call :ptag/delimited] if the front
   of the token stack is something that could be continued as a binary expression followed by a binary operator.
   This is called by the <exp> grammar rule, so you can't use it to anticipate something that starts with an
   expression such as '<exp> ? <exp> : <exp>'. That would result in a non-consuming loop."
  [ps]
  (binding [*debugging?* false] ; I don't think we need to see this!
    (continue-mac ps [:ptag/field :ptag/var :ptag/literal :ptag/fn-call :ptag/paren-delimited-exp :ptag/unary-op-exp] :suppress-bin-op? true)))

(defparse :ptag/field
  [ps]
  (as-> ps ?ps
    (assoc ?ps :result (:tkn ?ps))
    (eat-token ?ps field?)))

(defparse :ptag/var
  [ps]
  (as-> ps ?ps
    (assoc ?ps :result (:tkn ?ps))
    (eat-token ?ps id?)))

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

;;; <exp> ::=  ( <delimited-exp> | <binary-exp> | (<builtin-un-op> <exp>) | <fn-call> | <literal> | <field> | <$id>) ( <conditional-tail> | <exp> )?
(defrecord JaBinOpExp [exp1 bin-op exp2])
(defparse :ptag/exp
  [ps & {:keys [in-binary?]}]
  (let [tkn   (:tkn ps)
        tkn2  (look ps 1)
        operand-look (operand-exp? ps)
        base-ps (cond (delimited-next? operand-look)                              ; <delimited-exp> 
                      (parse :ptag/delimited-exp ps :operand-info operand-look),
                      (and (not in-binary?) (binary-next? operand-look))           ; <binary-exp>
                      (parse :ptag/binary-exp ps :operand-info operand-look),
                      (builtin-un-op tkn) (parse :ptag/unary-op-exp ps),          ; <unary-op-exp>
                      (= tkn :function) (parse :ptag/fn-def ps),
                      (and (or (builtin-fns tkn) (id? tkn)) (= \( tkn2))          ; <fn-call>
                      (parse :ptag/fn-call ps),
                      (or (literal? tkn) (#{\[ \{} tkn))                          ; <literal>
                      (parse :ptag/literal ps),
                      (or (id? tkn) (field? tkn)) (as-> ps ?ps                    ; <field> or <$id>
                                                    (assoc ?ps :result tkn)
                                                    (eat-token ?ps)),
                      :else
                      (throw (ex-info "Expected a unary-op, (, {, [, fn-call, literal, $id, or path element."
                                      {:got tkn :pstate ps})))]
    (cond (and (= \? (:tkn base-ps)) (not in-binary?))
          (parse :ptag/conditional-tail base-ps :predicate (:result base-ps))
          (and (not in-binary?) (-> base-ps :tkn binary-op?))
          (as-> base-ps ?ps
            (store ?ps :operator :tkn)
            (eat-token ?ps)
            (parse :ptag/exp ?ps :in-binary? true)
            (assoc ?ps :result (->JaBinOpExp (:result base-ps) (recall ?ps :operator) (:result ?ps))))
          :else base-ps)))

;;; <binary-exp>  ::= <operand-exp> <binary-op> <exp>
;;; <operand-exp> ::= <field> | <var> | literal | <fn-call> | <delimited-exp> | <unary-op-exp>
(defparse :ptag/binary-exp
  [ps & {:keys [operand-info]}]
  (as-> ps ?ps
    (parse (:operand-tag operand-info) ?ps) ; Parametric tag!
    (store ?ps :exp1)
    (store ?ps :op :tkn)
    (eat-token ?ps binary-op?)
    (parse :ptag/exp ?ps #_#_:in-binary? true) ; in-binary? so that the caller can consume \?, and other reasons.
    (assoc ?ps :result (->JaBinOpExp (recall ?ps :exp1) (recall ?ps :op) (:result ?ps)))))

;;; ToDo This one needs some explaining!
;;; <delimited-exp> ::= <operand-exp> ( ('.' <paren-delimited-exp>) | ('.' <curly-delimited-exp>) | <square-delimited-exp> ) 
(defparse :ptag/delimited-exp
  [ps & {:keys [operand-info]}]
  (let [{:keys [operand-tag next-tkns]} operand-info]
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
            :else (throw (ex-info "Expected a delimited-exp." {:operand-info operand-info})))
      (assoc-in ?ps [:result :operand] (recall ?ps :operand)))))

(defrecord JaParenDelimitedExp [exp operand])
(defparse :ptag/paren-delimited-exp
  [ps]
  (as-> ps ?ps
    (eat-token ?ps \()
    (parse :ptag/exp ?ps)
    (eat-token ?ps \))
    (assoc ?ps :result (map->JaParenDelimitedExp {:exp (:result ?ps)}))))

(defrecord JaSquareDelimitedExp [operand exp])
(defparse :ptag/square-delimited-exp
  [ps]
  (as-> ps ?ps
    (parse-list ?ps \[ \] \, :ptag/exp)
    (assoc ?ps :result (map->JaSquareDelimitedExp {:exp (:result ?ps)}))))

;;; <obj> ::= "{" <map-pair> (',' <map-pair> )* "}"
(defrecord JaCurlyDelimitedExp [operand map-pairs])
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
      (store ?ps :op :tkn)
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

;;; <array-like> ::=  <array> | <range-exp>
(defrecord JaRange [start stop])
(defparse :ptag/array-like
  [ps]
  (let [tvec (token-vec ps)
        close-pos (when (= (first tvec) \[) (find-balanced-pos tvec \]))
        range-pos (when close-pos (find-token tvec :range :stop-pos close-pos))]
    (if (and close-pos range-pos (< range-pos close-pos)) ; ToDo Heuristic! (Replace with try?)
      (parse :ptag/range-exp ps)
      (parse :ptag/array ps))))

;;; <array> '[' <exp>? ( ',' <exp>)? ']'
(defrecord JaArray [exprs])
(defparse :ptag/array
  [ps]
  (as-> ps ?ps
    (parse-list ?ps \[ \] \, :ptag/exp)
    (assoc ?ps :result (->JaArray (:result ?ps)))))

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

  ;;;------------ 'atomic' expressions --------------------------------------
(defn is-bound? [ps tkn]
  (some #(= (:name tkn) %)
        (->> ps :bound-vars (map #(-> % :var-name)))))

(defparse :ptag/var
  [pstate]
  (let [glob? (is-bound? pstate (:tkn pstate))
        tkn (if glob? (assoc (:tkn pstate) :bound? true) (:tkn pstate))]
    (if (id? tkn)
      (-> pstate (assoc :result tkn) eat-token)
      (throw (ex-info "expected an $id" {:got tkn :pstate pstate})))))


(defparse :ptag/string
  [pstate]
  (let [tkn (:tkn pstate)]
    (if (string? tkn)
      (-> pstate (assoc :result tkn) eat-token)
      (throw (ex-info "expected a string literal" {:got tkn :pstate pstate})))))

;;; <literal> ::= string | number | 'true' | 'false' | regex | <obj> | <array-like>
(defparse :ptag/literal
  [ps]
  (let [tkn (:tkn ps)]
    (cond (literal? tkn) (-> ps (assoc :result tkn) eat-token),
          (= tkn \{)     (parse :ptag/obj ps),
          (= tkn \[)     (parse :ptag/array-like ps),
          :else (throw (ex-info "expected a literal string, number, 'true', 'false' regex, obj, range, or array."
                                {:got tkn :pstate ps})))))

;;; fn-call ::=  <$id> '(' [<exp> ','...]* ')'
(defrecord JaFnCall [fn-name args])
(defparse :ptag/fn-call
  [ps]
  (as-> ps ?ps
    (store ?ps :fn-name :tkn)
    (eat-token ?ps id?)
    (eat-token ?ps \()
    (parse-list-terminated ?ps :term-fn #(= % \)) :sep-fn #(= % \,))
    (store ?ps :args)
    (eat-token ?ps \))
    (assoc ?ps :result (->JaFnCall (:var-name (recall ?ps :fn-name)) (recall ?ps :args)))))

;;; <fn-def> ::= 'function' '(' <$id> [',' <$id>]* ')' '{' <exp> '}'
(defrecord JaFnDef [vars body])
(defparse :ptag/fn-def
  [ps]
  (as-> ps ?ps
    (eat-token ?ps :function)
    (eat-token ?ps \()
    (parse-list-terminated ?ps :term-fn #{\)} :sep-fn #{\,} :parse-tag :ptag/var)
    (store ?ps :vars)
    (eat-token ?ps \))
    (eat-token ?ps \{)
    (parse :ptag/exp ?ps)
    (store ?ps :body)
    (eat-token ?ps \})
    (assoc ?ps :result (->JaFnDef (recall ?ps :vars) (recall ?ps :body)))))
