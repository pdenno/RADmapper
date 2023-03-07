(ns rad-mapper.data-util.schema-util
  "Functions to classify schema, include standard messaging schema."
  (:require
   [clojure.pprint               :refer [cl-format]]
   [clojure.string               :as str]
   #?(:clj  [datahike.api        :as d]
      :cljs [datascript.api      :as d])
   [rad-mapper.data-util.db-util :as du :refer [conn xpath xml-type?]]
   [taoensso.timbre              :as log]))

(def simple-xsd?
  {:xsd/minLength      :number
   :xsd/maxLength      :number
   :xsd/pattern        :string
   :xsd/fractionDigits :number
   :xsd/totalDigits    :number
   :xsd/minInclusive   :number})

(def generic-schema-type? "These might be associated with whole files, but specializations might exist"
  #{:generic/message-schema
    :generic/library-schema
    :generic/qualified-dtype-schema,
    :generic/unqualified-dtype-schema
    :generic/code-list-schema
    :generic/xsd-file})

(def special-schema-type? "These are associated with whole files."
  #{:ccts/message-schema
    :ubl/message-schema
    :oagis/message-schema
    :ccts/component-schema
    :oasis/component-schema
    :iso/iso-20022-schema})

;;; This does file-level dispatching as well as the details
(defn rewrite-xsd-dispatch
  [obj & [specified]]
   (let [stype (:schema/type obj)
         schema-sdo  (:schema/sdo obj)
         meth
         (cond ;; Optional 2nd argument specifies method to call
               (keyword? specified) specified,

               ;; Files (schema-type)
               (and (= stype :ccts/message-schema)    (= schema-sdo :oasis))  :ubl/message-schema,
               (and (= stype :ccts/message-schema)    (= schema-sdo :oagi))   :oagis/message-schema,
               (and (= stype :ccts/component-schema)  (= schema-sdo :oagi))   :generic/qualified-dtype-schema,
               (and (= stype :ccts/component-schema)  (= schema-sdo :oasis))  :oasis/component-schema,
               (and (= stype :generic/message-schema) (= schema-sdo :qif))    :generic/xsd-file, ; ToDo: Probably temporary.

               (special-schema-type? stype) stype,
               (generic-schema-type? stype) stype,

               ;; Odd cases
               (and (map? obj) (contains? simple-xsd? (:xml/tag obj)))        :simple-xsd,
               (and (map? obj) (contains? obj :xml/tag))                      (:xml/tag obj),
               (and (map? obj) (contains? obj :ref))                          :ref
               ; ToDo: this one for polymorphism #{:xsd/element :xsd/choice} etc.
               (contains? obj :xml/tag)                                       (:xml/tag obj))]
     meth))

(def ^:dynamic *skip-doc-processing?* false)

(defn obj-doc-string
  "If the object has annotations in its :xml/content, remove them and return
   modified object and the string content. Otherwise, just return object and nil.
   The xsd:appinfo optional content is ignored."
  [obj]
  (if (or *skip-doc-processing?*
          (not (map? obj)) ; ToDo: This and next are uninvestigated problems in the etsi files.
          (not (contains? obj :xml/content)))
    [obj nil]
    (let [parts (group-by #(xml-type? % :xsd/annotation) (:xml/content obj))
          annotes (get parts true)
          obj-annote-free (assoc obj :xml/content (vec (get parts false)))
          s (when (not-empty annotes)
              (str/trim
               (reduce (fn [st a]
                         (str st "\n" (let [docs (filter #(xml-type? % :xsd/documentation) (:xml/content a))]
                                        (reduce (fn [more-s d]
                                                  (if (-> d :xml/content string?)
                                                    (str more-s "\n" (str/trim (:xml/content d)))
                                                    more-s))
                                                ""
                                                docs))))
                       ""
                       annotes)))]
      [obj-annote-free (if (and (string? s) (re-matches #"^\s*$" s)) nil s)]))) ; ToDo: expensive?


(defn q-schema-topic
  "Lookup the topic for a schema in the DB."
  [urn]
  (d/q `[:find ?topic .
         :where [?s :schema/name ~urn]
         [?s :schema/topic ?topic]] @conn))

(defn q-schema-sdo
  "Lookup the SDO for a schema in the DB."
  [urn]
  (d/q `[:find ?sdo .
         :where [?s :schema/name ~urn]
         [?s :schema/sdo ?sdo]] @conn))

(defn q-schema-type
  "Lookup the type for a schema in the DB."
  [urn]
  (d/q `[:find ?type .
         :where [?s :schema/name ~urn]
         [?s :schema/type ?type]] @conn))

(defn schema-ns
  "Return the namespace urn string for the argument xmap."
  [xmap]
  (or (-> (xpath xmap :xsd/schema)  :xml/attrs :targetNamespace)
      (-> (xpath xmap :ROOT/schema) :xml/attrs :targetNamespace))) ; for some UBL-provided w3c schema

(defn schema-sdo
  "Return a keyword identifying the XML file's standards development organization."
  [xmap]
  (if-let [ns (schema-ns xmap)]
    (cond (re-matches #"^urn:oasis:[\w,\-,\:]+:ubl:[\w,\-,\:]+$" ns) :oasis,
          (re-matches #"^urn:un:unece:uncefact:[\w,\-,\:]+$" ns) :cefact, ; cefact via UBL ; ToDo: NONE OF THESE???
          (re-matches #"^http://www.openapplications.org/oagis/.*$" ns) :oagi,
          (re-matches #"^urn:iso:std:iso:20022:tech:xsd:pain.+$" ns) :iso ; ISO via oagis
          (re-matches #"^http://uri.etsi.*" ns) :etsi
          (re-matches #"^http://www.w3.org/.*" ns) :w3c
          (re-matches #"^http://qifstandards.org/xsd/qif3" ns) :qif)
    (do (log/warn "Cannot determine schema SDO:" (:schema/pathname xmap) " using :unknown.")
        :unknown)))

(def non-standard-oagis-schema-topics
  (let [pat {"urn:oagis-~A:CodeList_ConstraintTypeCode_1.xsd"            "Codelist, ConstraintTypes",
             "urn:oagis-~A:CodeList_TimeFormatCode_1.xsd"                "Codelist, TimeFormats"
             "urn:oagis-~A:CodeList_DateTimeFormatCode_1.xsd"            "Codelist, DateTimeFormats"
             "urn:oagis-~A:CodeList_TimeZoneCode_1.xsd"                  "Codelist, TimeZones",
             "urn:oagis-~A:CodeList_DateFormatCode_1.xsd"                "Codelist, DateFormat",
             "urn:oagis-~A:CodeList_CharacterSetCode_IANA_20131220.xsd"  "Codelist, CharacterSets",
             "urn:oagis-~A:CodeLists_1.xsd"                              "Codelist, Aggregated",
             "urn:oagis-~A:CodeList_ConditionTypeCode_1.xsd"             "Codelist, ConditionTypes",
             "urn:oagis-~A:CodeList_CurrencyCode_ISO_7_04.xsd"           "Codelist, Currencies"}]
    (merge (reduce-kv (fn [m k v] (assoc m (cl-format nil k "10.6") v)) {} pat)
           (reduce-kv (fn [m k v] (assoc m (cl-format nil k "10.8") v)) {} pat))))

(def non-standard-schema-topics
  "Easiest to just define these explicitly"
  (merge non-standard-oagis-schema-topics
         {"urn:iso:std:iso:20022:tech:xsd:pain.001.001.04"                              "Datatypes, Financial",
          "urn:iso:std:iso:20022:tech:xsd:pain.001.001.05"                              "Datatypes, Financial",
          "urn:iso:std:iso:20022:tech:xsd:pain.002.001.04"                              "Datatypes, Financial",
          "urn:iso:std:iso:20022:tech:xsd:pain.002.001.05"                              "Datatypes, Financial",
          "urn:iso:std:iso:20022:tech:xsd:pain.008.001.03"                              "Datatypes, Financial",
          "urn:iso:std:iso:20022:tech:xsd:pain.008.001.04"                              "Datatypes, Financial",
          "urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2"    "Components, CommonExtensions",
          "urn:oasis:names:specification:ubl:schema:xsd:UnqualifiedDataTypes-2"         "Datatypes, Unqualified",
          "urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2"        "Components, CommonBasic",
          "urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2"    "Components, CommonAggregate",
          "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2"    "Components, Core",
          "urn:oasis:names:specification:ubl:schema:xsd:QualifiedDataTypes-2"           "Datatypes, Qualified"
          "urn:oasis:names:specification:ubl:schema:xsd:CommonSignatureComponents-2"    "Components, CommonSignature",
          "http://uri.etsi.org/01903/v1.4.1#"                                            "ETSI, not investigaated"}))

(defn schema-topic
  "Return the portion of the URN that most specifically describes the schema content.
   This is not necessarily unique!"
  [urn]
  (let [desc [(q-schema-sdo urn) (q-schema-type urn)]]
     (cond (= desc [:oasis :ccts/message-schema])
           (->> urn (re-matches #"^urn:oasis:names:specification:ubl:schema:xsd:(.+)-\d$") second),

           (= desc [:oagi :ccts/message-schema])
           (->> urn (re-matches #"^urn:oagis-\d+\.\d+:(.+)$") second),

           (= desc [:oagi :generic/code-list-schema])
           (->> urn (re-matches #"^urn:oagis-\d+\.\d+:(.+)$") second),

           (= desc [:qif :generic/message-schema])
           (->> urn (re-matches #"^urn:QIF-\d:Application:QIF(.+)$") second),

           (= desc [:qif :generic/library-schema])
           (->> urn (re-matches #"^urn:QIF-\d:Library:(.+)$") second),

           (contains? non-standard-schema-topics urn)
           (get non-standard-schema-topics urn),

           :else
           (do (log/warn "Cannot determine schema topic" urn) ""))))

(defn schema-spec
  "Return a keyword signifying the specification of which the schema is part.
   These are #{:ubl, :oagis, etc.}. It is used as :schema/spec"
  [xmap]
  (let [ns   (schema-ns xmap)
        spec (case (:schema/sdo xmap)
               :cefact     (when (= ns "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2") :cefact-ccl)
               :oasis      (when (re-matches #"^urn:oasis:[\w,\-,\:]+:ubl:[\w,\-,\:]+(\-2)$" ns)                   :ubl)
               :oagi       (when (re-matches #"^http://www.openapplications.org/oagis/10$" ns)                     :oagis)
               :iso        :iso-20022
               :etsi       :etsi-1903 ; ToDo: guessing
               :w3c        :w3c       ; In QIF somewhere!
               :qif        :qif
               "default")]
    (or spec (do (log/warn "Cannot determine file spec:" (:schema/pathname xmap) " Using :default.")
                 :unknown))))

(defn schema-version
  [xmap]
  (case (:schema/spec xmap)
    :ubl    "2"
    :oagis  "10"
    :qif    (let [[_ n] (re-matches #"^http://qifstandards.org/xsd/qif(\d)" (schema-ns xmap))]
              (or n ""))
    ""))

(defn schema-subversion
  "Return the subversion. NB: Currently this is hard coded. Should be an environment variable." ; ToDo: need env.
  [xmap]
  (let [spec   (:schema/spec xmap)
        pname  (:schema/pathname xmap)]
    (cond (= spec :ubl) "3"
          (= spec :oagis)
          (let [[_ subver] (re-matches #".*OAGIS/[0-9]+\.([0-9,\.]+).*" pname)]
            (or subver
                (log/warn "Could not determine OAGIS subversion.")))
          (= spec :cefact-ccl) "" ; ToDo: See also UBL CCL.
          :else "")))

;;; ToDo: Make this a multi-method (on the case values)
(defn schema-type
  "Return a keyword signifying the specification of which the schema is part.
   These are #{:ubl-2, :oagis-10, etc.}. It is used as :schema/spec"
  [xmap]
  (let [sdo (:schema/sdo xmap)
        pname (:schema/pathname xmap)
        ns  (schema-ns xmap)]
    (case sdo
      :cefact
      (cond (= ns "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2")
            :generic/unqualified-dtype-schema
            :else (log/warn "Cannot determine file spec:" pname))

      :oasis
      (cond (= ns "urn:oasis:names:specification:ubl:schema:xsd:UnqualifiedDataTypes-2")
            :generic/unqualified-dtype-schema,
            (= ns "urn:oasis:names:specification:ubl:schema:xsd:QualifiedDataTypes-2")
            :generic/qualified-dtype-schema,
            (re-matches #"^urn:oasis:names:specification:ubl:schema:xsd:Common[\w,\-,\:]+Components\-2$" ns)
            :ccts/component-schema
            (re-matches #"^urn:oasis:names:specification:ubl:schema:xsd:\w+-2$" ns)
            :ccts/message-schema
            :else (log/warn "Cannot determine file spec:" pname))

      :oagi ; no URNs; guess based on pathname.
      (cond (re-matches #".*Fields\.xsd$" pname) ; though it is in the "Components" directory
            :generic/unqualified-dtype-schema
            (re-matches #".+CodeLists.+" pname)
            :generic/code-list-schema
            (re-matches #".+Components.+" pname)
            :ccts/component-schema
            (re-matches #"^http://www.openapplications.org/oagis/10$" ns)
            :ccts/message-schema ; ToDo: Not quite correct.
            :else (do (log/warn "Cannot determine schema-type:" pname)
                      :generic/xsd-file))

      :etsi :generic/xsd-file

      :iso :iso/iso-20022-schema

      :qif
      (cond (re-matches #".*QIFApplications/.*.xsd$" pname) :generic/message-schema
            (re-matches #".*QIFLibrary/.*.xsd$"   pname)    :generic/library-schema
            :else (do (log/warn "Cannot determine schema-type:" pname)
                      :generic/xsd-file))
      ;; Default
      :generic/xsd-file)))

;;; ToDo: A schema is getting past this with schema-name "".
(defn schema-name
  "Return the name of the schema object. This uses the XML content to determine one."
  [xmap]
  (let [sdo  (:schema/sdo xmap)
        ver  (:schema/version xmap)
        sver (:schema/subversion xmap)
        ver-str (if (empty? sver) ver (str ver "." sver ))
        pname (:schema/pathname xmap)]
    (cond
      (= :oagi sdo)
      (if-let [[_ fname] (re-matches #".*Components/(\w+).xsd" pname)]
        (str "urn:oagis-" ver-str ":Components:" fname)
        (if-let [[_ fname] (re-matches #".*Model/Nouns/(\w+).xsd" pname)]
          (str "urn:oagis-" ver-str ":Nouns:" fname)
          (if-let [[_ fname] (re-matches #".*Common/ISO20022/(pain[0-9,\.]+).xsd" pname)]
            (str "urn:oagis-" ver-str ":Common:" fname)
            (if-let [name  (-> (xpath xmap :xsd/schema :xsd/element) :xml/attrs :name)]
              (str  "urn:oagis-" ver-str ":" name)
              (if-let [res-pname (-> pname (str/split #"/") last (str/split #"\.") first)]
                (do (log/warn "Using pathname to define OAGIS" ver-str "schema name:" res-pname)
                    (str "urn:oagis-" ver-str ":" res-pname))
                (do (log/warn "Could not determine OAGIS" ver-str "schema name.")
                    :mm/nil)))))),
        (= :qif sdo)
      (if-let [[_ fname] (re-matches #".*/QIFLibrary/(\w+).xsd" pname)]
        (str "urn:QIF-" ver-str ":Library:" fname)
        (if-let [[_ fname] (re-matches #".*/QIFApplications/(\w+).xsd" pname)]
          (str "urn:QIF-" ver-str ":Application:" fname)
            (do (log/warn "Could not determine QIF" ver-str "schema name.") :mm/nil)))
      (#{:oasis :cefact :iso :etsi} sdo)
      (if-let [name (schema-ns xmap)]
        name
        (do (log/warn "Could not determine UBL or ISO schema name:" pname) ""))
      :else
      (let [fname (-> pname (str/split #"/") last)]
        (log/warn "Could not determine schema name:" pname " Using " fname)
        fname))))
