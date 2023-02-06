(ns rad-mapper.schema-db
  "Create and populate a database with schemas from various SDOs #{:CEFACT :OASIS :OAGI :ISO}
   Every element (a named BBIE, BIE, data type etc.) has an :schema/term and a :sp/type.
   (sp = schema-part, an abstraction over content and attributes.)
   The :sp/type are #{:BCC :ACC :ASCC :CCT :uDT :qDT :BBIE :ABIE :ASBIE } cite{Kabak2010}.
   :sp are derived from :xsd/element.
   #{:ABIE :BBIE :ASBIE} have various 'contexts' (e.g. the business process context :Trade).
   :uDT and :qDT specify restrictions on :CCT (core component type) and value sets of :BCC.
   Elements are organized into schema.
   Most access is through pathom, but some key functions for the REPL:
     (list-schemas) - Get the list of schema (their :mm/schema-name), which is a URN string.
     (get-schema <schema-urn-string>) - Get a map describing the whole schema. Its elements are :schema/content.
     (get-term <schema-urn-string> <term>) - Get the map of information for the :schema/term.
     (get-term-schemas <term>) - Get a list of schema (their URN strings) for the term (a string)."
  (:require
   [clojure.java.io              :as io]
   [clojure.pprint               :refer [cl-format]]
   [clojure.spec.alpha           :as s]
   [clojure.string               :as str]
   [com.wsscode.pathom.connect   :as pc]
   #?(:clj  [datahike.api        :as d]
      :cljs [datascript.api      :as d])
   #?(:clj  [datahike.pull-api   :as dp]
      :cljs [datascript.pull-api :as dp])
   [mount.core                   :refer [defstate]]
   [rad-mapper.db-util           :as du :refer [xpath xpath- xml-type?]]
   [rad-mapper.util              :as util]
   [taoensso.timbre              :as log]))

;;; ToDo:
;;;    * The best thing to do with docs is alway collect them, but eliminate them in presentation like I optionally do with :db/id.
;;;    * elem-props-r only works for UBL schema.
;;;    * Get :sp/function into everything. BTW, CEFACT schema have  <ccts:Acronym>BBIE</ccts:Acronym>
;;;    * Maybe split this file into something in src/dev and something in src/main/app/server (which is where the pathom stuff is).
;;;      But then I wonder what would remain in src/main/app/model.

(def db-cfg {:store {:backend :file :path "resources/databases/schema"}
             :rebuild-db? false
             :schema-flexibility :write})

(def diag (atom nil))
(def conn "A handy connection to the database" nil) ;

;;; ToDo: Make the following environment variables.
(def ubl-root   "/Users/pdenno/Documents/specs/OASIS/UBL-2.3/xsdrt/")
;;;(def oagis-root "/Users/pdenno/Documents/specs/OAGI/OAGIS_10_6_EnterpriseEdition/OAGi-BPI-Platform/org_openapplications_oagis/10_6/Model/")
(def oagis-10-6-root "data/OAGIS/10.6/")
(def oagis-10-8-root "data/OAGIS/10.8/Model/")
(def michael-root    "data/testing/michaelQIF/")
(def qif-root        "data/testing/QIF/xsd/")

(defonce bad-file-on-rebuild? (atom #{})) ; For debugging

;;; NB :db.cardinality/many means the property has a vector value {:foo/foo [1 2 3]}
;;;    :db.type/ref means that the property means the element(s) of the property are db/id to maps.
;;;    Perhaps everything that is the value of a ref should have its own ns name.
(def db-schema
  "Defines the datahike schema for this database.
     :db/db.cardinality=many means value is a vector of values of some :db.type."
  [#:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/Cardinality}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :cct/CategoryCode}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/DataTypeTermName}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/Definition}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/Description}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/DictionaryEntryName}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/Name}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/ObjectClass}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :cct/PrimitiveType}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/PropertyTermName}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/QualifierTerm}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/RepresentationTermName}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/UniqueID}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/UsageRule}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/VersionID}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/sc-id
        :doc "'sc' is supplementary to component"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :cct/sc-type}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :cct/sc-use}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :cct/supplementary}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :code-list/name}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :code-list/lists}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :doc/doc-string}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :fn/component-type}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :fn/type}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :fn/base}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :import/prefix}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :import/referenced-schema}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :iso/CodeName,
        :doc "Used in OAGIS/ISO currency codes"}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :library/content}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :mm/comment
        :doc "All the mm things are for debugging."}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :mm/debug}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/boolean, :ident :mm/file-not-read?}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :mm/unhandled-xml}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :mm/temp-include}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :model/sequence
        :doc "generic modeling concept"}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :model/enumeration}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/complex-types}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/imported-schemas}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :schema/included-schemas}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/inlined-typedefs}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/name, :unique :db.unique/identity}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/pathname}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :schema/sdo}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/short-name}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/simple-types}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :schema/spec}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/subversion
        :doc "e.g. for OAGIS 10.8 it is '8'"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/topic}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :schema/type}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/version
        :doc "e.g. for OAGIS 10.8 it is '10'"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/boolean, :ident :sp/abstract?}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/ref,     :ident :sp/component
        :doc "CCT see also supplementary"}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :sp/doc-string
        :doc "when :xsd/documentation is a string, rather than :sp/supplementary
              ToDo: Should I just use :doc/doc-string ?"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/ref,     :ident :sp/function,
        :doc ":fn objects."}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :sp/max-occurs}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :sp/min-occurs}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :sp/name}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :sp/ref
        :doc "e.g. xsd:element attr."}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/ref,     :ident :sp/supplementary
        :doc "CCT"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :sp/type}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :sp/type-ref
        :doc "xsd:elem with a name, type attrs"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :sp/xsd-type}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :term/name,
        :doc "An ID unique within the schema"}
   ;; ToDo: These are typically things that could be generalized or need further investigation
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :xsd/attributeGroup}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :xsd-agroup/data}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :xsd/choice}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/fractionDigits}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/maxLength}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/minInclusive}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/minLength}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :xsd/pattern}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/ref,     :ident :xsd/restriction} ; ToDo: eliminate this.
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/totalDigits}
   ;; These are for boxing values.
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :zip/keys}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :zip/vals}])

;;; NB s/every-kv and s/every are probabilistic; they do not check every entry.
;;; ToDo: Write something that uses these and does d/q to check the whole DB. Of course, these need more work!
(s/def ::db-ent (s/keys :req [:db/id]))
(s/def ::type-ref (s/and ::db-ent (s/keys :req [:sp/name :sp/type])))
(s/def ::tagged (s/or :generic-elem ::gelem :component ::basic))
(s/def ::basic (s/and ::db-ent (s/keys :req [:sp/name :sp/function]) #(= :BBIE  (-> % :sp/function :fn/component-type))))
(s/def ::gelem (s/and ::db-ent (s/keys :req [:sp/name :sp/function]) #(= :gelem (-> % :sp/function :fn/type))))
(s/def ::quantified-elem (s/and ::gelem (s/keys :req [:sp/min-occurs :sp/max-occurs])))
(s/def ::gelems (s/every ::gelem))
(s/def ::model-seq (s/and ::db-ent (s/keys :req [:model/sequence]) #(s/valid? ::gelems (:model/sequence %)))) ; Test a property!
(s/def ::ccts-based-message-schema (s/and ::db-ent (s/keys :req [:schema/type]) #(= :ccts/message-schema (:schema/type %))))

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
          (re-matches #"^http://qifstandards.org/xsd/qif3" ns) :qif
          :else :unknown)
    (log/warn "Cannot determine schema SDO:" (:schema/pathname xmap))))

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
        (if-let [[_ fname] (re-matches #".*Common/ISO20022/(pain[0-9,\.]+).xsd" pname)]
          (str "urn:oagis-" ver-str ":Common:" fname)
          (if-let [name  (-> (xpath xmap :xsd/schema :xsd/element) :xml/attrs :name)]
            (str  "urn:oagis-" ver-str ":" name)
            (if-let [res-pname (-> pname (str/split #"/") last (str/split #"\.") first)]
              (do (log/warn "Using pathname to define OAGIS" ver-str "schema name:" res-pname)
                  (str "urn:oagis-" ver-str ":" res-pname))
              (do (log/warn "Could not determine OAGIS" ver-str "schema name.")
                  :mm/nil))))),
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
      (do
        (log/warn "Could not determine schema name:" pname) ""))))

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

(defn imported-schemas
  "Using the :xsd/import, return a map of the prefixes used in the schema."
  [xmap]
  (let [ischemas
        (->>
         (xpath xmap :xsd/schema)
         :xml/content
         (filter #(xml-type? % :xsd/import))
         (map #(-> % :xml/attrs :namespace)))
        ns-info (:xml/ns-info xmap)]
    (reduce (fn [res schema]
               (if-let [prefix (-> ns-info :u->ps (get schema) first)]
                 (conj res {:import/prefix prefix :import/referenced-schema schema})
                 (do (log/warn "No prefix for schema" schema) res)))
            []
            ischemas)))

(defn read-clean
  "Return a map structure containing the :xml/content (cleaned-up) and :ns-info."
  [pathname]
  (let [xml (du/read-xml pathname)]
    (as-> xml ?xmap
        (assoc ?xmap :schema/sdo (schema-sdo ?xmap))
        (assoc ?xmap :schema/spec (schema-spec ?xmap))
        (assoc ?xmap :schema/version (schema-version ?xmap))
        (assoc ?xmap :schema/subversion (schema-subversion ?xmap))
        (assoc ?xmap :schema/type (schema-type ?xmap))
        (assoc ?xmap :schema/name (schema-name ?xmap)))))

;;;=======================  Rewrite xsd to schema content for DB  ===========================
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

(def debugging? (atom false))

;;; Establishes rewrite-xsd methods.
(defmacro defparse [tag [arg props] & body]
  `(defmethod rewrite-xsd ~tag [~arg & ~'_]
     ;; Once *skip-doc-processing?* is true, it stays so through the dynamic scope of the where it was set.
     (when @debugging? (log/info "defparse tag = " ~tag))
     (binding [*skip-doc-processing?* (or *skip-doc-processing?* (:skip-doc-processing? ~props))]
       (let [[~arg doc-string#] (obj-doc-string ~arg)
             result# (do ~@body)]
         (if (and doc-string# (map? result#))
           (assoc result# :doc/doc-string doc-string#)
           result#)))))

(def simple-xsd?
  {:xsd/minLength      :number
   :xsd/maxLength      :number
   :xsd/pattern        :string
   :xsd/fractionDigits :number
   :xsd/totalDigits    :number
   :xsd/minInclusive   :number})

(def special-schema-type? "These are associated with whole files."
  #{:ccts/message-schema
    :ubl/message-schema
    :oagis/message-schema
    :ccts/component-schema
    :oasis/component-schema
    :iso/iso-20022-schema})

(def generic-schema-type? "These might be associated with whole files, but specializations might exist"
  #{:generic/message-schema
    :generic/library-schema
    :generic/qualified-dtype-schema,
    :generic/unqualified-dtype-schema
    :generic/code-list-schema
    :generic/xsd-file})

(s/def ::schema-type-kw #(or (special-schema-type? %)
                             (generic-schema-type? %)))

;;; This does file-level dispatching as well as the details
(defn rewrite-xsd-dispatch
  [obj & [specified]]
   (let [stype (:schema/type obj)
         schema-sdo  (:schema/sdo obj)
         meth
         (cond ;; Optional 2nd argument specifies method to call
               (keyword? specified) specified,

               ;; Files (schema-type)
               (and (= stype :ccts/message-schema) (= schema-sdo :oasis))    :ubl/message-schema,
               (and (= stype :ccts/message-schema) (= schema-sdo :oagi))     :oagis/message-schema,
               (and (= stype :ccts/component-schema) (= schema-sdo :oagi))   :generic/qualified-dtype-schema,
               (and (= stype :ccts/component-schema) (= schema-sdo :oasis))  :oasis/component-schema,
               (and (= stype :generic/message-schema) (= schema-sdo :qif))   :generic/xsd-file, ; ToDo: Probably temporary.

               (special-schema-type? stype) stype,
               (generic-schema-type? stype) stype,

               ;; Odd cases
               (and (map? obj) (contains? simple-xsd? (:xml/tag obj)))        :simple-xsd,
               (and (map? obj) (contains? obj :xml/tag))                      (:xml/tag obj),
               (and (map? obj) (contains? obj :ref))                          :ref
               ; ToDo: this one for polymorphism #{:xsd/element :xsd/choice} etc.
               (contains? obj :xml/tag)                                       (:xml/tag obj))]
     meth))

(defmulti rewrite-xsd #'rewrite-xsd-dispatch)

(defmethod rewrite-xsd nil [obj & schema]
  (if schema
    (log/warn "No method for obj/schema.")
    (log/warn "No method for obj."))
  (reset! diag {:obj obj :schema schema})
  :mm/rewrite-xsd-nil-method)

;;; Seen once in a QIF file.
(defmethod rewrite-xsd :xsd/attributeGroup
  [xmap]
  (let [doc (-> xmap (xpath- :xsd/annotation :xsd/documentation) :xml/content)]
    {:xsd/attributeGroup
     (cond-> {}
       (not-empty doc)   (assoc :sp/doc-string doc)
       true              (assoc :xsd-agroup/data (-> xmap :xml/attrs :ref)))}))

;;;===============  File Level ==================================================
;;; ToDo: Adapted from :oasis/component-schema, which suggests that that could use this.
(defparse :generic/xsd-file
  [xmap]
  (let [elems (->> (xpath xmap :xsd/schema)
                   :xml/content
                   (filter #(xml-type? % :xsd/element))
                   not-empty)
        comps (->> (xpath xmap :xsd/schema)
                   :xml/content
                   (filter #(xml-type? % :xsd/complexType))
                   not-empty)
        schemas (-> xmap imported-schemas not-empty)]
    (as->
        (cond-> xmap
          true  (assoc  :library/content [])
          elems (update :library/content into (mapv #(-> (rewrite-xsd % :xsd/element)
                                                         (assoc :sp/function {:fn/type :type-ref}))
                                                    elems))
          comps (update :library/content into
                        (mapv (fn [cplx]
                                (as-> (rewrite-xsd cplx :xsd/complexType) ?t
                                 (if (:sp/type ?t) (assoc ?t :term/type (:sp/type ?t)) ?t)
                                 #_(update ?t :sp/function #(assoc % :fn/component-type cc-type))
                                 (dissoc ?t :sp/type)))
                              comps))
          schemas (assoc :schema/imported-schemas schemas))
        ?r
      (if (-> ?r :library/content empty?) (dissoc ?r :library/content) ?r)
      (dissoc ?r :xml/ns-info :xml/content))))

;;; ToDo: Need file type for QIF Library files
(defparse :generic/library-schema
  [xmap]
  (rewrite-xsd xmap :generic/xsd-file))

(defparse :ubl/message-schema
  [xmap]
  (let [short-name (second (re-matches  #"[\w,\:\d]+\:(\w+)\-\d" (:schema/name xmap)))]
    (as-> xmap ?x
      (assoc ?x :schema/imported-schemas (imported-schemas ?x))
      (assoc ?x :schema/short-name (or short-name :short-name-not-found))
      (if-let [typedefs (not-empty (filter #(xml-type? % :xsd/complexType) (-> (xpath ?x :xsd/schema) :xml/content)))]
        (assoc ?x :schema/inlined-typedefs
               (mapv #(rewrite-xsd % :inline-typedef) typedefs))
        ?x)
      (if-let [elems (not-empty (filter #(xml-type? % :xsd/element)
                                        (:xml/content (xpath ?x :xsd/schema))))]
        (assoc ?x :model/sequence (mapv #(rewrite-xsd % :xsd/element) elems))
        ?x)
      (dissoc ?x :xml/ns-info :xml/content))))

(defparse :oagis/message-schema
  [xmap {:skip-doc-processing? true}]
  (binding [*skip-doc-processing?* true] ; ToDo: not ready for this!
    (let [content (-> (xpath xmap :xsd/schema) :xml/content)
          includes  (not-empty  (filter #(xml-type? % :xsd/include) content))
          top-elems (not-empty  (filter #(xml-type? % :xsd/element) content))
          top       (first top-elems)
          inlined   (not-empty (filter #(xml-type? % :xsd/complexType) content))]
      (cond-> xmap
        includes  (assoc :mm/temp-include (mapv #(rewrite-xsd %) includes))
        top       (assoc :model/sequence  (vector (rewrite-xsd top :xsd/element)))
        top-elems (assoc :sp/type-ref (mapv #(-> (rewrite-xsd % :xsd/element)
                                                 (assoc :sp/function {:fn/type :type-ref}))
                                            top-elems))
        inlined   (assoc :schema/inlined-typedefs (mapv #(rewrite-xsd % :inline-typedef) inlined))
        true      (dissoc :xml/content :xml/ns-info)))))

;;; Some of these (see UBL-UnqualifiedDataTypes-2.3.xsd) have useful content in :xsd/documentation.
;;; Toplevels for some schema:
;;;   "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2"
;;;   "urn:oasis:names:specification:ubl:schema:xsd:UnqualifiedDataTypes-2"
;;;    OAGIS Components/Fields.xsd
(declare uq-dt-common q-dt-common)

(defparse :generic/unqualified-dtype-schema [xmap {:skip-doc-processing? true}]
  (let [content (-> (xpath xmap :xsd/schema) :xml/content)
        elems   (not-empty (filter #(xml-type? % :xsd/element) content))
        types   (not-empty (filter #(xml-type? % :xsd/complexType) content))]
    (cond-> xmap
      elems (assoc  :library/content (mapv #(-> (rewrite-xsd %)
                                                (assoc :sp/function {:fn/type :type-ref}))
                                           elems))
      types (update :library/content into (mapv uq-dt-common types))
      true (dissoc :xml/ns-info :xml/content))))

(defparse :generic/qualified-dtype-schema [xmap]
  (-> xmap
      (assoc :library/content (mapv q-dt-common
                                    (filter #(or (xml-type? % :xsd/complexType) ; ToDo: Is that really it?
                                                 (xml-type? % :xsd/element))
                                            (-> (xpath xmap :xsd/schema) :xml/content))))
      (assoc :schema/imported-schemas (imported-schemas xmap))
      (assoc :mm/comment "The :schema/imported-schema is used to lookup the type being restricted.")
      (dissoc :xml/ns-info :xml/content))) ; ToDo: remove :xml/content from this dissoc if you aren't using :library/content

(defparse :oasis/component-schema
  [xmap]
  (let [topic (-> xmap :schema/name schema-topic) ; not set yet
        cc-type (cond (= topic "Components, CommonAggregate" ) :ABIE
                      (= topic "Components, CommonBasic"     ) :BBIE
                      (= topic "Components, CommonExtensions") :extensions
                      :else                                    :unknown)
        elems (->> (xpath xmap :xsd/schema)
                   :xml/content
                   (filter #(xml-type? % :xsd/element))
                   not-empty)
        comps (->> (xpath xmap :xsd/schema)
                   :xml/content
                   (filter #(xml-type? % :xsd/complexType))
                   not-empty)
        schemas (-> xmap imported-schemas not-empty)]
    (as->
        (cond-> xmap
          true  (assoc  :library/content [])
          elems (update :library/content into (mapv #(-> (rewrite-xsd % :xsd/element)
                                                         (assoc :sp/function {:fn/type :type-ref}))
                                                    elems))
          comps (update :library/content into
                        (mapv (fn [cplx]
                                (as-> (rewrite-xsd cplx :xsd/complexType) ?t
                                 (if (:sp/type ?t) (assoc ?t :term/type (:sp/type ?t)) ?t)
                                 (update ?t :sp/function #(assoc % :fn/component-type cc-type))
                                 (dissoc ?t :sp/type)))
                              comps))
          schemas (assoc :schema/imported-schemas schemas))
        ?r
      (if (-> ?r :library/content empty?) (dissoc ?r :library/content) ?r)
      (dissoc ?r :xml/ns-info :xml/content))))

(defparse :generic/code-list-schema
  [xmap]
  (let [content (-> (xpath xmap :xsd/schema) :xml/content)]
    (as-> xmap ?x
      (if (= (:schema/name ?x) "urn:oagis-10.6:CodeLists_1.xsd")
        (do ; ToDo: This one is a combination of all the others. I don't yet see the point.
          (log/warn "Skipping CodeLists_1.xsd")
          (assoc ?x :mm/debug "Skipping Code List that is an aggregate of others."))
        (assoc ?x :code-list/lists (mapv #(rewrite-xsd % :code-list)
                                        (filter #(xml-type? % :xsd/simpleType) content))))
      (dissoc ?x :xml/ns-info :xml/content))))

(defparse :iso/iso-20022-schema
  ;; Translate an ISO 20022 schema, financial codes as simple and complex types.
  [xmap]
  (let [content (-> (xpath xmap :xsd/schema) :xml/content)]
    (-> xmap
        (assoc :schema/simple-types
               (mapv #(rewrite-xsd % :xsd/simpleType)
                     (filter #(xml-type? % :xsd/simpleType) content)))
        (assoc :schema/complex-types ; ToDo: nils an uninvestigated problem in pain files.
               (filterv identity
                        (mapv #(rewrite-xsd % :xsd/complexType)
                              (filter #(xml-type? % :xsd/complexType) content))))
        (dissoc :xml/content :xml/ns-info))))

;;;--------------------- End File Level ------------------------------------------
(defparse :simple-xsd
  [obj]
  (let [tag (:xml/tag obj)]
    {tag
     (if (= :number (simple-xsd? tag))
       (-> obj :xml/attrs :value read-string)
       (-> obj :xml/attrs :value))}))

(defparse :xsd/attribute
  [obj]
  (let [attrs (:xml-attrs obj)
        required? (= "required" (:use attrs))]
    (cond-> {}
      (:name attrs) (assoc :sp/name (:name attrs)) ; ToDo: no name???
      (:type attrs) (assoc :sp/type (:type attrs)) ; Needs work, see :etsi
      required? (assoc :sp/min-occurs 1)
      required? (assoc :sp/max-occurs 1)
      true (assoc :sp/xsd-type :attribute))))

(defparse :extend-restrict-content
  [content]
  (let [enums (atom [])
        result (mapv #(if (xml-type? % :xsd/enumeration)
                        (swap! enums conj (-> % :xml/attrs :value))
                        (rewrite-xsd %))
                     content)]
    (if (not-empty @enums)
      {:model/enumeration @enums},
      {:model/sequence result})))

(defparse :xsd/extension
  [obj]
  (let [cnt (:xml/content obj)
        m (if cnt (rewrite-xsd cnt :extend-restrict-content) {})]
    (assoc m :sp/function {:fn/type :extension :fn/base (-> obj :xml/attrs :base)})))

;;; restrictions can have enumerations, factionDigits totalDigits minInclusive pattern minLength maxLength
(defparse :xsd/restriction
  [obj]
  (let [cnt (:xml/content obj)
        m (if cnt (rewrite-xsd cnt :extend-restrict-content) {})]
    (assoc m :sp/function {:fn/type :restriction :fn/base (-> obj :xml/attrs :base)})))

(defparse :xsd/choice
  [xchoice]
  {:xsd/choice (mapv rewrite-xsd (:xml/content xchoice))})

;;;
(defparse :xsd/complexType
  [xmap]
  (let [name (-> xmap :xml/attrs :name)]
    (as-> xmap ?r
      (cond (xpath ?r :xsd/complexContent)
            (-> (xpath ?r :xsd/complexContent) :xml/content first rewrite-xsd)
            (xpath ?r :xsd/simpleContent)
            (-> (xpath ?r :xsd/simpleContent) :xml/content first rewrite-xsd)
            ;; Weirdness in next two forms to handle explicit xsd:/sequence and possible
            ;; other stuff (a vector of content) which I am likewise treating as a :model/sequence.
            (== 1 (-> ?r :xml/content count))
            (rewrite-xsd (-> ?r :xml/content first))
            :else
            {:model/sequence (mapv rewrite-xsd (:xml/content ?r))})
      (if name (assoc ?r :sp/name name) ?r)
      (if (= "true" (-> xmap :xml/attrs :abstract))
        (assoc ?r :sp/abstract? true)
        ?r))))

;;; This is used at least for ISO 20022 "pain" schema. I assume one element
(defparse :xsd/simpleType
  [xmap]
  (if-not (== 1 (count (:xml/content xmap)))
    (log/warn "xsd:simpleType has many :xml/content.")
    (rewrite-xsd (-> xmap :xml/content first))))

(defparse :xsd/simpleContent
  [xmap]
  (when-not (== 1 (count (:xml/content xmap)))
    (log/warn "xsd:simpleContent has many :xml/content.")
    (rewrite-xsd (-> xmap :xml/content first))))

(defparse :xsd/complexContent
  [xmap]
  (when-not (== 1 (count (:xml/content xmap)))
    (log/warn "xsd:complexContent has many :xml/content.")
    (rewrite-xsd (-> xmap :xml/content first))))

(defparse :xsd/any
  [xmap]
  (let [attrs (:xml/attrs xmap)]
    (cond-> {:sp/xsd-type :any}
      true (assoc :doc/doc-string ; ToDo: Investigate later
                  (str (:namespace attrs) "|" (:processContents attrs)))
      (:minOccurs attrs) (assoc :sp/min-occurs (-> attrs :minOccurs keyword))
      (:maxOccurs attrs) (assoc :sp/max-occurs (-> attrs :maxOccurs keyword)))))

(defparse :xsd/group
  [xmap]
  (-> {:sp/xsd-type :any}
      (assoc :sp/ref (-> xmap :xml/attrs :ref))))

(defparse :xsd/sequence
  [xmap]
  (let [content (:xml/content xmap)]
    ;(if (= 1 (count content))
    ; (rewrite-xsd (first content)) ; ToDo: OK practice? The caller has to handle a ref.
      {:model/sequence (mapv rewrite-xsd content)}))

(defparse :xsd/element
  [xelem]
  (assert (and (xml-type? xelem :xsd/element)
               (or (-> xelem :xml/attrs :ref)     ; UBL, OAGIS/Components.xsd
                   (-> xelem :xml/attrs :name)))) ; ISO/OAGIS
  (let [attrs   (:xml/attrs xelem)
        doc     (xpath- xelem :xsd/annotation :xsd/documentation) ; Not commonly used! (Is used in  MF challenge problem.)
        doc?    (when-not (-> doc :xml/content string?) doc)
        comp?   (when doc? (rewrite-xsd doc? :cct-component))
        doc-str (when (-> doc :xml/content string?) (:xml/content doc))]
    (cond-> {}
      (:ref  attrs)       (assoc :sp/name (:ref  attrs))
      (:name attrs)       (assoc :sp/name (:name attrs))
      (:type attrs)       (assoc :sp/type (:type attrs))
      (:use  attrs)       (assoc :sp/user (:use  attrs))
      (:minOccurs attrs)  (assoc :sp/min-occurs (-> attrs :minOccurs keyword))
      (:maxOccurs attrs)  (assoc :sp/max-occurs (-> attrs :maxOccurs keyword))
      doc?                (assoc :sp/function {:fn/type :cct-component}),
      comp?               (assoc :sp/component comp?)
      doc-str             (assoc :sp/doc-string doc-str)
      true (assoc :sp/function {:fn/type :gelem}))))

;;; ToDo: better that this would be a set of things unhandled.
;;; Write them as :mm/unhandled-xml (a stringified map).
(defparse :xsd/anyAttribute
  [xmap]
  (log/warn "anyAttribute = " xmap)
  {})


;;; In OAGIS and UBL schema, these are parsed by other means. Not so in QIF, where it is just text.
(defparse :xsd/annotation
  [xmap]
  (->> (xpath- xmap :xsd/documentation)
       :xml/content
       not-empty))

;;; This just returns the string pointing to a .xsd file. Those will be replaced in postprocessing.
(defparse :xsd/include
  [xmap]
  (-> xmap :xml/attrs :schemaLocation))

;;;---------------------------------- 'Custom' ---------------------------------------------------
(def cct-renames
  {:ccts/Cardinality :cct/Cardinality,
   :ccts/CategoryCode  :cct/CategoryCode,
   :ccts/DataTypeTermName :cct/DataTypeTermName,
   :ccts/Definition :cct/Definition,
   :ccts/Description :cct/Description,
   :ccts/DictionaryEntryName :cct/DictionaryEntryName,
   :ccts/Name :cct/Name,
   :ccts/ObjectClass :cct/ObjectClass,
   :ccts/PrimitiveType :cct/PrimitiveType,
   :ccts/PropertyTermName :cct/PropertyTermName,
   :ccts/QualifierTerm :cct/QualifierTerm,
   :ccts/RepresentationTermName :cct/RepresentationTermName,
   :ccts/UniqueID :cct/UniqueID,
   :ccts/UsageRule :cct/UsageRule,
   :ccts/VersionID :cct/VersionID,
   :ccts/sc-id :cct/sc-id,
   :ccts/sc-type :cct/sc-type,
   :ccts/sc-use :cct/sc-use,
   :ccts/supplemental :cct/supplemental})

(def oagis2ccts-key-map
  "Translate names of properties found in OAGIS to the equivalent used in the database."
  (let [key-names (->> db-schema (map #(get % :db/ident)) (filter #(and % (= (namespace %) "cct"))))
        oagis-names (map #(keyword "ROOT" (str "ccts_" (name %))) key-names)]
    (zipmap oagis-names key-names)))

(defn fix-ccts-keys
  "Some files uses keywords qualified by 'ccts', I use 'cct' everywhere."
  [k]
  (or (cct-renames k) (oagis2ccts-key-map k) k))

(defn cc-from-doc
  "Return a map describe a core component or supplementary component from the
   argument :xsd/documentation."
  [xmap]
  (assert (xml-type? xmap :xsd/documentation))
  (not-empty (reduce (fn [m info]
                       (if-let [content (:xml/content info)]
                         (assoc m (-> info :xml/tag fix-ccts-keys) content)
                         m))
                     {}
                     (:xml/content xmap))))

(defparse :cct-component
  [xmap]
  (assert (xml-type? xmap :xsd/documentation))
  (cc-from-doc xmap))

;;; This is for processing the :xsd/documentation under the simpleContent (supplemental component)
;;; under complexContent the :xsd/documentation has the main component.
(defparse :cct-supplementary
  [xmap]
  (assert (xml-type? xmap :xsd/simpleContent))
  (let [res?  (xpath- xmap :xsd/restriction)
        ext?  (xpath- xmap :xsd/restriction)
        attr  (xpath- (or res? ext?) :xsd/attribute)
        name  (-> attr :xml/attrs :name)
        type  (-> attr :xml/attrs :type)
        use   (-> attr :xml/attrs :use)
        doc   (xpath- attr :xsd/annotation :xsd/documentation)]
    (cond-> {}
      true (assoc :sp/function {:fn/type :cct-supplementary-component})
      name (assoc :cct/sc-id   name)
      type (assoc :cct/sc-type type)
      use  (assoc :cct/sc-use  use)
      doc  (assoc :cct/supplementary (cc-from-doc doc)))))

;;; (def ubl-udt-file "/Users/pdenno/Documents/specs/OASIS/UBL-2.3/xsdrt/common/UBL-UnqualifiedDataTypes-2.3.xsd")
;;; (-> ubl-udt-file read-clean rewrite-xsd)
(defn uq-dt-common
  "Process common parts of CEFACT and OASIS unqualified datatypes.
   These (see UBL-UnqualifiedDataTypes-2.3.xsd are the things with useful
   content in :xsd/documentation."
  [cplx-type]
  (assert (xml-type? cplx-type  :xsd/complexType)) ; ToDo: top part is similar to :inline-typedef
  (let [doc?  (let [doc (xpath- cplx-type :xsd/annotation :xsd/documentation)]
                (when-not (-> doc :xml/content string?) doc))
        sup?  (xpath- cplx-type :xsd/simpleContent)
        elems (not-empty (filter #(xml-type? % :xsd/element)
                                 (:xml/content (xpath cplx-type :xsd/sequence))))]
    (cond-> {}
      true  (assoc :sp/type (-> cplx-type :xml/attrs :name)),
      true  (assoc :sp/function {:fn/type :sequence}),
      doc?  (assoc :sp/function {:fn/type :cct-component}),
      doc?  (assoc :sp/component     (rewrite-xsd doc? :cct-component))
      sup?  (assoc :sp/supplementary (rewrite-xsd sup? :cct-supplementary))
      elems (assoc :model/sequence (mapv rewrite-xsd elems)))))


;;; ToDo: maybe rewrite this to have something like namespace for the references "udt". "schema/referenced-schema
(defn q-dt-common
  "Process one qualified datatype schema element."
  [xmap]
  (assert (or (xml-type? xmap :xsd/complexType)
              (xml-type? xmap :xsd/element)))
  (let [name (-> xmap :xml/attrs :name)]
    (-> xmap
        rewrite-xsd
        (assoc :term/name name)
        (dissoc :sp/name))))

(defparse :oagis-type-attr
  [xsd-attr]
  (let [attr (:xml/attrs xsd-attr)]
    (cond-> {}
      true         (assoc :sp/name (:name attr))
      (:type attr) (assoc :sp/type (:type attr))
      (= (:use attr) "optional") (assoc :sp/min-occurs :0)
      (= (:use attr) "optional") (assoc :sp/max-occurs :1))))

(defparse :inline-typedef ; OAGIS only AFAIK
  [cplx-type]
  (assert (xml-type? cplx-type :xsd/complexType))
  (let [ext?  (xpath- cplx-type :xsd/complexContent :xsd/extension)
        res?  (xpath- cplx-type :xsd/complexContent :xsd/restriction)
        attrs (not-empty (filter #(xml-type? % :xsd/attribute) cplx-type))
        elems (not-empty
               (cond ext?  (filter #(xml-type? % :xsd/element)
                                  (:xml/content (xpath ext? :xsd/sequence)))
                     res?  (filter #(xml-type? % :xsd/element)
                                  (:xml/content (xpath res? :xsd/sequence)))
                     :else (filter #(xml-type? % :xsd/element)
                                  (:xml/content (xpath cplx-type :xsd/sequence)))))]
    (cond-> {}
      true    (assoc :sp/type (-> cplx-type :xml/attrs :name)),
      true    (assoc :sp/function {:fn/type :sequence}),
      ext?    (assoc :sp/function {:fn/type :extension   :fn/base (-> ext? :xml/attrs :base)}),
      res?    (assoc :sp/function {:fn/type :restriction :fn/base (-> res? :xml/attrs :base)}),
      attrs   (assoc :schema/type-attrs (mapv #(rewrite-xsd % :oagis-type-attr) attrs)),
      elems   (assoc :model/sequence    (mapv #(rewrite-xsd % :xsd/element) elems)))))


(defparse :oagis-ccts-def
  ;; Argument is a vector of :ROOT/ccts_whatever properties.
  ;; Return a msp of these translated :cct/whatever.
  [ccts-tags]
  (reduce (fn [m elem]
            (let [tag (:xml/tag elem)]
              (if-let [ccts-tag (oagis2ccts-key-map tag)]
                (if-let [content (:xml/content elem)]
                  (assoc m ccts-tag content)
                  m)
                (if (= tag :ROOT/CodeName)
                  (assoc m :iso/CodeName (:xml/content elem))
                  (do (log/warn "Unknown code-term tag" tag) m)))))
          {}
          ccts-tags))

(defparse :code-term
  ;; Return a map of the CCTS and ISO properties defined by an OAGIS or OAGIS/ISO code term.
  [xmap {:skip-doc-processing? true}]
  (assert (xml-type? xmap :xsd/enumeration))
  (let [term (-> xmap :xml/attrs :value)
        doc    (xpath xmap :xsd/annotation :xsd/documentation)]
    ;; Some code lists, like OAGIS CodeList_CurrencyCode_ISO_7_04, don't have documentation.
    {term (rewrite-xsd (:xml/content doc) :oagis-ccts-def)}))

(defparse :code-list
  ;; Walk through a code list collecting terms.
  [xmap]
  (assert (xml-type? xmap :xsd/simpleType))
  (-> {}
      (assoc :code-list/name (-> xmap :xml/attrs :name))
      (assoc :sp/function {:fn/type :code-list})
      (assoc :xsd/restriction (rewrite-xsd (xpath xmap :xsd/restriction) :xsd/restriction))
      (assoc :code-list/terms (reduce (fn [m v]
                                        (merge m (rewrite-xsd v :code-term)))
                                      {}
                                      (filter #(xml-type? % :xsd/enumeration)
                                              (:xml/content (xpath xmap :xsd/restriction)))))
      (dissoc :xsd/restriction)))

;;;=========================== Schema Operations ===========================================
(defn list-schemas
  "Return a list of schema, by default they are sorted by 'topic'"
  [& {:keys [sdo sort?] :or {sort? true}}]
  (let [base-names
        (if sdo
          (d/q `[:find [?n ...] :where [?s :schema/name ?n] [?s :schema/sdo ~sdo]] @conn)
          (d/q '[:find [?n ...] :where [_ :schema/name ?n]] @conn))]
    (if sort?
      (let [urns&topics (map (fn [urn topic] {:urn urn :topic topic})
                             base-names
                             (map q-schema-topic base-names))]
        (->> urns&topics
             (sort-by :topic)
             (mapv :urn)))
      (vec base-names))))

(defn get-schema
  "Return the map stored in the database for the given schema-urn. Useful in development.
    :filter-set - DB attribute to leave out (e.g. #{:db/id} or #{:db/doc-string}) "
  [schema-urn & {:keys [resolve? filter-set] :or {resolve? true filter-set #{:doc/doc-string}}}]
  (when-let [ent  (d/q `[:find ?ent .
                         :where [?ent :schema/name ~schema-urn]] @conn)]
    (cond-> (dp/pull @conn '[*] ent)
      resolve? (du/resolve-db-id conn filter-set))))

(def diag-path "Pathname for debugging. Keep." (atom nil))

;;; (read-schema-file "data/testing/elena/Company A - Invoice.xsd")
(defn read-schema-file
  "Create map structure for the DB for the given file."
  [path]
  (-> path read-clean rewrite-xsd du/condition-form vector))

(defn add-schema-file!
  [path]
  (reset! diag-path path)
  (let [db-content (read-schema-file path)]
    (try
      (if (du/storable? db-content)
        (try (d/transact conn db-content) ; Use d/transact here, not transact! which uses a future.
             (catch Exception e
               (swap! bad-file-on-rebuild? conj path)
               (log/error "Error adding" path ":" e)))
        (do (swap! bad-file-on-rebuild? conj path)
            (log/error "Schema-map contains nils and cannot be stored." path)))
      (catch Exception e
        (swap! bad-file-on-rebuild? conj path)
        (log/error "Error checking storable?" path ":" e)))))

(defn add-schema-files!
  "Read a directory of files into the database.
   They consist of a rewritten (see rewrite-xsd) maps with some useful metadata.
    DIR is the directory to read from. All the .xsd files there will be added to the DB."
  [dir]
  (let [grammar-matcher (.getPathMatcher
                          (java.nio.file.FileSystems/getDefault)
                          "glob:*.xsd")
        files (->> (file-seq (io/file dir))
                   (filter #(.isFile %))
                   (filter #(.matches grammar-matcher (.getFileName (.toPath %))))
                   (mapv #(.getAbsolutePath %)))]
    (doseq [file files]
      (add-schema-file! file))))

(defn update-bad-files!
  "On rebuild, note what couldn't be read."
  []
  (when (:rebuild-db? db-cfg)
    (doseq [file @bad-file-on-rebuild?]
      (d/transact conn [{:schema/pathname file
                         :mm/file-not-read? true}]))))

(defn bad-files
  "This creates a 'to do' list for debugging!"
  []
  (d/q '[:find [?p ...] :where
         [?ent :mm/file-not-read? true]
         [?ent :schema/pathname ?p]]
       @conn))

(defn unused-attrs
  "Return a list of unused database attributes (for debugging)."
  []
  (let [unused (atom [])]
    (doseq [attr (map :db/ident db-schema)]
      (when (empty? (d/q `[:find ?e :where [?e ~attr ?]] @conn))
        (swap! unused conj attr)))
    @unused))

;;; =================================== Schema-db post-processing ==============================
(defn add-topics! []
  (let [forms (reduce (fn [forms urn]
                        (if-let [topic (schema-topic urn)]
                          (conj forms {:schema/name urn :schema/topic topic})
                          forms))
                      []
                      (list-schemas))]
    (d/transact conn forms)))

(defn fix-includes! ; Just used on OAGIS schema, UBL-CommonExtensionComponents-2.3.xsd AFAIK.
  "Files have :mm/temp-include which are paths (typically relative)
   Add :schema/included-schemas where these are resolved to references to :schema/name."
  []
  (let [temps (d/q '[:find ?ent ?i ?p :keys s/ent s/include-file s/s-path :where
                     [?ent :mm/temp-include ?i]
                     [?ent :schema/pathname ?p]]
                   @conn)
        with-urn (map (fn [temp]
                        (let [[_ up _ ipath] (re-matches #"^((\.\./)*)(.*)$" (:s/include-file temp))
                              schema-path (clojure.java.io/file (:s/s-path temp))
                              up-cnt (inc (int (/ (count up) 3)))
                              dir    (du/dir-up schema-path up-cnt)
                              file   (str dir "/" ipath)]
                          (if-let [urn (d/q `[:find ?urn . :where
                                              [?e :schema/pathname ~file]
                                              [?e :schema/name ?urn]]
                                            @conn)]
                            (assoc temp :s/include urn)
                            (do (log/warn "While resolving includes, cannot find schema with :schema/pathname" file)
                                temp))))
                      temps)]
    (d/transact conn
                (mapv #(-> {}
                           (assoc :db/id (:s/ent %))
                           (assoc :schema/included-schemas (:s/include %)))
                      (filter #(contains? % :s/include) with-urn)))))

(defn postprocess-schemas!
  "Do some additional work on schema already in the DB."
  []
  (add-topics!)
  (fix-includes!)
  (update-bad-files!))

;;; ================================ Expand: resolve to atomic schema parts  ===========================
;;; In the process, keep track of the sp's :sp*/children, a property that is not in the DB owing to
;;; how much repetitive content that would produce for most schema and profiles.
;;; The idea of the expand methods is to serve client queries *indirectly* from DB content.
(defn inlined-typedef-ref
  "Return the object defining the argument type-term in the argument schema
  using inlined schema."
  [type-term schema-urn]
  (when-let [ent (d/q `[:find ?ent . :where
                          [?s :schema/name ~schema-urn]
                          [?s :schema/inlined-typedefs ?ent]
                          [?ent :sp/type ~type-term]]
                      @conn)]
    (-> (du/resolve-db-id {:db/id ent} conn)
        (assoc :mm/access-method :inlined-typedef))))

(defn imported-typedef-ref
  "Return the object defining the argument type-term in the argument schema
   using imported schema."
  [type-term schema-urn]
  (let [[prefix term] (clojure.string/split type-term #":")]
    (when (and term prefix) ; Without the prefix things go awry!
      (let [[{:mm/keys [ent lib]}]
                (d/q `[:find ?ref2 ?s2-name
                       :keys mm/ent mm/lib
                       :where
                       [?s1    :schema/name ~schema-urn]
                       [?s1    :schema/imported-schemas ?i]
                       [?i     :import/prefix ~prefix]
                       [?i     :import/referenced-schema ?s2-name]
                       [?s2    :schema/name ?s2-name]
                       [?s2    :library/content ?ref1] ; many :db/id
                       [?ref1  :sp/name ~term]
                       [?ref1  :sp/function ?fn]
                       [?fn    :fn/type :type-ref]
                       [?ref1  :sp/type ?type]
                       [?s2    :library/content ?ref2]
                       [?ref2  :sp/name ?type]]
                     @conn)]
        (when (and ent lib)
          (-> (du/resolve-db-id {:db/id ent} conn)
              (assoc :mm/lib-where-found lib)
              (assoc :mm/access-method :imported-typedef)))))))

(defn model-sequence-type-ref
  [type-term schema-urn]
  (when-let [ent (d/q `[:find ?m . :where
                        [?s  :schema/name ~schema-urn]
                        [?s  :model/sequence ?m]
                        [?m  :sp/type ~type-term]]
                      @conn)]
    (-> (du/resolve-db-id {:db/id ent} conn)
        (assoc :mm/access-method :model-sequence-type))))

#_(defn model-sequence-name-ref
  "Return the object that has term somewhere in its :model/sequence."
  [term schema-urn]
  (when-let [ent (d/q `[:find ?m . :where
                        [?s  :schema/name ~schema-urn]
                        [?s  :model/sequence ?m]
                        [?m  :sp/name ~term]]
                      @conn)]
    (let [found (du/resolve-db-id {:db/id ent} conn)]
      (-> {}
          (assoc :mm/access-method :model-sequence-name)
          (assoc :model/sequence (:model/sequence found))))))

(defn schema-ref
  "The term is found in the :model/sequence of a schema; return the schema."
  [term schema-urn]
  (when-let [ent (d/q `[:find ?s . :where
                        [?s  :schema/name ~schema-urn]
                        [?s  :schema/type :ccts/message-schema]
                        [?s  :model/sequence ?m]
                        [?m  :sp/name ~term]
                        [?m  :sp/type ?type]]
                      @conn)]
    (let [found (du/resolve-db-id {:db/id ent} conn)]
      (-> {} ; I'm not keeping much of the schema!
          (assoc :db/id          ent)
          (assoc :schema/type    :ccts/message-schema)
          (assoc :model/sequence (:model/sequence found))
          (assoc :mm/access-method :schema-ref)))))

(defn included-typedef-ref ; This is just for OAGIS, AFAIK.
  "Return the object defining the argument type-term in the argument schema
   using included schema."
  [type-term schema-urn]
  (when-let [ent (d/q `[:find ?s2 #_?cont . :where
                        [?s1 :schema/name ~schema-urn]
                        [?s1 :schema/included-schemas ?i]
                        [?s2 :schema/name ?i]
                        [?s2 :sp/name ~type-term]
                        #_[?cont  :term/type ~type-term]]
                      @conn)]
    (-> (du/resolve-db-id {:db/id ent} conn)
        (assoc :mm/access-method :included-typedef))))

;;;(library-lookup-ref "UBLExtension" "urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2")
(defn library-lookup-ref
  "Find the term as :library/content"
  [term schema-urn]
  (when-let [ent (d/q `[:find ?c . :where
                        [?s :schema/name ~schema-urn]
                        [?s :library/content ?c]
                        [?c :sp/name ~term]]
                      @conn)]
    (-> (du/resolve-db-id {:db/id ent} conn)
        (assoc :mm/access-method :library-lookup))))

;;; (term-ref ubl-invoice "InvoiceType")    ; get from inlined.
;;; (term-ref ubl-invoice "cbc:IssueDate")  ; get from imported.
;;; (term-ref ubl-invoice "cac:InvoiceLine") ; get from imported.
;;; (term-ref oagis-invoice "InvoiceLine") ; get from included
(defn term-ref
  "Return the object referenced by the term in schema."
  [term schema-urn]
  (or (library-lookup-ref      term schema-urn)
      (inlined-typedef-ref     term schema-urn)
      (imported-typedef-ref    term schema-urn)
      (included-typedef-ref    term schema-urn)
      (model-sequence-type-ref term schema-urn)
      (schema-ref              term schema-urn)
      #_(model-sequence-name-ref term schema-urn)
      (throw (ex-info (str "In term-ref could not resolve " term " " schema-urn)
                      {:term term :schema-urn schema-urn}))))

;;; ToDo: Remember that once you run this, it is in the schema from then on.
;;; (get-schema "small-invoice-schema-1")
(defn store-test [] (->> "small-invoice-schema-1.edn" slurp read-string vector (d/transact conn)))
;;; (expand "Invoice" "small-invoice-schema-1")

(defn expand-type
  [found]
  (cond (= (:mm/access-method found) :library-lookup) :type-def
        (s/valid? ::ccts-based-message-schema found)  :ccts/message-schema
        (s/valid? ::tagged found)          :tagged
        (s/valid? ::type-ref found)        :type-ref
        (s/valid? ::model-seq found)       :model-seq
        :else nil))

(defn expand [term schema]
  (letfn [(expand-aux [obj term schema]
            (let [res  (term-ref term schema)
                  type (expand-type res)]
              (println "\n\n term = " term  "\n res =" res "\n type =" type)
              (case type
                :type-def res
                :tagged   (-> res
                              (assoc :expand/method ::tagged)
                              (assoc :sp/type (:sp/name res))
                              (assoc :sp/name term)
                              #_(dissoc :mm/access-method))
               :type-ref  (-> (expand-aux obj (:sp/type res) schema)
                               (assoc :expand/method ::type-ref)
                               (assoc :sp/name (:sp/name res)))
               :ccts/message-schema (-> obj
                                    (assoc :expand/method :ccts/message-schema)
                                    (assoc :sp/name term)
                                    (assoc :sp/children (mapv #(expand-aux {} (:sp/type %) schema)
                                                              (:model/sequence res))))
               :model-seq (-> obj
                               (assoc :expand/method ::model-seq)
                               (assoc :sp/name term)
                               (assoc :sp/children (mapv #(expand-aux {} (:sp/name %) schema)
                                                         (:model/sequence res))))
               nil (log/warn "Cannot expand term" term "for schema" schema))))]
    (expand-aux {} term schema)))

(defn sp-defaults
  [sp]
  (cond-> sp
    (not (:sp/min-occurs sp)) (assoc :sp/min-occurs 1)
    (not (:sp/max-occurs sp)) (assoc :sp/max-occurs 1)
    (not (:sp/type       sp)) (assoc :sp/type :mm/undefined)))

;;;=========================================================================================================
(defn get-term-type
  "Return a map describing what is known about the argument data type.
    - Schema-urn is a string
    - term is a string naming a schema object such as a data type (e.g. 'AmountType')"
  [schema-urn term]
  (when-let [ent (or (d/q `[:find ?content .
                            :where
                            [?schema  :schema/name ~schema-urn]
                            [?schema  :library/content ?content]
                            [?content :term/type ~term]
                            [?content :sp/function ?fn]
                            [?fn      :fn/component-type :ABIE]]
                          @conn) ; ToDo: Datahike OR an NOT queries not implemented??? Use predicate?
                     (d/q `[:find ?content .
                            :where
                            [?schema  :schema/name ~schema-urn]
                            [?schema  :library/content ?content]
                            [?content :term/type ~term]
                            [?content :sp/function ?fn]
                            [?fn      :fn/component-type :BBIE]]
                          @conn))]
    (du/resolve-db-id (dp/pull @conn '[*] ent) conn)))

;;; (get-term "urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2" "InvoiceLine")
;;; (get-term "urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2" "DeliveryTerms")
;;; (get-term "urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2" "BestBeforeDate")
;;; I seem to need something different for 'types' e.g. "InvoiceLineType"
(defn get-term
  "Return a map describing what is known about the argument data type.
    - Schema-urn is a string
    - term is a string naming a schema object such as a data type (e.g. 'AmountType')"
  [schema-urn term & {:keys [expand-type?] :or {expand-type? true}}]
  (when-let [ent (d/q `[:find ?content .
                        :where
                        [?schema :schema/name ~schema-urn]
                        [?schema :library/content ?content]
                        [?content :term/name ~term]]
                      @conn)]
    (let [not-typed (du/resolve-db-id (dp/pull @conn '[*] ent) conn)]
      (if expand-type?
        (if-let [type (:term/type not-typed)]
          (if-let [type-info (get-term-type schema-urn type)]
            (assoc not-typed :type-info type-info)
            not-typed)
          not-typed)
        not-typed))))

(defn get-term-schemas
  "Return all the schema containing the term (currently schema/term)"
  [term]
  (when-let [ents (not-empty (d/q `[:find [?ent ...]
                                    :where [?ent :term/name ~term]]
                                  @conn))]
    (mapv (fn [ent] (d/q `[:find ?s .
                           :where
                           [?ent :library/content ~ent]
                           [?ent :schema/name ?s]]
                         @conn))
          ents)))

;;;============================ Resolvers (communication with clients)  ==================================
;;; I think the key idea here for pathom-mediated composabiltiy is for each resolver to rename all db/id
;;; to context specific names. These are currently #{:sdb/schema-id :sdb/elem-id :sdb/imported-schema-id}.
;;; (The last one isn't just a schema but a prefix too.)
;;; The simplest composition then is implemented as a flat table with values and foreign key references.
;;; I think, however, you can 'go deep' in the ::pc/output and still maintain. See all-schema-ids-r.
;;; See also person-resolver at Part 6, 43:26.

;;; For debugging
(defn query-for [pathom-eql] ((var-get (resolve 'rad-mapper.pathom/parser)) {} pathom-eql))

(defn mf-spec
  "Get Michael's TestSpecification Schema"
  []
  (let [sname "urn:oagis-10.6:TestSpecification-MF"]
    (as-> sname ?s
      (query-for [{[:schema/name ?s] [:sdb/schema-id]}])
      (get ?s [:schema/name sname])
      (:sdb/schema-id ?s)
      {:db/id ?s}
      (du/resolve-db-id ?s conn))))

(defn mf-meth
  "Get Michael's TestMethod Schema"
  []
  (query-for [{[:schema/name "urn:oagis-10.6:TestMethod-MF"] [:sdb/schema-id]}]))

;;; Note that when you send an Ident, you get back a map with that ident and the response <=========
;;;(tryme [{[:schema/name "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"] [:sdb/schema-id]}])
;;; ==> {[:schema/name "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"] #:sdb{:schema-id 1230}}
(pc/defresolver schema-by-name-r [env {:schema/keys [name]}]
  {::pc/input #{:schema/name}
   ::pc/output [:sdb/schema-id]}
  {:sdb/schema-id (d/q `[:find ?e . :where [?e :schema/name ~name]] @conn)})

;;; This is based on the book. https://book.fulcrologic.com/#GoingRemote (See friends-resolver
;;; (tryme [{:ccts/message-schema [:list/id  {:list/schemas [:sdb/schema-id :schema/name]}]}]) ; RIGHT!
;;; (tryme [{[:list/id :ccts/message-schema] {:list/schemas [:sdb/schema-id :schema/name]}}])  ; WRONG! WHY?
(pc/defresolver list-r [env {:list/keys [id]}] ; e.g :list/id = :ccts/message-schema
  {::pc/input  #{:list/id}
   ::pc/output [{:list/schemas [:sdb/schema-id :schema/name]}]}
  (when (= id :ccts/message-schema)
    (when-let [schema-maps (->>
                            (d/q `[:find ?ent ?name ?topic
                                   :keys sdb/schema-id schema/name schema/topic
                                   :where
                                   [?ent :schema/name  ?name]
                                   [?ent :schema/topic ?topic]
                                   [?ent :schema/type ~id]]
                                 @conn)
                            (sort-by :schema/topic)
                            (mapv #(dissoc % :schema/topic))
                            not-empty)]
      {:list/id id
       :list/schemas schema-maps})))

(pc/defresolver message-schema-r [env input] ; THIS ONE WORKS (But...)
  {::pc/output [{:ccts/message-schema [:list/id {:list/schemas [:schema/name :sdb/schema-id]}]}]}
  {:ccts/message-schema {:list/id :ccts/message-schema}})

;;; (tryme [{[:sdb/schema-id 5173] [{:model/sequence [{:sdb/elem-id [:sp/name :sp/type :sp/min-occurs :sp/max-occurs]}]}]}])
;;; (tryme [{[:sdb/schema-id 5173] [{:model/sequence [:sp/name :sp/type]}]}])
;;; (tryme [{[:sdb/schema-id 5173] [:schema/name]}])
;;; (tryme [{[:sdb/schema-id 5173] [:model/sequence]}])
(pc/defresolver schema-props-r [env {:sdb/keys [schema-id]}]
  {::pc/input #{:sdb/schema-id}
   ::pc/output [:schema/name :sdb/schema-id :schema/sdo :schema/type :schema/topic
                :schema/subversion :schema/inlined-typedefs :schema/spec
                {:schema/imported-schemas [:sdb/imported-schema-id]}
                {:model/sequence [:sdb/elem-id]}]}
  (-> (dp/pull @conn '[*] schema-id) ; ToDo: could also do the :keys thing on the pull.
      (update :model/sequence (fn [s] (mapv #(-> % (assoc :sdb/elem-id (:db/id %)) (dissoc :db/id)) s)))
      (update :schema/imported-schemas
              (fn [s]
                (mapv #(-> %
                           (assoc :sdb/imported-schema-id (:db/id %))
                           (dissoc :db/id)) s)))))

;;; (tryme [{[:sdb/elem-id 1280] [:schema/min-occurs]}])                          ; Simple
;;; (tryme [{[:schema/name invoice] [{[:sdb/elem-id 1280] [:schema/min-occurs]}]}]) ; YES!
;;; (tryme [{[:schema/name invoice] [{:model/sequence [:sp/name :sp/type :sp/min-occurs :sp/max-occurs]}]}]) ; COMPLETE!
;;; (tryme [{[:sdb/schema-id 1230] [{:model/sequence [:sp/name :sp/type :sp/min-occurs :sp/max-occurs]}]}]) ; COMPLETE!
(pc/defresolver elem-props-r [env {:sdb/keys [elem-id]}]
  {::pc/input #{:sdb/elem-id}
   ::pc/output [:doc/doc-string
                :sp/name
                :sp/type
                :sp/min-occurs
                :sp/max-occurs]}
  (dp/pull @conn '[*] elem-id))

;;; Nice thing about pathom (relative to GraphQL) is that you don't have to start at the root.
;;; This has nothing to do with ::pc/input; you can add this to a query anywhere.
(pc/defresolver current-system-time-r [_ _]
  {::pc/output [:server/time]}
  {:server/time (java.util.Date.)})

;;; ToDo: Looks suspicious!
;;; Try it with (currently!):
;;;           [:sdb/elem-id 1305] (UBL "Invoice" element)
;;;           [:sdb/elem-id 5230] (OAGIS "Invoice" element)
(defn substructure-resolver-r
  "Return substructure for the argument elem-id"
  [elem-id]
  (if-let [owning-schema (d/q `[:find ?s :where [?s :model/sequence ~elem-id]] @conn)]
    (let [ref-1 (d/q `[:find [?rs ...] :where [~owning-schema :schema/imported-schemas ?rs]] @conn)
          ref-2 (d/q `[:find [?rs ...] :where [~owning-schema :schema/inlined-typedefs  ?rs]] @conn)
          _refs (into ref-1 ref-2)
          _elem-info (dp/pull @conn '[*] elem-id)]
      (if-let [_ref-schema nil #_(schema-containing-ref)]
        :found-it
        (log/warn "Could not find referenced schema for" elem-id)))
    (log/warn "Could not find owning schema for" elem-id)))

;;; ToDo: Of course, this needs to take an argument or be part of a user's project, etc.
;;; (tryme [{[:file/id :map-spec] [:user/data-file]}])
(pc/defresolver data-file-r [env {:file/keys [id]}]
  {::pc/input #{:file/id}
   ::pc/output [:file/text]}
  (case id
    :source-data {:file/text (slurp "./data/messages/UBL-Invoice-2.1-Example.xml")}
    :target-data {:file/text " "}
    :map-spec    {:file/text (str   "// Map spec started " (java.util.Date.))}))

(def resolvers [schema-by-name-r
                schema-props-r
                elem-props-r
                list-r
                message-schema-r
                data-file-r
                current-system-time-r])

;;;================================ Starting and Stopping ===========================================
;;; (user/restart) whenever you update the DB or the resolvers. (tools/refresh) if compilation fails.

(defn create-db!
  "Create the database if :rebuild? is true, otherwise just set the connection atom, conn."
  []
  (when (:rebuild-db? db-cfg)
    (reset! bad-file-on-rebuild? #{})
    (when (d/database-exists? db-cfg) (d/delete-database db-cfg))
    (d/create-database db-cfg)
    (alter-var-root (var conn) (fn [_] (d/connect db-cfg)))
    (d/transact conn db-schema)
    (add-schema-files! (str ubl-root "maindoc"))
    (add-schema-files! (str ubl-root "common"))
;;;;;    (add-schema-files! (str oagis-10-6-root "Nouns"))
;;;;;    (add-schema-files! (str oagis-10-6-root "Platform/2_6/Common"))
    (add-schema-files! (str oagis-10-8-root "Nouns"))
    (add-schema-files! (str oagis-10-8-root "Platform/2_7/Common"))
    (add-schema-files! (str qif-root "QIFApplications"))
    (add-schema-files! (str qif-root "QIFLibrary"))
    (add-schema-files! michael-root)
    (postprocess-schemas!)
    (log/info "Created schema DB")))

(defn connect-db
  "Set the var rad-mapper.schema-db/conn by doing a d/connect."
  []
  (if (d/database-exists? db-cfg)
    (alter-var-root (var conn) (fn [_] (d/connect db-cfg))),
    (log/warn "There is no DB to connect to.")))

(defstate schema
  :start
  (do
    (util/config-log :info)
    (connect-db)))
