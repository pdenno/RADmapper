(ns rad-mapper.data-util.generic-schema
  "Functions to read XML to structures that the DB can use."
  (:require
   [rad-mapper.data-util.macros      :as mac :refer [defparse rewrite-xsd]]
   [rad-mapper.data-util.db-util     :as du  :refer [xpath xpath- xml-type?]]
   [rad-mapper.data-util.schema-util :as su  :refer [schema-sdo schema-spec schema-version schema-subversion schema-type schema-name]]
   [taoensso.timbre                  :as log]))

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
                 (conj res {:import/prefix prefix :import/referencedSchema schema})
                 (do (log/warn "No prefix for schema" schema) res)))
            []
            ischemas)))

(defn read-clean
  "Return a map structure containing the :xml/content (cleaned-up) :ns-info and :schema info."
  [pathname]
  (let [xml (du/read-xml pathname)]
    (as-> xml ?xmap
        (assoc ?xmap :schema/sdo (schema-sdo ?xmap))
        (assoc ?xmap :schema/spec (schema-spec ?xmap))
        (assoc ?xmap :schema/version (schema-version ?xmap))
        (assoc ?xmap :schema/subversion (schema-subversion ?xmap))
        (assoc ?xmap :schema/type (schema-type ?xmap))
        (assoc ?xmap :schema/name (schema-name ?xmap)))))

;;; (read-schema-file "data/testing/elena/Company A - Invoice.xsd")
(defn read-schema-file
  "Create map structure for the DB for the given file.
   This sets :schema/type, which determines what method rewrite-xsd executes.
   See su/rewrite-xsd-dispatch."
  [path]
  (-> path
      read-clean
      rewrite-xsd
      du/condition-form vector))

;;;===============  File Level =========================================================
;;; ToDo: Adapted from :oasis/component-schema, which suggests that that could use this.
;;; ToDo: This will prove to be woefully inadequate...later!
(defparse :generic/xsd-file
  [xmap]
  (let [{:xsd/keys [element complexType simpleType]}
        (-> (xpath xmap :xsd/schema) :xml/content du/child-types)
        schemas (-> xmap imported-schemas not-empty)]
    (as->
        (cond-> xmap
          true  (assoc  :schema/content [])
          element     (update :schema/content into
                              (mapv #(-> (rewrite-xsd % :xsd/element)
                                         (assoc :sp/function {:fn/type :type-ref}))
                                    element))
          complexType (update :schema/content into
                        (mapv (fn [cplx]
                                (as-> (rewrite-xsd cplx :xsd/complexType) ?t
                                 (if (:sp/type ?t) (assoc ?t :term/type (:sp/type ?t)) ?t)
                                 #_(update ?t :sp/function #(assoc % :fn/componentType cc-type))
                                 (dissoc ?t :sp/type)))
                              complexType))
          simpleType (update :schema/content into (mapv #(rewrite-xsd % :xsd/simpleType) simpleType))
          schemas (assoc :schema/importedSchemas schemas))
        ?r
      (if (-> ?r :schema/content empty?) (dissoc ?r :schema/content) ?r)
      (dissoc ?r :xml/ns-info :xml/content))))

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
    (cond-> {:sp/xsdType :any}
      true (assoc :doc/docString ; ToDo: Investigate later
                  (str (:namespace attrs) "|" (:processContents attrs)))
      (:minOccurs attrs) (assoc :sp/minOccurs (-> attrs :minOccurs keyword))
      (:maxOccurs attrs) (assoc :sp/maxOccurs (-> attrs :maxOccurs keyword)))))

(defparse :xsd/group
  [xmap]
  (-> {:sp/xsdType :any}
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
                   (-> xelem :xml/attrs :name)))) ; ISO/OAGIS, :generic/xsd-file.
  (let [attrs   (:xml/attrs xelem)
        doc     (xpath- xelem :xsd/annotation :xsd/documentation) ; Not commonly used! (Is used in  MF challenge problem.)
        doc?    (when-not (-> doc :xml/content string?) doc)
        comp?   (when doc? (rewrite-xsd doc? :cct-component))
        doc-str (when (-> doc :xml/content string?) (:xml/content doc))
        cplx?   (xpath- xelem :xsd/complexType)] ; Generic schema example, Elena's.
    (cond-> {}
      (:ref  attrs)       (assoc :sp/name (:ref  attrs))
      (:name attrs)       (assoc :sp/name (:name attrs))
      (:type attrs)       (assoc :sp/type (:type attrs))
      (:use  attrs)       (assoc :sp/user (:use  attrs))
      (:minOccurs attrs)  (assoc :sp/minOccurs (-> attrs :minOccurs keyword))
      (:maxOccurs attrs)  (assoc :sp/maxOccurs (-> attrs :maxOccurs keyword))
      doc?                (assoc :sp/function {:fn/type :cct-component}),
      comp?               (assoc :sp/component comp?)
      doc-str             (assoc :sp/docString doc-str)
      cplx?               (assoc :schema/complexTypes
                                  (as-> (rewrite-xsd cplx? :xsd/complexType) ?t
                                    (if (:sp/type ?t)
                                      (assoc ?t :term/type (:sp/type ?t))
                                      ?t)
                                    (dissoc ?t :sp/type)))
      true (assoc :sp/function {:fn/type :gelem}))))

;;; ToDo: better that this would be a set of things unhandled.
;;; Write them as :mm/unhandledXML (a stringified map).
(defparse :xsd/anyAttribute
  [xmap]
  (log/warn "anyAttribute = " xmap)
  {})

(defparse :xsd/annotation
  [xmap]
  (->> (xpath- xmap :xsd/documentation)
       :xml/content
       not-empty))

(defparse :xsd/attribute
  [obj]
  (let [attrs (:xml-attrs obj)
        required? (= "required" (:use attrs))]
    (cond-> {}
      (:name attrs) (assoc :sp/name (:name attrs)) ; ToDo: no name???
      (:type attrs) (assoc :sp/type (:type attrs)) ; Needs work, see :etsi
      required? (assoc :sp/minOccurs 1)
      required? (assoc :sp/maxOccurs 1)
      true (assoc :sp/xsdType :attribute))))

;;; In OAGIS and UBL schema, these are parsed by other means. Not so in QIF, where it is just text.
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
        (assoc ?r :sp/abstract true)
        ?r))))

;;; This just returns the string pointing to a .xsd file. Those will be replaced in postprocessing.
(defparse :xsd/include
  [xmap]
  (-> xmap :xml/attrs :schemaLocation))

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
