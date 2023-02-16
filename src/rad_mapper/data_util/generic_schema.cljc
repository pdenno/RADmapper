(ns rad-mapper.data-util.generic-xml
  "Functions to read XML to structures that the DB can use."
  (:require
;;   [clojure.java.io              :as io]
;;   [clojure.pprint               :refer [cl-format]]
;;   [clojure.spec.alpha           :as s]
   [clojure.string               :as str]
;; [mount.core                   :refer [defstate]]
   [rad-mapper.data-util.macros      :as mac   :refer [defparse rewrite-xsd *skip-doc-processing?*]]
   [rad-mapper.data-util.db-util     :as du :refer [xpath #_xpath- xml-type?]]
   [rad-mapper.data-util.schema-util :as su :refer [schema-ns schema-sdo schema-spec schema-version schema-subversion schema-type schema-name
                                                    q-schema-topic q-schema-sdo q-schema-type special-schema-type? generic-schema-type?]]
   [rad-mapper.util                  :as util]
   [taoensso.timbre                  :as log]))

(def diag (atom nil))

(def simple-xsd?
  {:xsd/minLength      :number
   :xsd/maxLength      :number
   :xsd/pattern        :string
   :xsd/fractionDigits :number
   :xsd/totalDigits    :number
   :xsd/minInclusive   :number})

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

;;; (read-schema-file "data/testing/elena/Company A - Invoice.xsd")
(defn read-xml-file
  "Create map structure for the DB for the given file."
  [path]
  (-> path read-clean rewrite-xsd du/condition-form vector))


;;;===============  File Level =========================================================
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
                                 #_(update ?t :sp/function #(assoc % :fn/componentType cc-type))
                                 (dissoc ?t :sp/type)))
                              comps))
          schemas (assoc :schema/importedSchemas schemas))
        ?r
      (if (-> ?r :library/content empty?) (dissoc ?r :library/content) ?r)
      (dissoc ?r :xml/ns-info :xml/content))))
