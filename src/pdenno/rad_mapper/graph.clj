(ns pdenno.rad-mapper.graph
  (:require
   [pdenno.owl-db-tools.core     :as owl]
   [datahike.api          :as d]
   [datahike.pull-api     :as dp]))

(def info-cfg {:store {:backend :mem :id "test"} :keep-history? false :schema-flexibility :write})
(def info-sources {"info"  {:uri "http://www.ontologydesignpatterns.org/ont/dlp/InformationObjects.owl"}})
(def info-user-attrs [#:db{:ident :testing/found? :cardinality :db.cardinality/one :valueType :db.type/boolean}])
(def info-atm nil)

(defn make-info-db [cfg]
  (when (d/database-exists? cfg) (d/delete-database cfg))
  (alter-var-root (var info-atm)
                  (fn [_]
                    (owl/create-db! cfg
                                    info-sources
                                    :user-attrs info-user-attrs
                                    :rebuild? true))))
