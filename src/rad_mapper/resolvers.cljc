(ns rad-mapper.resolvers
  "This is temporarily part of rad-mapper. This plus schema-db.clj ought to be their own libaray"
  (:require
   [com.wsscode.pathom.connect   :as pc]
   #?(:clj  [datahike.api        :as d]
      :cljs [datascript.core     :as d])
   #?(:clj  [datahike.pull-api   :as dp]
      :cljs [datacript.pull-api  :as dp])
   [rad-mapper.pathom            :as pathom]
   [rad-mapper.schema-db         :as db]
   [taoensso.timbre              :as log]))

(def conn (db/connect-db))
