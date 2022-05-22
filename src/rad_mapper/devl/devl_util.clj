(ns rad-mapper.devl.devl-util
  "Tools for repl-based development"
  (:require
   [clojure.pprint      :refer [pprint]]
   #_[clojure.set         :as set]
   #_[clojure.spec.alpha  :as s]))

;;; (require '[rad-mapper.devl.devl-util :refer [nicer]])

(defn nicer
  "Show macroexpand-1 printed sans package names.
   Argument is a quoted form"
  [form]
  (let [ns-alia {"rad-mapper.builtins" "bi"
                 "bi"                  "bi"}]
    (letfn [(ni [form]
              (cond (vector? form) (->> form (map ni) doall vec)
                    (seq? form)    (->> form (map ni) doall)
                    (map? form)    (reduce-kv (fn [m k v] (assoc m k (ni v))) {} form)
                    (symbol? form) (if-let [nsa (-> form namespace ns-alia)]
                                     (symbol nsa (name form))
                                     (-> form name symbol))
                    :else form))]
      (-> form macroexpand-1 ni pprint))))

