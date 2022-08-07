(ns rad-mapper.devl.devl-util
  "Tools for repl-based development"
  (:require
   [clojure.pprint :refer [pprint]]))

;;; (require '[rad-mapper.devl.devl-util :refer [nicer]])

(defn clean-form [form]
  (let [ns-alia {"rad-mapper.builtins" "bi"
                 "bi"                  "bi"
                 "java.lang.Math"      "Math"}] ; ToDo: Make it more general. (Maybe "java.lang" since j.l.Exception too.)
    (letfn [(ni [form]
              (cond (vector? form) (->> form (map ni) doall vec)
                    (seq? form)    (->> form (map ni) doall)
                    (map? form)    (reduce-kv (fn [m k v] (assoc m k (ni v))) {} form)
                    (symbol? form) (let [nsa (-> form namespace ns-alia)]
                                     (if-let [[_ s] (re-matches #"([a-zA-Z0-9\-]+)__.*" (name form))]
                                       (symbol nsa s)
                                       (->> form name (symbol nsa))))
                    :else form))]
      (ni form))))

(defn nicer
  "Show macroexpand-1 printed sans package names.
   Argument is a quoted form"
  [form & {:keys [pprint?] :or {pprint? true}}]
        (cond-> (-> form macroexpand-1 clean-form)
          pprint? pprint))

(defn nicer-sym
  "Forms coming back from rew/rewrite* have symbols prefixed by clojure.core
   and other namespaces. On the quoted form in testing, I'd rather not see this.
   This takes away those namespace prefixes."
  [form]
  (clean-form form))
