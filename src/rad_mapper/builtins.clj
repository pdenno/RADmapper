(ns rad-mapper.builtins
  (:require
   [clojure.string :as str]))

(defmacro defn*
  "Convenience macro for numerical operators. They can be passed functions."
  [fn-name doc-string [& args] & body]
  `(def ~fn-name
     ~doc-string
     (fn [~@args]
       (let [~@(mapcat #(list % `(-> (if (fn? ~%) (~%) ~%) jflatten)) args)]
         ~@(map #(list 'clojure.spec.alpha/assert ::number %) args)
         ~@body))))

(defmacro defn$
  "Define two function arities using the body:
     (1) the ordinary one, that has the usual arguments for the built-in, and,
     (2) a function where the missing argument will be assumed to be the context variable, $.
   The parameter ending in a \\_ is the one elided in (2). (There must be such a parameter.)
   doc-string is required."
  [fn-name doc-string [& params] & body]
  (let [param-map (zipmap params (map #(symbol nil (name %)) params))
        abbrv-params (vec (remove #(str/ends-with? (str %) "_") (vals param-map)))
        abbrv-args (mapv #(if (str/ends-with? (str %) "_") '@$ %) (vals param-map))
        fn-name (symbol nil (name fn-name))]
    (letfn [(rewrite [x]
              (cond (seq? x)    (map  rewrite x)
                    (vector? x) (mapv rewrite x)
                    (map? x)    (reduce-kv (fn [m k v] (assoc m (rewrite k) (rewrite v))) {} x)
                    :else (get param-map x x)))]
    `(defn ~fn-name ~doc-string
       (~abbrv-params (~fn-name ~@abbrv-args))
       ([~@(vals param-map)]
        (let [res# (do ~@(rewrite body))] (if (seq? res#) (doall res#) res#)))))))
