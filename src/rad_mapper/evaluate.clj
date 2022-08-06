(ns rad-mapper.evaluate
  "Evaluate (someday with SCI) a rewritten form."
  (:require
   [clojure.spec.alpha  :as s]
   [rad-mapper.builtins :as bi]
   [taoensso.timbre     :as log]))

;;; ToDo: It would be nice to say *what symbol* is unresolved. In tracking this down,
;;; of course, I will have to watch for cycles.
(defn user-eval
  "Do clojure eval in namespace app.model.mm-user.
   If the sexp has unresolvable symbols, catch them and return :unresolved-symbol."
  [form & {:keys [verbose? check-asserts?] :or {check-asserts? true}}]
  (when verbose? (println "eval form: " form))
  (s/check-asserts check-asserts?)
  (binding [*ns* (find-ns 'user)]
    (try
      (bi/reset-env)
      (let [res (eval form)]
         (if (and (fn? res) (= :bi/primary (-> res meta :bi/step-type)))
           (bi/jflatten (res))
           (bi/jflatten res)))
      (catch Exception e
        (log/error "\nError evaluating form:" e "\nform:" form)))))
