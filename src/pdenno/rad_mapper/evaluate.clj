(ns pdenno.rad-mapper.evaluate
  "Evaluate (someday with SCI) a rewritten form."
  (:require
   [taoensso.timbre   :as log]))


;;; ToDo: SCI (Small Clojure Interpreter)
;;; ToDo: It would be nice to say *what symbol* is unresolved. In tracking this down,
;;; of course, I will have to watch for cycles.
;;; (user-eval '(+ 1 2))
(defn user-eval
  "Do clojure eval in namespace app.model.mm-user.
   If the sexp has unresolvable symbols, catch them and return :unresolved-symbol."
  [fn-form & {:keys [verbose?]}]
  (when verbose? (println "eval form: " fn-form))
  (binding [*ns* (find-ns 'user)]
    (try
      ((eval fn-form))
      (catch Exception e
        (log/error "\nError evaluating form:" e "\nform:" fn-form)))))

