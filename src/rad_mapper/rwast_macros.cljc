(ns rad-mapper.rwast-macros
  "Rewrite the parse tree closer to its interoperable form. rwast = 'rewrite AST'"
  (:require
   [clojure.pprint      :refer [cl-format]]
   [rad-mapper.util     :as util :refer [rwast-dispatch rwast-meth]])) ; ToDo: Why aren't these here?

(def ^:dynamic *debugging?* false)

(def tags   (atom []))
(def locals (atom [{}]))

(defn clear-rwast!
  "Trouble just passing tags and locals to rewrite.cljc!"
  []
  (reset! tags [:toplevel])
  (reset! locals [{}]))

;;; Similar to par/defparse except that it serves no role except to make debugging nicer.
;;; You could eliminate this by global replace of "defrewrite" --> "defmethod rewrite" and removing defn rewrite.
;;; Note: The need for the doall below eluded me for a long time.
;;;       It is necessary when using dynamic binding or want state in an atom to be seen.
(defmacro defrwast [tag [obj & keys-form] & body]
  `(defmethod rwast-meth ~tag [~'tag ~obj ~@(or keys-form '(& _))]
     (when *debugging?* (println (cl-format nil "~A==> ~A" (util/nspaces (count @tags)) ~tag)))
     (swap! tags #(conj % ~tag))
     (swap! locals #(into [{}] %))
     (let [res# (do ~@body)
           result# (if (seq? res#) (doall res#) res#)] ; See note above.
     (swap! tags   #(-> % rest vec))
     (swap! locals #(-> % rest vec))
     (do (when *debugging?*
           (println (cl-format nil "~A<-- ~A returns ~S" (util/nspaces (count @tags)) ~tag result#)))
         result#))))
