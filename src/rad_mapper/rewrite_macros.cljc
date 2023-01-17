(ns rad-mapper.rewrite-macros
  "Rewrite the parse tree as Clojure, a simple task except for precedence in binary operators.
   processRM is a top-level function for this."
  (:require
   [clojure.pprint      :refer [cl-format]]
   [rad-mapper.util     :as util :refer [rewrite-dispatch rewrite-meth]]
   #_[rad-mapper.parse    :as par :refer [builtin-fns]])) ; Needed because the it might be used in the body of defrewrite??? DID NOT HELP.

(def ^:dynamic *debugging?* false)

(def tags (atom []))
(def locals (atom [{}]))

;;; Similar to par/defparse except that it serves no role except to make debugging nicer.
;;; You could eliminate this by global replace of "defrewrite" --> "defmethod rewrite" and removing defn rewrite.
;;; Note: The need for the doall below eluded me for a long time.
;;;       It is necessary when using dynamic binding or want state in an atom to be seen.
(defmacro defrewrite [tag [obj & keys-form] & body]
  `(defmethod rewrite-meth ~tag [~'tag ~obj ~@(or keys-form '(& _))]
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
