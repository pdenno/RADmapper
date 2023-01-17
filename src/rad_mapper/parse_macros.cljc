(ns rad-mapper.parse-macros
  "Parse the JSONata-like message mapping language."
  (:require
   [clojure.pprint  :refer [cl-format]]
   [clojure.spec.alpha :as s]
   [rad-mapper.util :as util]
   [taoensso.timbre :as log]))

(def ^:dynamic *debugging?* false)

(defn parse-dispatch [tag & _] tag)

(defmulti parse #'parse-dispatch)

(defmacro defparse [tag [pstate & keys-form] & body]
  `(defmethod parse ~tag [~'tag ~pstate ~@(or keys-form '(& _))]
     (when *debugging?*
       (println (cl-format nil "~A==> ~A" (util/nspaces (* 3 (-> ~pstate :tags count))) ~tag)))
     (as-> ~pstate ~pstate
       (update ~pstate :call-count inc) ; ToDo: (maybe) There was a max calls check here based on the token count,
       (update ~pstate :tags conj ~tag) ; but with the buffered reading enhancement, we don't have token count.
       (update ~pstate :local #(into [{:locals-for ~tag :collection/items []}] %)) ; Put the newest at the front (a stack).
       (let [res# (do ~@body)] (if (seq? res#) (doall res#) res#))
       (cond-> ~pstate (-> ~pstate :tags not-empty) (update :tags pop))
       (update ~pstate :local #(-> % rest vec))
       (do (when *debugging?*
             (println (cl-format nil "~A<-- ~A   ~S" (util/nspaces (* 3 (-> ~pstate :tags count))) ~tag (:result ~pstate))))
           ~pstate))))
