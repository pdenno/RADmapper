(ns rad-mapper.parse
  "Parse the JSONata-like message mapping language."
  (:require
   [clojure.pprint  :refer [cl-format]]
   [clojure.spec.alpha :as s]
   [rad-mapper.util :as util]
   [taoensso.timbre :as log]))

(def ^:dynamic *debugging?* false)

(defn line-msg
  "Return a string 'Line <n>: ' or 'Line <n>: <msg', depending on args."
  ([pstate] (line-msg pstate ""))
  ([pstate msg & args]
   (if (not-empty args)
     (cl-format nil "Line ~A: ~A ~{~A~^, ~}~% ~A" (-> pstate :tokens first :line) msg args pstate)
     (cl-format nil "Line ~A: ~A~% ~A"            (-> pstate :tokens first :line) msg pstate))))

(def diag (atom nil))

(defn ps-throw
  "A special throw to eliminate to clean up line-seq and "
  [pstate msg data]
  (as-> pstate ?ps
    (dissoc ?ps :line-seq) ; So REPL won't freak out over the reader being closed...
    (reset! diag {:pstate ?ps :msg msg :data data})
    (throw (ex-info (line-msg ?ps msg) data))))

(defn ps-assert
  "A special s/assert that does ps-throw on an error."
  [ps]
  (try (s/assert ::ps ps)
       (catch #?(:clj Exception :cljs :default) e
         (ps-throw ps (str "pstate is invalid: " #?(:clj (.getMessage e) :cljs e))
                   {:tags (:tags ps)
                    :head (:head ps)
                    :tokens (:tokens ps)}))))

(defmacro defparse [tag [pstate & keys-form] & body]
  `(defmethod parse ~tag [~'tag ~pstate ~@(or keys-form '(& _))]
     (when *debugging?*
       (log/info (cl-format nil "~A==> ~A" (util/nspaces (* 3 (-> ~pstate :tags count))) ~tag)))
     (as-> ~pstate ~pstate
       (update ~pstate :call-count inc) ; ToDo: (maybe) There was a max calls check here based on the token count,
       (update ~pstate :tags conj ~tag) ; but with the buffered reading enhancement, we don't have token count.
       (update ~pstate :local #(into [{:locals-for ~tag}] %))
       (let [res# (do ~@body)] (if (seq? res#) (doall res#) res#))
       (cond-> ~pstate (-> ~pstate :tags not-empty) (update :tags pop))
       (update ~pstate :local #(vec (rest %)))
       (do (when *debugging?*
             (log/info (cl-format nil "~A<-- ~A   ~S" (util/nspaces (* 3 (-> ~pstate :tags count))) ~tag (:result ~pstate))))
           (ps-assert ~pstate)))))
