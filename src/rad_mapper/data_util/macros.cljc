(ns rad-mapper.data-util.macros
  (:require
   [rad-mapper.data-util.schema-util :as su]
   [taoensso.timbre                  :as log]))

(def debugging? (atom false))
(def diag (atom false))

(defmulti rewrite-xsd #'su/rewrite-xsd-dispatch)

(defmethod rewrite-xsd nil [obj & schema]
  (if schema
    (log/warn "No method for obj/schema.")
    (log/warn "No method for obj."))
  (reset! diag {:obj obj :schema schema})
  :mm/rewrite-xsd-nil-method)

;;; Establishes rewrite-xsd methods.
(defmacro defparse [tag [arg props] & body]
  `(defmethod rewrite-xsd ~tag [~arg & ~'_]
     ;; Once *skip-doc-processing?* is true, it stays so through the dynamic scope of the where it was set.
     (when @debugging? (log/info "defparse tag = " ~tag))
     (binding [su/*skip-doc-processing?* (or su/*skip-doc-processing?* (:skip-doc-processing? ~props))]
       (let [[~arg doc-string#] (su/obj-doc-string ~arg)
             result# (do ~@body)]
         (if (and doc-string# (map? result#))
           (assoc result# :doc/docString doc-string#)
           result#)))))
