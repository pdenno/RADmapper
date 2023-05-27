(ns rm-exerciser.app.util
  (:require
   [applied-science.js-interop :as j]
   ["@codemirror/view" :as view :refer [EditorView]]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def diag (atom nil))

;;; ToDo: Is there a react way? It looks like react doesn't have this notion.
(def root "The application's root 'Symbol(react.element)' element" (atom nil))

#_(defn fn? [arg] ; I didn't see this in the docs!
  (= "Function" (j/get-in arg [:constructor :name])))

(def component-refs
  "Some components instances are named and their refs stored here."
  (atom  {}))

(defn custom-output-fn
  " - I don't want :hostname_ and :timestamp_ in the log output preface text..
    - I don't want any preface text in rad-mapper.parse output."
  ([data] (custom-output-fn nil data))
  ([opts data]
   (if (=  (:?ns-str data) "rad-mapper.parse")
     (apply str (:vargs data)) ; So it can do simple indented call tracing.
     (taoensso.timbre/default-output-fn opts (dissoc data :hostname_ :timestamp_)))))

(defn config-log
  "Configure Timbre: set reporting levels and specify a custom :output-fn."
  [min-level]
  (if (#{:trace :debug :info :warn :error :fatal :report} min-level)
    (log/set-config!
     (-> log/*config*
         (assoc :output-fn #'custom-output-fn)
         (assoc :min-level [[#{"rad-mapper.*" "rm-exerciser.*"} min-level]
                            [#{"datahike.*"} :error]
                            [#{"datascript.*"} :error]
                            [#{"*"} :error]])))
    (log/error "Invalid timbre reporting level:" min-level)))
