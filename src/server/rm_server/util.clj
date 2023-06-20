(ns rm-server.util
  "Do lowest level configuration (logging, etc.)."
  (:require
   [mount.core :as mount :refer [defstate]]
   [taoensso.timbre :as log]))

;;; ToDo: The problem with output to log/debug might have to do with *err* not defined in cljs.
(defn custom-output-fn
  " - I don't want :hostname_ and :timestamp_ in the log output preface text..
    - I don't want any preface text in rad-mapper.parse output."
  ([data] (custom-output-fn nil data))
  ([opts data]
   (taoensso.timbre/default-output-fn opts (dissoc data :hostname_ :timestamp_))))

(defn config-log
  "Configure Timbre: set reporting levels and specify a custom :output-fn."
  [min-level]
  (if (#{:trace :debug :info :warn :error :fatal :report} min-level)
    (log/set-config!
     (-> log/*config*
         (assoc :output-fn #'custom-output-fn)
         (assoc :min-level [[#{"user" "exerciser-app.*" "rad-mapper.*" "RM-DEFAULT"} min-level]
                            [#{"datahike.*"} :error]
                            [#{"datascript.*"} :error]
                            [#{"*"} :error]])))
     (log/error "Invalid timbre reporting level:" min-level)))

(defn default-min-log-level
  "Get the value of 'RM-DEFAULT' in (:min-level log/*config*), it designates
   the logging level for namespaces of rad-mapper, including rad-mapper,
   exerciser-app, and user."
  []
  (->> log/*config* :min-level (some #(when (contains? (first %) "RM-DEFAULT") (second %)))))

(defn init-util []
  (config-log :info))

(defstate util-state
  :start (init-util))
