(ns rad-mapper.server.config
  (:require
   [kit.config :as config]
   [taoensso.timbre :as log]))

(def ^:const system-filename "system.edn")

(defn system-config
  [options]
  (log/info "options = " options)
  (config/read-config system-filename options))
