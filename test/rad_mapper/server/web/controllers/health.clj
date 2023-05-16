(ns rad-mapper.server.web.controllers.health
  (:require
   [ring.util.http-response :as http-response]
   [taoensso.timbre :as log])
  (:import
    [java.util Date]))

(defn healthcheck!
  [_req]
  (log/info "Doing a health check.")
  (http-response/ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))
     :app      {:status  "up"
                :message ""}}))
