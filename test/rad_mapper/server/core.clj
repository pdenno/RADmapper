(ns rad-mapper.server.core
  "top-most file for starting the server, sets mount state server and system atom."
   ;; ToDo: There might not be a reason for a server in this test environment; just use the app."
  (:require
   [ajax.core :refer [GET POST]] ; for testing
   [clojure.java.io               :as io]
   [mount.core :as mount          :refer [defstate]]
   [ring.adapter.jetty            :as jetty]
   [rad-mapper.server.web.handler :refer [app]]
   [taoensso.timbre               :as log])
  (:gen-class))

;; log uncaught exceptions in threads
(Thread/setDefaultUncaughtExceptionHandler
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [_ thread ex]
      (log/error {:what :uncaught-exception
                  :exception ex
                  :where (str "Uncaught exception on" (.getName thread))}))))

(defonce system (atom nil))

(defn stop-server [& {:keys [profile] :or {profile :dev}}]
  (.stop @system)
  (reset! system nil)
  ;(log/info "Server has shut down successfully.")
  (when (= profile :prod) (shutdown-agents)))

(defn start [handler {:keys [port] :as opts}]
  (try
    ;; Convert the Ring handler into a running web server.
    (let [server (jetty/run-jetty handler {:port port, :join? false})]
      (POST (str "http://localhost:" port "/api/health")
            {:handler (fn [resp] (log/info "Response through server (POST):" resp))
             :error-handler (fn [{:keys [status status-text]}]
                              (log/error "Server fails response through server: status = " status " status-text = " status-text)
                              (throw (ex-info "Server fails health test." {:status status :status-text status-text})))
             :timeout 1000})
      server)
    (catch Throwable t
      (log/error t (str "server failed to start on port: " port)))))

(defn start-server [& {:keys [profile] :or {profile :dev}}]
  (let [base-config (-> "system.edn" io/resource slurp read-string profile)
        port (-> base-config :server/http :port)
        host (-> base-config :server/http :host)]
    (try (let [handler (atom (delay app))
               server (start (fn [req] (@@handler req)) {:port port :host host})]
           (reset! system server)
           (log/info "Started server on port" port)
           server)
         (catch Throwable t
           (log/error t "Server failed to start on host " host " port " port ".")))))

(defn -main [& _]
  (start-server))

;;; This is top-most state for starting the server; it happens last.
(defstate server
  :start (start-server)
  :stop (stop-server))
