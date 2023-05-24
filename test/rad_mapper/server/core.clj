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
  (if-let [sys @system]
    (.stop sys)
    (log/info "System not stopped: No record of system."))
  (when (= profile :prod) (shutdown-agents)))

(defn test-server [port]
  (try
    ;; Convert the Ring handler into a running web server.
    (GET (str "http://localhost:" port "/api/health")
         {:handler (fn [resp] (log/info "Response through server (GET):" resp))
          :error-handler (fn [{:keys [status status-text]}]
                           (log/error "Server fails response through server: status = " status " status-text = " status-text)
                           (throw (ex-info "Server fails health test." {:status status :status-text status-text})))
          :timeout 1000})
    (catch Throwable t
      (log/error t (str "server failed to start on port: " port)))))

(defn start-server [& {:keys [profile] :or {profile :dev}}]
  (let [base-config (-> "system.edn" io/resource slurp read-string profile)
        port (-> base-config :server/http :port)
        host (-> base-config :server/http :host)]
    (try (let [server (jetty/run-jetty #'rad-mapper.server.web.handler/app {:port port, :join? false})]
           (reset! system server)
           #_(test-server port) ; ToDo: Later!
           (log/info "Started server on port" port))
         (catch Throwable t
           (log/error t "Server failed to start on host " host " port " port ".")))))

(defn -main [& _]
  (start-server))

;;; This is top-most state for starting the server; it happens last.
(defstate server
  :start (start-server)
  :stop (stop-server))
