(ns rad-mapper.server.core
  "top-most file for starting the server, sets mount state server and system atom."
  (:require
   [clojure.java.io :as io]
   [mount.core :as mount :refer [defstate]]
   [ring.adapter.undertow :refer [run-undertow]]
   [rad-mapper.server.web.handler :refer [handler-map]]
   [taoensso.timbre :as log])
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
  ;(log/info "In start: handler = " (:handler/ring handler-map))
  ;(log/info "In start: here is what undertow gets: " opts)
  (try
    (let [server (run-undertow handler opts)]
      ;(log/info "server started on port" port)
      ;(log/info "In start: handler = " handler)
      ;(log/info "In start: server = " server)
      server)
    (catch Throwable t
      (log/error t (str "server failed to start on port: " port)))))

;;; Handler of opts =  #function[clojure.lang.AFunction/1]
;;; Handler made from opts handler =  #atom[#delay[{:status :pending, :val nil} 0x769219c3] 0x1630694c]
;;; handler = #function[kit.guestbook.core/eval24494/fn--24495/fn--24498]
;;; server  = #object[io.undertow.Undertow 0x7dbbdbb0 io.undertow.Undertow@7dbbdbb0]
(defn start-server [& {:keys [profile] :or {profile :dev}}]
  (let [base-config (-> "system.edn" io/resource slurp read-string profile)
        port (-> base-config :server/http :port)
        host (-> base-config :server/http :host)]
    (try (let [handler (atom (delay (:handler/ring handler-map)))
               server (start (fn [req] (@@handler req)) {:port port :host host})]
           ;(log/info "Handler of opts (used to make server) = " (:handler/ring handler-map))
           ;(log/info "Handler made from opts handler = " handler)
           ;(log/info "Handler derefed twice = " @@handler)
           ;(log/info "server made from @@handler = " server)
           ;(log/info "In start-server: handler = " handler)
           ;(log/info "In start-server: server = " server)
           (reset! system server)
           (log/info "Started server on port" port)
           {:handler handler
            :server  server})
         (catch Throwable t
           (log/error t "Server failed to start on host " host " port " port ".")))))

(defn -main [& _]
  (start-server))

;;; This is top-most state for starting the server; it happens last.
(defstate server
  :start (start-server)
  :stop (stop-server))
