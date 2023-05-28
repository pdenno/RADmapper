(ns dev.script
  "For running as a script called by node."
  (:require
   ["http" :as http]
   [develop.dutil :as dutil]
   [rad-mapper.evaluate :as ev]
   [rad-mapper.parse-test :as pt]
   [taoensso.timbre :as log]))

(defn request-handler [req res]
  (.end res "foo"))

; a place to hang onto the server so we can stop/start it
(defonce server-ref
  (volatile! nil))

(defn main [& args]
  (js/console.log "starting server")
  (let [server (http/createServer #(request-handler %1 %2))]

    (.listen server 3000
      (fn [err]
        (if err
          (js/console.error "server start failed")
          (js/console.info "http server running"))
        ))

    (vreset! server-ref server)))

(defn start
  "Hook to start. Also used as a hook for hot code reload."
  []
  (js/console.warn "start called")
  (main))

(defn stop
  "Hot code reload hook to shut down resources so hot code reload can work"
  [done]
  (js/console.warn "stop called")
  (when-some [srv @server-ref]
    (.close srv
      (fn [err]
        (js/console.log "stop completed" err)
        (done)))))

(js/console.log "__filename" js/__filename)
