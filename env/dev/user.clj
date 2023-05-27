(ns user
  "For REPL-based start/stop of the server.
   This file isn't used in cljs and is a problem for shadow-cljs without the
   :clj compiler directives."
  (:require
    [clojure.pprint]
    [clojure.spec.alpha :as s]
    [clojure.tools.namespace.repl :as tools-ns :refer [disable-reload! refresh clear set-refresh-dirs]]
    [expound.alpha :as expound]
    [mount.core :as mount]
    [rad-mapper.evaluate]
    [lambdaisland.classpath.watch-deps :as watch-deps]      ;; hot loading for deps
    [rad-mapper.resolvers   :refer [schema-db-atm]] ; for mount
    [rm-server.core :refer [server]] ; for mount
    [rm-server.web.handler]          ; for mount, defines rm.server.config/config, and router stuff.
    [taoensso.timbre :as log]))

;;; If you get stuck do: (clojure.tools.namespace.repl/refresh)

;; uncomment to enable hot loading for deps
(watch-deps/start! {:aliases [:dev :test]})

(alter-var-root #'s/*explain-out* (constantly expound/printer))
(add-tap (bound-fn* clojure.pprint/pprint))
(set-refresh-dirs "test/rad_mapper/server")  ; put as many as you need here

(defn start
  "Start the web server"
  []
  (mount/start))

(defn stop
  "Stop the web server"
  []
  (mount/stop))

(defn restart
  "Stop, reload code, and restart the server. If there is a compile error, use:

  ```
  (tools-ns/refresh)
  ```

  to recompile, and then use `start` once things are good."
  []
  (stop)
  (tools-ns/refresh :after 'user/start))
