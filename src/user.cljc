(ns user
  (:require
    [clojure.tools.namespace.repl :as tools-ns :refer [set-refresh-dirs]]
    [mount.core :as mount]
    ;; this is the top-level dependent component...mount will find the rest via ns requires
    [rad-mapper.pathom :refer [parser]]))

(set-refresh-dirs "src/rad-mapper" "src/dev" "test/rad-mapper") ; Will lose this file if you start at "src" ???

(defn start
  "Start the server"
  [] (mount/start))

(defn stop
  "Stop the server"
  [] (mount/stop))

(defn unstuck []
  (tools-ns/refresh))

(defn restart
  "Stop, reload code, and restart the server. If there is a compile error,
   use: unstuck to recompile, and then use `start` once things are good."
  []
  (stop)
  (tools-ns/refresh :after 'user/start))

