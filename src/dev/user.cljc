(ns user
  (:require
    [clojure.tools.namespace.repl :as tools-ns :refer [set-refresh-dirs]]
    [mount.core :as mount]
    ;; this is the top-level dependent component...mount will find the rest via ns requires
    [rad-mapper.pathom :refer [parser]]))

;; ==================== SERVER ====================
(set-refresh-dirs "src" "src/dev" "src/test")
;; Change the default output of spec to be more readable

(defn start
  "Start the web server"
  [] (mount/start))

(defn stop
  "Stop the web server"
  [] (mount/stop))

(defn restart
  "Stop, reload code, and restart the server. If there is a compile error, use:

  ```
  (tools-ns/refresh)
  ```

  to recompile, and then use `start` once things are good."
  []
  (stop)
  (tools-ns/refresh :after 'user/start))

