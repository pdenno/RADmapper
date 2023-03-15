(ns user
  (:require
   [clojure.tools.namespace.repl :as tools-ns :refer [set-refresh-dirs]]
   [mount.core :as mount]
   ;; this is the top-level dependent component...mount will find the rest via ns requires
   ;; ToDo: Consider using integrant, since that what kit-clj / exerciser use.
   #?(:clj [schema-db.core])
   #?(:clj [schema-db.db-util :as du])
   [rad-mapper.evaluate :refer [processRM]]
   [taoensso.timbre     :as log]))

;;; https://clojure.org/guides/dev_startup_time
;;; (binding [*compile-files* true] (require 'user :reload-all)) ==> No such file or directory.
;;; So far, I have to compile this in CIDER.
(set-refresh-dirs "src/rad-mapper" "src/dev" "test/rad-mapper")

#?(:clj
(defn start
  "Start the server"
  []
  (mount/start)
  (log/info "Started RADmapper with schema-db connection " @(du/connect-atm)))
)   

(defn go [] (start))

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
