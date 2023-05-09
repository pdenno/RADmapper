(ns rad-mapper.server.web.handler
  (:require
   [clojure.java.io :as io]
   [mount.core :as mount :refer [defstate]]
   [rad-mapper.server.web.middleware.core :as middleware]
   [rad-mapper.server.web.routes.api   :refer [api-routes]]
   [reitit.ring :as ring]
   [reitit.swagger-ui :as swagger-ui]))

(defn fix-routes
  "The way routes are provided by page-routes and api-routes is not the way ring-handler wants them."
  [r]
  (into (-> r butlast vec) (-> r last)))

(defn handler-map-init [& {:keys [profile] :or {profile :dev}}]
  (let [base-config (-> "system.edn" io/resource slurp read-string profile)
        all-routes [(fix-routes api-routes)]]
    {:handler/ring (ring/ring-handler
                    (ring/router all-routes)
                    (ring/routes
                     (ring/create-resource-handler {:path "/"})
                     (swagger-ui/create-swagger-ui-handler {:path "/api" :url "/api/swagger.json"}) ; ToDo: make base-config in edn so you can get these.
                     (ring/create-default-handler
                      {:not-found
                       (constantly {:status 404, :body "Page not found"})
                       :method-not-allowed
                       (constantly {:status 405, :body "Not allowed"})
                       :not-acceptable
                       (constantly {:status 406, :body "Not acceptable"})}))
                    {:middleware [(middleware/wrap-base (:handler/ring base-config))]})
     :router/routes all-routes
     ;; This need not be stored; it isn't used; but might be useful later.
     :router/core (ring/router all-routes)}))

(defstate handler-map
  :start (handler-map-init))
