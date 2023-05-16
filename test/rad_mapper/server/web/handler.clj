(ns rad-mapper.server.web.handler
  (:require
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [mount.core :as mount :refer [defstate]]
   [rad-mapper.server.web.middleware.core :as middleware]
   [rad-mapper.server.web.routes.api   :refer [api-routes]]
   [reitit.ring :as reitit]
   [reitit.swagger-ui :as swagger-ui]
   [taoensso.timbre :as log]))

(defn fix-routes
  "The way routes are provided by api-routes is not the way ring-handler wants them."
  [r]
  (into (-> r butlast vec) (-> r last)))

(defn handler-init [& {:keys [profile] :or {profile :dev}}]
  (let [base-config (-> "system.edn" io/resource slurp read-string profile)
        all-routes [(fix-routes api-routes)]
        handler (reitit/ring-handler
                 (reitit/router all-routes) ; Creates a reitit.core/Router from raw route data, optiona middleware
                 (reitit/routes
                  (reitit/create-resource-handler {:path "/"})
                  (swagger-ui/create-swagger-ui-handler {:path "/api" :url "/api/swagger.json"})
                  (reitit/create-default-handler
                   {:not-found
                    (constantly {:status 404, :body "Page not found"})
                    :method-not-allowed
                    (constantly {:status 405, :body "Not allowed"})
                    :not-acceptable
                    (constantly {:status 406, :body "Not acceptable"})}))
                 {:middleware [(middleware/wrap-base (:handler/ring base-config))]})]
    (->> {:request-method :get :uri "/api/health"}
         handler
         :body
         io/reader
         line-seq
         first
         json/read-str
         (log/info "Handler reports health:"))
    handler))

(defstate app
  "Reitit Ring handler (a self-sufficient 'app' sans listening on port)."
  :start (handler-init))
