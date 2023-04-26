(ns rad-mapper.server.web.routes.pages
  (:require
    [rad-mapper.server.web.middleware.exception :as exception]
    [rad-mapper.server.web.pages.layout :as layout]
    [integrant.core :as ig]
    [reitit.ring.middleware.muuntaja :as muuntaja]
    [reitit.ring.middleware.parameters :as parameters]
    [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
    [rad-mapper.server.web.routes.utils :as utils]
    [rad-mapper.server.web.controllers.light-server :as rad-mapper]))

(defn wrap-page-defaults []
  (let [error-page (layout/error-page
                     {:status 403
                      :title "Invalid anti-forgery token"})]
    #(wrap-anti-forgery % {:error-response error-page})))

(defn home [{:keys [flash] :as request}]
  (layout/render request "home.html" {:errors (:errors flash)}))

(defn home-preload [{:keys [flash] :as request}]
  (layout/render request "home.html" {:errors (:errors flash)}))

(defn page-routes [_opts]
  [["/" {:get home}]
   ["/example" {:get home-preload}]
   #_["/save-message" {:post light-server/save-message!}]])

(defn route-data [opts]
  (merge
   opts
   {:middleware
    [;; Default middleware for pages
     (wrap-page-defaults)
     ;; query-params & form-params
     parameters/parameters-middleware
     ;; encoding response body
     muuntaja/format-response-middleware
     ;; exception handling
     exception/wrap-exception]}))

(derive :reitit.routes/pages :reitit/routes)

(defmethod ig/init-key :reitit.routes/pages
  [_ {:keys [base-path]
      :or   {base-path ""}
      :as   opts}]
  (layout/init-selmer!)
  [base-path (route-data opts) (page-routes opts)])
