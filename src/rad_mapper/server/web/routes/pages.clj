(ns rad-mapper.server.web.routes.pages
  (:require
    [rad-mapper.server.web.middleware.exception :as exception]
    [integrant.core :as ig]
    [reitit.ring.middleware.muuntaja :as muuntaja]
    [reitit.ring.middleware.parameters :as parameters]
    [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
    [rad-mapper.server.web.routes.utils :as utils]
    [rad-mapper.server.web.controllers.light-server :as rad-mapper]))

#_(defn wrap-page-defaults []
  (let [error-page (layout/error-page
                     {:status 403
                      :title "Invalid anti-forgery token"})]
    #(wrap-anti-forgery % {:error-response error-page})))

(defn route-data [opts]
  (merge
   opts
   {:middleware
    [;; Default middleware for pages
     #_(wrap-page-defaults)
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
  [base-path (route-data opts) #_(page-routes opts)])
