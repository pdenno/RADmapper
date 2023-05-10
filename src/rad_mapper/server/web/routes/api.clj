(ns rad-mapper.server.web.routes.api
  (:require
   [mount.core :as mount :refer [defstate]]
   [rad-mapper.server.web.controllers.health :as health]
   [rad-mapper.server.web.controllers.rad-mapper :as rm]
   [rad-mapper.server.web.middleware.exception :as exception]
   [rad-mapper.server.web.middleware.formats :as formats]
   [reitit.coercion.malli :as malli]
   [reitit.ring.coercion :as coercion]
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [reitit.ring.middleware.parameters :as parameters]
   [reitit.swagger :as swagger]))

;; Routes. See https://cljdoc.org/d/metosin/reitit/0.5.5/doc/ring/swagger-support
(defn api-routes-vec
  "Define API routes (as opposed to page routes defined elsewhere).
   The things are examined by the swagger 2.0 API. Thus if I define a route
   here '/process-rm/:code', it will show up in swagger as /api/process-rm/{code}."
  [_opts]
  [["/swagger.json"
    {:get {:no-doc  true
           :swagger {:info {:title "rad-mapper.server API"}}
           :handler (swagger/create-swagger-handler)}}]
   ["/health"
    {:get {:handler health/healthcheck!}}]
   ["/process-rm"
    {:get {:summary "Run RADmapper code (and optionally data) provided as query parameters."
           :parameters {:query {:code string?
                                :data string?}} ; ToDo: Learn how to express optional query parameters.
           :handler rm/process-rm}}]
   ["/graph-query"
    {:get {:summary "Make a graph query."
           :parameters {:query {:ident-type string?
                                :ident-val  string?
                                :request-objs string?}}
           :responses {200 {:graph-query-response map?}}
           :handler rm/graph-query}}]])

(defn route-data
  [opts]
  (merge
    opts
    {:coercion   malli/coercion
     :muuntaja   formats/instance
     :swagger    {:id ::api}
     :middleware #_[;; query-params & form-params
                  parameters/parameters-middleware
                  ;; encoding response body
                  ;muuntaja/format-response-middleware
                  ;; exception handling
                  exception/wrap-exception]
                 [;; query-params & form-params
                  parameters/parameters-middleware
                  ;; content-negotiation
                  muuntaja/format-negotiate-middleware
                  ;; encoding response body
                  muuntaja/format-response-middleware
                  ;; exception handling
                  coercion/coerce-exceptions-middleware
                  ;; decoding request body
                  muuntaja/format-request-middleware
                  ;; coercing response bodys
                  coercion/coerce-response-middleware
                  ;; coercing request parameters
                  coercion/coerce-request-middleware
                  ;; exception handling
                  exception/wrap-exception]}))

(defn api-routes-init []
  ["/api" (route-data {}) (api-routes-vec {})])

(defstate api-routes
  :start (api-routes-init))