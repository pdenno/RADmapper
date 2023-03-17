(ns rad-mapper.server
  "A test server for graph-query, at least."
  (:require
   [clojure.java.io]
   [clojure.string        :refer [split]]
   [clojure.tools.logging :as log]
   [clojure.walk          :as walk :refer [keywordize-keys]]
   [luminus-transit.time :as time]
   [muuntaja.core :as m]
   [reitit.ring :as ring]
   [reitit.coercion.malli :as malli]
   [reitit.ring.coercion :as coercion]
   [reitit.ring.middleware.exception :as exception]
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [reitit.ring.middleware.parameters :as parameters]
   [ring.adapter.jetty :as jetty]
   [ring.middleware.anti-forgery :refer [*anti-forgery-token*]]
   [ring.util.anti-forgery :refer [anti-forgery-field]]
   [ring.util.http-response :as http-response :refer [content-type ok]]
   [selmer.parser :as parser]
   [schema-db.resolvers   :refer [pathom-resolve]]))

;;; ========= From middleware/formats =======================
(def instance
  (m/create
    (-> m/default-options
        (update-in
          [:formats "application/transit+json" :decoder-opts]
          (partial merge time/time-deserialization-handlers))
        (update-in
          [:formats "application/transit+json" :encoder-opts]
          (partial merge time/time-serialization-handlers)))))

;;; ========= From middleware/exceptions ===================
(defn handler [message status exception request]
  (when (>= status 500)
    ;; You can optionally use this to report error to an external service
    (log/error exception))
  {:status status
   :body   {:message   message
            :exception (.getClass exception)
            :data      (ex-data exception)
            :uri       (:uri request)}})

(def wrap-exception
  (exception/create-exception-middleware
    (merge
      exception/default-handlers
      {:system.exception/internal     (partial handler "internal exception" 500)
       :system.exception/business     (partial handler "bad request" 400)
       :system.exception/not-found    (partial handler "not found" 404)
       :system.exception/unauthorized (partial handler "unauthorized" 401)
       :system.exception/forbidden    (partial handler "forbidden" 403)

       ;; override the default handler
       ::exception/default            (partial handler "default" 500)

       ;; print stack-traces for all exceptions
       ::exception/wrap               (fn [handler e request]
                                        (handler e request))})))

;;; ================ From web/controllers/rm_exerciser.clj ===========================
(defn graph-query
  "Make a graph query (currently only to data managed by this server).
   Query parameters:
     - ident-type   : a namespaced string such as 'schema/name'.
     - ident-val    : a string, that is the value of a lookup-id.
     - request-objs : a string of elements separated by '|' that will be keywordized to the 'sdb' ns,
                      for example, 'foo|bar' ==> [:sdb/foo :sdb/bar]."
  [request]
  (log/info "Call to graph-query")
  (let [{:keys [ident-type ident-val request-objs] :as req}
        (-> request
            :query-params
            keywordize-keys
            (update :ident-type keyword)
            (update :request-objs #(as-> % ?x
                                     (split ?x #"\|")
                                     (mapv (fn [x] (keyword "sdb" x)) ?x))))] ; ToDo: check for '/' in x.
    (log/info "****/api/graph-query: " req)
    (if (and ident-type ident-val request-objs)
      (let [res (pathom-resolve {ident-type ident-val} request-objs)]
        (http-response/ok {:graph-query-response res}))
      (http-response/ok {:body "Missing query args."}))))

;;; =======From server/web/pages/layout.clj
(def selmer-opts {:custom-resource-path (clojure.java.io/resource "html")})

(defn init-selmer!
  []
  (parser/add-tag! :csrf-field (fn [_ _] (anti-forgery-field))))

(defn render
  [_request template & [params]]
  (-> (parser/render-file template
                          (assoc params :page template :csrf-token *anti-forgery-token*)
                          selmer-opts)
      (ok)
      (content-type "text/html; charset=utf-8")))

(defn error-page
  "error-details should be a map containing the following keys:
   :status - error status
   :title - error title (optional)
   :message - detailed error message (optional)
   returns a response map with the error page as the body
   and the status specified by the status key"
  [error-details]
  {:status  (:status error-details)
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body    (parser/render-file "error.html" error-details selmer-opts)})


(defn home [{:keys [flash] :as request}]
  (render request "home.html" {:errors (:errors flash)}))

(def app
  (ring/ring-handler
    (ring/router
     ["/" {:get home}
      "/api"
       ["/graph-query"
            {:get {:summary "Make a graph query."
                   :parameters {:query {:ident-type string?
                                        :ident-val  string?
                                        :request-objs string?}}
                   :responses {200 {:graph-query-response map?}}
                   :handler graph-query}}]]
      ;; router data affecting all routes
      {:data {:coercion   malli/coercion
              :muuntaja   instance
              :middleware
              [parameters/parameters-middleware
               coercion/coerce-request-middleware
               muuntaja/format-response-middleware
               coercion/coerce-response-middleware]
              #_[;; query-params & form-params
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
               wrap-exception]}})))

(def test-obj
   {:ident-type "schema/name"
    :ident-val "urn:oagis-10.8.4:Nouns:Invoice"
    :request-objs "schema-object"})

#_(app {:request-method :get
      :uri "/api/graph-query"
      :query-params test-obj})

;;; http://localhost:3000/api/graph-query?ident-val=urn%3Aoagis-10.8.4%3ANouns%3AInvoice&ident-type=schema%2Fname&request-objs=schema-object
(defn start []
  (jetty/run-jetty #'app {:port 3000, :join? false})
  (println "server running at port 3000"))
