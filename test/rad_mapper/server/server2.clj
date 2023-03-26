(ns rad-mapper.server.server2
  "An uncomplicated adaption from the README https://github.com/metosin/reitit
   used to test AJAX calls to bi/$read (if nothing else)."
  (:require
   [muuntaja.core :as m]
   [reitit.ring :as ring]
   [reitit.coercion.spec]
   [reitit.ring.coercion :as rrc]
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [reitit.ring.middleware.parameters :as parameters]
   [ring.adapter.jetty :as jetty]))

(def app
  (ring/ring-handler
    (ring/router
      ["/api"
       ["/math" {:get {:parameters {:query {:x int?, :y int?}}
                       :responses  {200 {:body {:total int?}}}
                       :handler    (fn [{{{:keys [x y]} :query} :parameters}]
                                     {:status 200
                                      :body   {:total (+ x y)}})}}]]
      ;; router data affecting all routes
      {:data {:coercion   reitit.coercion.spec/coercion
              :muuntaja   m/instance
              :middleware [parameters/parameters-middleware
                           rrc/coerce-request-middleware
                           muuntaja/format-response-middleware
                           rrc/coerce-response-middleware]}})))

(defonce jetty-atm (atom nil))

;;; http://localhost:3000/api/graph-query?ident-val=urn%3Aoagis-10.8.4%3ANouns%3AInvoice&ident-type=schema%2Fname&request-objs=schema-object
(defn start []
  (reset! jetty-atm (jetty/run-jetty #'app {:port 3000, :join? false}))
  (println "server running at port 3000"))

(defn stop []
  (.stop @jetty-atm))

(defn reset [] (stop) (start))

;;; http://localhost:3000/api/math?x=7&y=8
(comment
  (app {:request-method :get
        :uri "/api/math"
        :query-params {:x "1", :y "2"}}))

;;; That's not what it shows on the README. It shows {:status 200 :body {:total 3}}
;;; What I get from the above needs a bit of more processing, since it returns:
;;;{:status 200,
;;;   :body #object[java.io.ByteArrayInputStream 0x5a9ef42b "java.io.ByteArrayInputStream@5a9ef42b"],
;;;   :headers {"Content-Type" "application/json; charset=utf-8"}}
;;;
;;; However, on the browser it shows: {"total": 15} (for the URL above).
