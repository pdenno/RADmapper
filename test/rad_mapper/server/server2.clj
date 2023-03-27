(ns rad-mapper.server.server2
  "An uncomplicated adaption from the README https://github.com/metosin/reitit
   used to test AJAX calls to bi/$read (if nothing else)."
  (:require
   [clojure.java.io]
   [clojure.string          :refer [split]]
   [clojure.walk  :as walk  :refer [keywordize-keys]]
   [mount.core :as mount]
   [muuntaja.core :as m]
   [reitit.ring :as rr]
   [reitit.coercion.spec]
   [reitit.coercion.malli]
   [reitit.ring.coercion :as rrc]
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [reitit.ring.middleware.parameters :as parameters]
   [ring.middleware.resource]
   [ring.util.http-response :as http-response #_#_:refer [content-type ok]]
   [ring.util.response :as resp]
   [ring.adapter.jetty :as jetty]
   [selmer.parser :as selmer]
   [schema-db.db-util :as du :refer [connect-atm]] ; keep for debugging?
   [schema-db.resolvers :refer [pathom-resolve]]
   [taoensso.timbre :as log])
 (:import
    [java.util Date]))

(def diag (atom nil))

(defn healthcheck!
  [_req]
  (log/info "=============== Doing the health check! ===================")
  (http-response/ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))
     :app      {:status  "up"
                :message ""}}))

;;; ($read [["schema/name" "urn:oagis-10.8.4:Nouns:Invoice"],  ["schema-object"]])
;;;  = (pathom-resolve {:schema/name "urn:oagis-10.8.4:Nouns:Invoice"} [:sdb/schema-object])
(defn graph-query
  "Make a graph query (currently only to data managed by this server).
   Query parameters:
     - ident-type   : a namespaced string such as 'schema/name'.
     - ident-val    : a string, that is the value of a lookup-id.
     - request-objs : a string of elements separated by '|' that will be keywordized to the 'sdb' ns,
                      for example, 'foo|bar' ==> [:sdb/foo :sdb/bar]."
  [request]
  (reset! diag request)
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
        (reset! diag {:res res})
        (http-response/ok {:graph-query-response res}))
      (http-response/ok {:body "Missing query args."}))))

  #_{:status 200,
     :headers {"Content-Type" "text/html"},
     :body
     "<!DOCTYPE html>\n<html>\n<head>\n
        <meta charset=\"UTF-8\" />\n
        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n
        <title>RADmapper Light Server</title>\n</head>\n
     <h2>Hey, we made it!</h2>\n<body>\n
     <div id=\"app\">\n<script src=\"/js/app.js\"></script>\n</body>\n</html>\n"}
(defn home-page
  "This just returns the above."
  [_req]
  (log/info "Real call to home-page!!!")
  (-> (resp/response (selmer/render-file "home.html" {}))
      (resp/content-type "text/html")))

#_["/api"
       ["/health"
        {:get {:handler healthcheck!}}]

       ["/math" {:get {:parameters {:query {:x int?, :y int?}}
                       :responses  {200 {:body {:total int?}}}
                       :handler    (fn [{{{:keys [x y]} :query} :parameters}]
                                     {:status 200
                                      :body   {:total (+ x y)}})}}]
       ["/graph-query"
        {:get {:summary "Make a graph query."
               :parameters {:query {:ident-type string?
                                    :ident-val  string?
                                    :request-objs string?}}
               :responses {200 {:graph-query-response map?}}
               :handler graph-query}}]]

;;;====================================================================

(defn my-dne [& args]
  (log/error "DNE: args = " args))

(defn my-dne2 [& args]
  (log/error "DNE2: args = " args))

(defn my-handler [& args]
  (log/error "HANDLER: args = " args))


(defn wrap-resource [arg]
  (-> (constantly {:status 200, :body "pong"})
      (ring.middleware.resource/wrap-resource arg "public")))


;;; https://clojurians.slack.com/archives/C7YF1SBT3/p1598986669027700
;;; See rm-exerciser.server.web.handler
(def app
  (rr/ring-handler
   (rr/router ; Creates a [[reitit.core/Router]] from raw route data and optionally an options map with support for http-methods and Middleware.
    [["/" {:get home-page}]
     ;["/public/*path" (rr/create-resource-handler {:not-found-handler my-dne})]
     ["/api"
      ["/health"
       {:get {:handler healthcheck!}}]
      ["/graph-query"
       {:get {:summary "Make a graph query."
              :parameters {:query {:ident-type string?
                                   :ident-val  string?
                                   :request-objs string?}}
               :responses {200 {:graph-query-response map?}}
              :handler graph-query}}]]]
    ;; router data affecting all routes
    {:data {:coercion   reitit.coercion.spec/coercion
            :muuntaja   m/instance
            :middleware [parameters/parameters-middleware
                         rrc/coerce-request-middleware
                         muuntaja/format-response-middleware
                         rrc/coerce-response-middleware]}})
   (rr/routes ; Create a ring handler by combining several handlers into one.
    ;; :path is "/" in the exerciser. w/o it, "too many redirects", it is "where to mount handler to.
    (rr/create-resource-handler {:path "/" :not-found-handler my-dne2})
    (rr/create-default-handler
     {:not-found
      (constantly {:status 404, :body "Hey, page not found."})
       :method-not-allowed
      (constantly {:status 405, :body "Hey, not allowed."})
      :not-acceptable
      (constantly {:status 406, :body "Hey, not acceptable."})}))))

(defonce jetty-atm (atom nil))

;;; Before this, do (user/start)
(defn start []
  (mount/start)
  ;; Note that (clojure.java.io/resource "html") returns nil.
  ;(selmer/set-resource-path! "/home/msid/pdenno/Documents/git/RADmapper/resources/html")
  (log/info "Started RADmapper with schema-db connection " @(du/connect-atm))
  (reset! jetty-atm (jetty/run-jetty #'app {:port 3000, :join? false}))
  (println "server running at port 3000"))

(defn stop []
  (.stop @jetty-atm))

(defn reset [] (stop) (start))

;;; http://localhost:3000/api/graph-query?ident-val=urn%3Aoagis-10.8.4%3ANouns%3AInvoice&ident-type=schema%2Fname&request-objs=schema-object
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
:DONE
