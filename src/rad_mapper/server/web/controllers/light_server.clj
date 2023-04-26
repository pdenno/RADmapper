(ns rad-mapper.server.web.controllers.light-server
  (:require
   [clojure.string        :refer [split]]
   [clojure.tools.logging :as log]
   [clojure.walk          :as walk :refer [keywordize-keys]]
   [rad-mapper.evaluate   :as ev]
   [schema-db.resolvers   :refer [pathom-resolve]]
   [ring.util.http-response :as http-response]))

(def diag (atom nil))

;;; http://localhost:3000/process-rm?code=1%2B2
(defn process-rm
  "Run RADmapper processRM, returning the result."
  [{:keys [query-params] :as request}]
  (reset! diag request)
  (try
    (if-let [code (get query-params "code")]
      (let [data (or (get query-params "data") "")
            res (ev/processRM :ptag/exp code {:pprint? true :user-data data})]
        (http-response/ok {#_#_:status 200
                           #_#_:headers {}
                           :body res}))
      (http-response/ok {:status 400 ; "bad request"
                         :body "No code found."}))
    (catch Exception e

      (log/error e "Error processing RADmapper code. Code = " (get query-params "code"))
      (-> (http-response/found "/")
          (assoc :flash {:errors {:unknown (.getMessage e)}})))))

;;; (bi/$get [["schema/name" "urn:oagis-10.8.4:Nouns:Invoice"],  ["schema/content"]])
;;;  = (schema-db.resolvers/pathom-resolve {:schema/name "urn:oagis-10.8.4:Nouns:Invoice"} [:schema/content])
;;; http://localhost:3000/api/graph-query?ident-type=schema%2Fname&ident-val=urn%3Aoagis-10.8.4%3ANouns%3AInvoice&request-objs=schema%2Fcontent
(defn graph-query
  "Make a graph query (currently only to data managed by this server).
   Query parameters:
     - ident-type   : a namespaced string such as 'schema/name'.
     - ident-val    : a string, that is the value of a lookup-id.
     - request-objs : a string of ns-qualified symbols."
  [request]
  (log/info "Call to graph-query")
  (reset! diag {:request request})
  (let [{:keys [ident-type ident-val request-objs] :as req} ; request-objs "a|string|like|this" (4 request-objs in that one).
        (-> request
            :query-params
            keywordize-keys
            (update :ident-type keyword))]
    (log/info "****/api/graph-query: " req)
    (if (and ident-type ident-val request-objs)
      (let [request-objs (->> (split request-objs #"\|") (mapv keyword))
            zippy (swap! diag #(merge % {:ident-type ident-type :ident-val ident-val :request-objs request-objs}))
            res (pathom-resolve {ident-type ident-val} request-objs)] ; ToDo: Looks like there is just one!
        (swap! diag #(merge % {:res res}))
        (http-response/ok {:graph-query-response res}))
      (http-response/ok {:body "Missing query args."}))))
