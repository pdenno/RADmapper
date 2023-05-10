(ns rad-mapper.server.web.controllers.rad-mapper
  (:require
   [clojure.data.json     :as json]
   [clojure.string        :refer [split]]
   [clojure.walk          :as walk :refer [keywordize-keys]]
   [rad-mapper.evaluate   :as ev]
   [rad-mapper.builtin    :as bi]
   [rad-mapper.server.web.routes.utils :as utils] ; for mount
   [ring.util.http-response :as response]
   [ring.util.request :as req]
   [taoensso.timbre :as log])
  (:import
    [java.util Date]))

(def diag (atom {}))

;;; http://localhost:3000/process-rm?code=1%2B2
(defn process-rm
  "Run RADmapper processRM, returning the result."
  [{:keys [query-params] :as request}]
  (reset! diag request)
  (try
    (if-let [code (get query-params "code")]
      (let [data (or (get query-params "data") "")
            res (ev/processRM :ptag/exp code {:pprint? true :user-data data})]
        (response/ok {#_#_:status 200
                           #_#_:headers {}
                           :body res}))
      (response/ok {:status 400 ; "bad request"
                         :body "No code found."}))
    (catch Exception e
      (log/error e "Error processing RADmapper code. Code = " (get query-params "code"))
      (-> (response/found "/")
          (assoc :flash {:errors {:unknown (.getMessage e)}})))))

;;; (bi/$get [["schema/name" "urn:oagis-10.8.4:Nouns:Invoice"],  ["schema-object"]])
;;;  = (pathom-resolve {:schema/name "urn:oagis-10.8.4:Nouns:Invoice"} [:sdb/schema-object])
;;; (But we don't care because we can call $get.)
(defn graph-query
  "Make a graph query (currently only to data managed by this server).
   Query parameters:
     - ident-type   : a namespaced string such as 'schema/name'.
     - ident-val    : a string, that is the value of a lookup-id.
     - request-objs : a string of elements separated by '|' that will be keywordized to the 'sdb' ns,
                      for example, 'foo|bar' ==> [:sdb/foo :sdb/bar]."
  [request]
  (log/info "Call to graph-query")
  (let [{:keys [ident-type ident-val request-objs]} (-> request :query-params keywordize-keys)
        request-objs (split request-objs #"\|")]
     (if (and ident-type ident-val request-objs)
      (let [res (bi/$get [[ident-type ident-val] request-objs])]
        (response/ok res))
      (response/ok {:failure "Missing query args."}))))

(def diag (atom nil))

(defn sem-match
  "Do semantic match (bi/$semMatch) and return result. Request was a POST."
  [request]
  (reset! diag (-> request req/body-string))
  (try (let [{:keys [src tar]} (-> request req/body-string json/read-str keywordize-keys) ; <====================
             res (bi/$semMatch src tar)]
         (response/ok res))
       (catch Throwable e
         (log/error "sem-match:" (.getMessage e))
         (response/ok {:failure "sem-match: Args bad or request to LLM failed."}))))
