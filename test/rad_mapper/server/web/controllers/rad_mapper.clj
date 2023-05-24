(ns rad-mapper.server.web.controllers.rad-mapper
  (:require
   [clojure.java.io       :as io]
   [clojure.string        :refer [split]]
   [clojure.walk          :as walk :refer [keywordize-keys]]
   [muuntaja.core         :as m]
   [rad-mapper.builtin    :as bi]
   [rad-mapper.evaluate   :as ev]
   [rad-mapper.resolvers  :refer [connect-atm]]
   [ring.util.http-response :as response]
   [taoensso.timbre :as log])
  (:import
   [java.util Date]))

(def diag (atom {}))

(defn healthcheck
  [_request]
  (log/info "Doing a health check.")
  (response/ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))}))

(defn process-rm
  "Run RADmapper processRM, returning the result."
  [{{{:keys [code data]} :body} :parameters}]
  (if code
    (try
      (let [res (ev/processRM :ptag/exp code {:pprint? true :user-data data})]
        (println "=== Result of" code "is" res)
        (response/ok {:result res}))
      (catch Exception e
        (log/error e "Error processing RADmapper code. Code = " code)
        (-> (response/found "/") (assoc :flash {:errors {:unknown (.getMessage e)}}))))
    (response/bad-request "No code provided.")))

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
        (response/ok res #_(->> res (m/encode "application/transit+json") io/reader line-seq first)))
      (response/bad-request "Missing query args."))))

(defn sem-match
  "Do semantic match (bi/$semMatch) and return result. Request was a POST."
  [{{{:keys [src tar]} :body} :parameters}]
  (log/info "sem-match: src =" src "tar =" tar)
  (if (and src tar)
    (try (let [res (bi/$semMatch src tar)]
           (log/info "sem-match result: " res)
           (response/ok res))
         (catch Throwable e
           (log/error "sem-match:" (.getMessage e))
           (response/bad-request "sem-match: Args bad or request to LLM failed.")))
    (response/bad-request "src or tar not provided.")))

;;; (->> '[[?e :schema/name ?name]] (m/encode "application/transit+json") (m/decode "application/transit+json"))
;;; ToDo: Support the 3 options to $query that are nil below.
;;; ToDo: The non-immediate version of $query could also be supported, I think.
(defn datalog-query
  "Run a datalog query against the schema database. Request was a POST.

   Currently this REST datalog query is only used where the client has
   previously used a graph query ($get) to identify the schema DB:
   $db := $get([[db/name 'schemaDB'], ['db/connection']])

   Since $query is a higher-order function, the function produced on a JS client
   checks whether the DB is this $db (the value is just a keyword that will print <<connection>>).
   and creates a REST call to this code using metadata on $query rather than execute
   the main body of the query as is typical where the query is executed on the server."
  [{{{:keys [qforms]} :body} :parameters}]
  (log/info "Datalog query: qforms (string) = " qforms)
  (if (not-empty qforms)
    (try (let [qforms (read-string qforms)
               res (bi/query-fn-aux [(connect-atm)] qforms '[$] nil nil nil)]
           (log/info "Datalog query returns: " res)
           (response/ok res #_(->> res (m/encode "application/transit+json") io/reader line-seq first))) ; CLJS will decode it.
         (catch Throwable e
           (log/error "Datalog-query:" (.getMessage e))
           (response/bad-request "Bad arguments to datalog query.")))
    (response/bad-request "No arguments applied to datalog query.")))
