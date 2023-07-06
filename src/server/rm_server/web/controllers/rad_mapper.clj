(ns rm-server.web.controllers.rad-mapper
  (:require
   [clojure.string        :refer [split]]
   [clojure.walk          :as walk :refer [keywordize-keys]]
   [promesa.core          :as p]
   [rad-mapper.builtin    :as bi]
   [ring.util.http-response :as response]
   [rm-server.sutil      :refer [connect-atm]]
   [rm-server.exerciser-saves :as user-saves]
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
      (let [res (bi/processRM :ptag/exp code {:pprint? true :user-data data})]
        (log/info "=== Result of" code "is" res)
        (response/ok {:result res}))
      (catch Exception e
        (log/error e "Error processing RADmapper code. Code = " code)
        (-> (response/found "/") (assoc :flash {:errors {:unknown (.getMessage e)}}))))
    (response/bad-request "No code provided.")))

(defn graph-get
  "Do $get (currently only to data managed by this server).
   Query parameters:
     - ident-type   : a namespaced string such as 'schema/name'.
     - ident-val    : a string, that is the value of a lookup-id.
     - request-objs : a string of elements separated by '|' that will be keywordized to the 'sdb' ns,
                      for example, 'foo|bar' ==> [:sdb/foo :sdb/bar]."
  [request]
  (let [{:keys [ident-type ident-val request-objs]} (-> request :query-params keywordize-keys)
        request-objs (split request-objs #"\|")]
    (log/info "Call to graph-get: ident-type = " ident-type "ident-val =" ident-val "request-objs =" request-objs)
     (if (and ident-type ident-val request-objs)
       (-> (bi/$get [ident-type ident-val] request-objs)
           (dissoc "fn_exe")
           response/ok)
      (response/bad-request "Missing query args."))))

(defn graph-put
  "Do $put (currently only to data managed by this server).
   Query parameters:
     - ident-type   : a namespaced string such as 'schema/name'.
     - ident-val    : a string, that is the value of a lookup-id.
     - request-objs : a string of elements separated by '|' that will be keywordized to the 'sdb' ns,
                      for example, 'foo|bar' ==> [:sdb/foo :sdb/bar]."
  [{{{:keys [put-ident-type put-ident-val put-obj]} :body} :parameters}]
  (log/info "Call to graph-put")
  (if (and put-ident-type put-ident-val put-obj)
    (let [res (bi/$put [put-ident-type put-ident-val] put-obj)]
      (log/info "graph-put: res =" res)
      (response/ok res))
    (response/bad-request "Missing args.")))

(defn llm-match
  "Use an LLM to match maps (bi/$llmMatch) and return result. Request was a POST."
  [{{{:keys [src tar]} :body} :parameters}]
  (log/info "llm-match: src =" src "tar =" tar)
  (if (and src tar)
    (let [p (p/deferred)]
      (future (p/resolve! p (bi/$llmMatch src tar)))
      (let [res (p/await p 45000)]
        (if (nil? res)
          (do
            (log/error "$llmMatch timeouted out. (LLM call)")
            (response/ok {:status :failure :cause "Call to LLM timed out."}))
          (response/ok res))))
    (response/bad-request "src or tar not provided.")))

(defn llm-extract
  "Use an LLM to extract text (bi/$llmExtract) and return result. Request was a GET."
  [request]
  (let [{:keys [source seek]} (-> request :query-params keywordize-keys)]
    (log/info "llm-extract: source =" source "seek =" seek)
    (if (and source seek)
      (let [p (p/deferred)]
        (future (p/resolve! p (bi/$llmExtract source seek)))
        (let [res (p/await p 20000)]
          (if (nil? res)
            (do
              (log/error "$llmExtract timeouted out. (LLM call)")
              (response/ok {:status :failure :cause "Call to LLM timed out."}))
            (response/ok res))))
      (response/bad-request "extract-src or extract-seek not provided."))))

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
               res (bi/query-fn-aux [(connect-atm :schema)] qforms '[$] nil nil nil)]
           (log/info "Datalog query returns: " res)
           (response/ok res))
         (catch Throwable e
           (log/error "Datalog-query:" (.getMessage e))
           (response/bad-request "Bad arguments to datalog query.")))
    (response/bad-request "No arguments applied to datalog query.")))

;;; ToDo: This probably belongs elsewhere; it is an exerciser-only thing
(defn post-code
  "Save the user's code for future reference."
  [request]
  (log/info "body = " (-> request :parameters :body))
  (reset! diag {:request request})
  (try
    (if (-> request :parameters :body :code)
      (if-let [uuid (user-saves/store-code (-> request :parameters :body))]
        (response/ok {:save-id (str uuid)})
        (response/ok {:status 400 :body "Store failed."}))
      (response/ok {:status 400 :body "No code found."}))
    (catch Exception e
      (log/error e "Error in post-code. parameters = " (:parameters request))
      (-> (response/found "/")
          (assoc :flash {:errors {:unknown (.getMessage e)}})))))
