(ns rad-mapper.server.web.controllers.rad-mapper
  (:require
   [clojure.data.json     :as json]
   [clojure.string        :refer [split]]
   [clojure.walk          :as walk :refer [keywordize-keys]]
   [datahike.api          :as d]
   [rad-mapper.builtin    :as bi]
   [rad-mapper.evaluate   :as ev]
   [rad-mapper.resolvers  :refer [connect-atm]]
   [rad-mapper.server.web.routes.util :as util] ; for mount
   [ring.util.http-response :as response]
   [ring.util.request :as req]
   [taoensso.timbre :as log])
  (:import
   [java.util Date]))

(def diag (atom {}))

#_(->>  {:request-method :post :uri "/api/process-rm" :body {:code "1+2"}}
        rad-mapper.server.web.handler/app
        :body
        clojure.java.io/reader
        line-seq
        (map clojure.data.json/read-str))
(defn process-rm
  "Run RADmapper processRM, returning the result."
  [request]
  (let [{:keys [code data]} (:body request)]
    (if code
      (try
        (response/ok {:body (ev/processRM :ptag/exp code {:pprint? true :user-data data})})
        (catch Exception e
          (log/error e "Error processing RADmapper code. Code = " code)
          (-> (response/found "/") (assoc :flash {:errors {:unknown (.getMessage e)}}))))
      (response/ok {:status 400 :body "No code provided."}))))

#_(->>  {:request-method :get :uri "/api/graph-query"
         :query-params  {:ident-type "schema/name"
                         :ident-val "urn:oagis-10.8.4:Nouns:Invoice"
                         :request-objs "schema/content"}}
        rad-mapper.server.web.handler/app
        :body
        clojure.java.io/reader
        line-seq
        (map clojure.data.json/read-str))
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

;;; Check these, I might have screwed them up.
(def src {"ProcessInvoice"
          {"DataArea"
           {"Invoice"
            {"InvoiceLine"
             {"Item" {"ManufacturingParty" {"Name" "<data>"}},
              "BuyerParty"
              {"Location"
               {"Address"
                {"BuildingNumber" "<data>"
                 "PostalCode" "<data>",
                 "Street" "<data>",
                 "City" "<data>",
                 "State" "<data>",
                 "Country" "<data>"}}
               "TaxIDSet" {"ID" "<data>"}}}},
           "Process" "<data>"},
           "ApplicationArea" {"CreationDateTime" "<data>"}}})

(def tar {"ProcessInvoice"
          {"DataArea"
           {"Invoice"
            {"InvoiceLine"
             {"Item" {"ManufacturingParty" {"Name" "<data>"}},
              "BuyerParty" {"Location" {"Address" {"AddressLine1" "<data>" "AddressLine2" "<data>" "ZipCode" "<data>"}}, "TaxIDSet" {"ID" "<data>"}}}},
            "Process" "<data>"},
           "ApplicationArea" {"CreationDateTime" "<data>"}}})

;;; ToDo: Why does this one return a string and the other a stream?
#_(->>  {:request-method :post :uri "/api/sem-match"
         :body {:src rad-mapper.server.web.controllers.rad-mapper/src
                :tar rad-mapper.server.web.controllers.rad-mapper/tar}}
        rad-mapper.server.web.handler/app
        :body)
(defn sem-match
  "Do semantic match (bi/$semMatch) and return result. Request was a POST."
  [request]
  (let [{:keys [src tar]} (:body request)]
    (if (and src tar)
      (try (response/ok (bi/$semMatch src tar))
           (catch Throwable e
             (log/error "sem-match:" (.getMessage e))
             (response/ok {:failure "sem-match: Args bad or request to LLM failed."})))
      (response/ok {:status 400 :body "src or tar not provided."}))))

#_(->>  {:request-method :post :uri "/api/datalog-query"
         :body {:qforms '[[?e :schema/name ?name]]}}
        rad-mapper.server.web.handler/app
        :body
        clojure.java.io/reader
        line-seq
        (map clojure.data.json/read-str))
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
  [request]
  (reset! diag request)
  (let [{:keys [qforms]} (:body request)]
    (log/info "Datalog query: qforms = " qforms)
    (if (not-empty qforms)
      (try (let [res (bi/query-fn-aux [(connect-atm)] qforms '[$] nil nil nil)]
             (log/info "datalog query returns: " res)
             (response/ok res))
           (catch Throwable e
             (log/error "datalog-query:" (.getMessage e))
             (response/ok {:failure "Bad arguments to datalog query."})))
      (response/ok {:status 400 :body "Now arguments applied to datalog query."}))))
