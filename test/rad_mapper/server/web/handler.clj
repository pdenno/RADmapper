(ns rad-mapper.server.web.handler
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [mount.core :as mount :refer [defstate]]
   [muuntaja.core :as m]
   [rad-mapper.server.web.controllers.rad-mapper :as rm]
   [ring.middleware.defaults :as defaults]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.middleware.session.cookie :as cookie]
   [reitit.ring :as ring]
   [reitit.http :as http]
   [reitit.coercion.spec]
   [reitit.swagger :as swagger]
   [reitit.swagger-ui :as swagger-ui]
   [reitit.http.coercion :as coercion]
   [reitit.dev.pretty :as pretty]
   [reitit.interceptor.sieppari :as sieppari]
   [reitit.http.interceptors.parameters :as parameters] ; This one stays!
   [reitit.http.interceptors.muuntaja :as muuntaja]
   [reitit.http.interceptors.exception :as exception]
   [reitit.http.interceptors.multipart :as multipart]
   [reitit.http.interceptors.dev :as dev] ; for testing
   [reitit.http.spec :as spec]
   [spec-tools.core  :as st]
   [spec-tools.spell :as spell]))

;;; ToDo: See rad-mapper.server.web.handler/app, Pretty printing spec errors.

(def example-src
  {"ProcessInvoice"
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

(def example-tar
  {"ProcessInvoice"
   {"DataArea"
    {"Invoice"
     {"InvoiceLine"
      {"Item" {"ManufacturingParty" {"Name" "<data>"}},
       "BuyerParty" {"Location" {"Address" {"AddressLine1" "<data>" "AddressLine2" "<data>" "ZipCode" "<data>"}}, "TaxIDSet" {"ID" "<data>"}}}},
     "Process" "<data>"},
    "ApplicationArea" {"CreationDateTime" "<data>"}}})

(def example-query '[[?e :schema/name ?name]])

(def example-process-rm
  "( $db  := $get([['db/name', 'schemaDB'], ['db/connection']]);
          $qfn := query{[?e :schema/name ?name]};
          $qfn($db) )")

(def example-process-rm-2
"{'a' : {'b' : {'c' : 30, 'f' : 3}}}.a.b.(c + f)")

;;; processRM
(s/def ::code (st/spec {:spec string?
                        :name "code"
                        :description "RADmapper-syntax code to evaluated."
                        :json-schema/default example-process-rm}))
(s/def ::data (st/spec {:spec string?
                        :name "data"
                        :description "Optional RADmapper-syntax data used in evaluation."
                        :json-schema/default ""}))
(s/def ::processRM-request (s/keys :req-un [::code] :opt-un [::data]))
(s/def ::processRM-response string?)

;;; Semantic-match ($semMatch)
(s/def ::src (st/spec {:spec map?
                       :name "src"
                       :description "Source schema (e.g. JSON) for matching."
                       :json-schema/default example-src}))
(s/def ::tar (st/spec {:spec map?
                       :name "tar"
                       :description "Target schema (e.g. JSON) for matching."
                       :json-schema/default example-tar}))
(s/def ::semantic-match-request (s/keys :req-un [::src ::tar]))
(s/def ::semantic-match-response map?)

;;; datalog-query (query)
(s/def ::qforms (st/spec {:spec string? ; In CLJS,
                          :name "qforms"
                          :description "datalog query triples."
                          :json-schema/default (str example-query)}))
(s/def ::datalog-request (s/keys :req-un [::qforms]))
(s/def ::datalog-response vector?) ; A vector of maps (binding sets).

;;; graph-query ($get)
(s/def ::ident-type (st/spec {:spec string?
                              :name "ident-type"
                              :description "The type of the unique identifier from which the query is based."
                              :json-schema/default "schema/name"}))
(s/def ::ident-val (st/spec {:spec string?
                              :name "ident-val"
                              :description "The value of the unique identifier from which the query is based."
                             :json-schema/default "urn:oagis-10.8.4:Nouns:Quote"}))
(s/def ::request-objs (st/spec {:spec string?
                                :name "request-objs"
                                :description "A collection of (vertical bar, e.g. '|')-separated properties sought for the identified object."
                                :json-schema/default "schema/content"}))
(s/def ::graph-query-request (s/keys :req-un [::ident-type ::ident-val ::request-objs]))
(s/def ::graph-query-response map?)

(defn wrap-base
  "Wrap handler for CORS (at least). The CORS concern is for Kaocha testing through port 1818."
  [{:keys [site-defaults-config cookie-secret]}]
  (let [s ^String cookie-secret
        cookie-store (cookie/cookie-store {:key (.getBytes s)})]
    (fn [handler]
      (-> (defaults/wrap-defaults handler
                                  (assoc-in site-defaults-config [:session :store] cookie-store))
          (wrap-cors :access-control-allow-origin [#"http://localhost:1818"]
                     :access-control-allow-methods [:get :put :post :delete])))))

(def routes
   [["/swagger.json"
     {:get {:no-doc true
            :swagger {:info {:title "RADmapper API"
                             :description "API with reitit-http"}}
            :handler (swagger/create-swagger-handler)}}]

    ["/api"
     {:swagger {:tags ["RADmapper functions"]}}

     ["/process-rm"
      {:post {:summary "Run RADmapper code."
              :parameters {:body ::processRM-request}
              :responses {200 {:body {:result ::processRM-response}}}
              :handler rm/process-rm}}]

     ["/sem-match"
      {:post {:summary "Do a semantic match similar to $semMatch()."
              :parameters {:body ::semantic-match-request}
              :responses {200 {:body {:result ::semantic-match-response}}}
              :handler rm/sem-match}}]

     ["/graph-query"
      {:get {:summary "Make a graph query similar to $get()."
             :parameters {:query ::graph-query-request}
             :responses {200 {:body ::graph-query-response}}
             :handler rm/graph-query}}]

     ["/datalog-query"
      {:post {:summary "Run datalog against the schema database."
              :parameters {:body ::datalog-request}
              :responses {200 {:body ::datalog-response}}
              :handler rm/datalog-query}}]

     ["/health"
      {:get {:summary "Check server health"
             :responses {200 {:body {:time string? :up-since string?}}}
             :handler rm/healthcheck}}]]])

(def options
  {;:reitit.interceptor/transform dev/print-context-diffs ;; pretty context diffs
   :validate spec/validate ;; enable spec validation for route data
   :reitit.spec/wrap spell/closed ;; strict top-level validation  (error reported if you don't have the last two interceptors)
   :exception pretty/exception
   :data {:coercion reitit.coercion.spec/coercion
          :muuntaja m/instance
          :interceptors [;; swagger feature
                         swagger/swagger-feature
                         ;; query-params & form-params
                         (parameters/parameters-interceptor)
                         ;; content-negotiation
                         (muuntaja/format-negotiate-interceptor)
                         ;; encodeing response body                ; This one will take :body object (e.g. a map) and return ad java.io.ByteArrayInputStream
                         (muuntaja/format-response-interceptor)    ; Nothing past here reports anything trough print-context-diffs.
                         ;; exception handling
                         (exception/exception-interceptor)
                         ;; decoding request body
                         (muuntaja/format-request-interceptor)
                         ;; coercing response bodies
                         (coercion/coerce-response-interceptor)
                         ;; coercing request parameters
                         (coercion/coerce-request-interceptor)
                         ;; multipart
                         (multipart/multipart-interceptor)]}})

(def default-routes
  "The swagger examples meaningful and therefore good tests.
   However, keep in mind that they don't test calls from CLJS!"
  (ring/routes
   (swagger-ui/create-swagger-ui-handler
    {:path "/"
     :config {:validatorUrl nil
              :operationsSorter "alpha"}})
   (ring/create-default-handler)))

(defn handler-init []
  (let [app (-> (http/ring-handler
                 (http/router routes options)
                 default-routes
                 {:executor sieppari/executor})

                (wrap-cors :access-control-allow-origin [#"http://localhost:1818"]
                           :access-control-allow-methods [:get :put :post :delete]))]
    app))

(defstate app
  "Reitit Ring handler (a self-sufficient 'app' sans listening on port)."
  :start (handler-init))
