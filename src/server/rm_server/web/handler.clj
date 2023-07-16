(ns rm-server.web.handler
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [mount.core :as mount :refer [defstate]]
   [muuntaja.core :as m]
   [rm-server.web.controllers.rad-mapper :as rm]
   [ring.middleware.defaults :as defaults]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.middleware.session.cookie :as cookie]
   [reitit.ring :as ring]
   [ring.middleware.anti-forgery :refer [*anti-forgery-token*]]
   [ring.util.http-response :as response :refer [content-type ok]]
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
   [selmer.parser :as parser] ; kit influence
   [spec-tools.core  :as st]
   [spec-tools.spell :as spell]
   [taoensso.timbre  :as log]))

;;; ToDo: See rm-server.web.handler/app, Pretty printing spec errors.

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

;;; llm-match ($llmMatch)
(s/def ::src (st/spec {:spec map?
                       :name "src"
                       :description "Source schema (e.g. JSON) for matching."
                       :json-schema/default example-src})) ; (Despite the name, give it a clojure map.)
(s/def ::tar (st/spec {:spec map?
                       :name "tar"
                       :description "Target schema (e.g. JSON) for matching."
                       :json-schema/default example-tar}))
(s/def ::llm-match-request (s/keys :req-un [::src ::tar]))
(s/def ::llm-match-response map?)

(s/def ::source (st/spec {:spec string?
                          :name "extract-src"
                          :description "Source string from which to find information."
                          :json-schema/default "Acme Widgets, 100 Main Street, Bldg 123, Chicago, IL, 60610"}))
(s/def ::seek (st/spec {:spec string?
                        :name "extract-seek"
                        :description "Type of information sought."
                        :json-schema/default "building"}))

(s/def ::probability number?)
(s/def ::found string?)

(s/def ::llm-extract-request  (s/keys :req-un [::source ::seek]))
(s/def ::llm-extract-response string? #_(s/keys :req-un [::source ::seek ::probability ::found]))

;;; datalog-query (query)
(s/def ::qforms (st/spec {:spec string? ; In CLJS,
                          :name "qforms"
                          :description "datalog query triples."
                          :json-schema/default (str example-query)}))
(s/def ::datalog-request (s/keys :req-un [::qforms]))
(s/def ::datalog-response vector?) ; A vector of maps (binding sets).

;;; graph-get($get)
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
                                :json-schema/default "schema_content"}))
(s/def ::graph-get-request (s/keys :req-un [::ident-type ::ident-val ::request-objs]))
(s/def ::graph-get-response map?)

;;; graph-put ($put)
(s/def ::put-ident-type (st/spec {:spec string?
                                  :name "ident-type"
                                  :description "The type recognized in some database."
                                  :json-schema/default "library_fn"}))
(s/def ::put-ident-val (st/spec {:spec string?
                                 :name "ident-val"
                                 :description "The uniqueness property of the object being stored."
                                 :json-schema/default "addTwo"}))
(s/def ::put-obj (st/spec {:spec map?
                           :name "object"
                           :description "Some object for which there is schema definitions for all its properties."
                           :json-schema/default {"fn_name" "addTwo", "fn_src" "function($x){$x + 2}" "fn_doc" "Add 2 to arg"}}))
(s/def ::graph-put-request (s/keys :req-un [::put-ident-type ::put-ident-val ::put-obj]))
(s/def ::graph-put-response string?)

;;; =========== Pages (just homepage, thus far.)  =======
(def selmer-opts {:custom-resource-path (io/resource "html")})

(defn render
  [_request template & [params]]
  (-> (parser/render-file template
                          (assoc params :page template :csrf-token *anti-forgery-token*)
                          selmer-opts)
      (ok)
      (content-type "text/html; charset=utf-8")))

(defn home [{:keys [flash] :as request}]
  (log/info "Request for home.html")
  (render request "home.html" {:errors (:errors flash)}))
;;;====================================================

(def routes
  [["/swagger.json"
     {:get {:no-doc true
            :swagger {:info {:title "RADmapper API"
                             :description "API with reitit-http"}}
            :handler (swagger/create-swagger-handler)}}]

   ["/api"
    {:swagger {:tags ["RADmapper API functions"]}}

    ["/process-rm"
     {:post {:summary "Run RADmapper code."
             :parameters {:body ::processRM-request}
             :responses {200 {:body {:result ::processRM-response}}}
             :handler rm/process-rm}}]

    ["/llm-match"
     {:post {:summary "Use an LLM to match keys of two structures; similar to $llmMatch()."
             :parameters {:body ::llm-match-request}
             :responses {200 {:body ::llm-match-response}}
             :handler rm/llm-match}}]

    ["/llm-extract"
     {:get {:summary "Do an LLM text extraction similar to $llmExtract()."
            :parameters {:query ::llm-extract-request}
            :responses {200 {:body ::llm-extract-response}}
            :handler rm/llm-extract}}]

    ["/graph-get"
     {:get {:summary "Make a graph query similar to $get()."
            :parameters {:query ::graph-get-request}
            :responses {200 {:body ::graph-get-response}}
            :handler rm/graph-get}}]

    #_["/fake-get"
     {:get {:summary "Use this in testing calls to $get. It runs a REST call in CLJ. Useful, for example to test pprint-obj/promises."
            :parameters {:query map?}
            :responses {200 {:body map?}}
            :handler (fn [_] (response/ok {"abc" 123}))}}]

    ["/graph-put"
     {:post {:summary "Write to a graph DB similar to $put()."
             :parameters {:body ::graph-put-request}
             :responses {200 {:body ::graph-put-response}}
             :handler rm/graph-put}}]

    ["/datalog-query"
     {:post {:summary "Run datalog against the schema database."
             :parameters {:body ::datalog-request}
             :responses {200 {:body ::datalog-response}}
             :handler rm/datalog-query}}]

    ["/example" ; Can have a get here too!
    {:post {:summary "POST a RADmapper example (code and, optionally, data)."
            :parameters {:body {:code string?, :data string?}}
            :responses {200 {:body {:save-id string?}}}
            :handler rm/post-code}}]

    ["/health"
     {:get {:summary "Check server health"
            :responses {200 {:body {:time string? :up-since string?}}}
            :handler rm/healthcheck}}]]
   ["/app" {:get {:summary "Ignore this swagger entry. I get rid of this soon."
                  :handler home}}]])


(def options
  {;:reitit.interceptor/transform dev/print-context-diffs ;; pretty context diffs  <=========================================== For debugging!
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
                         (muuntaja/format-response-interceptor)    ; Nothing past here reports anything through print-context-diffs.
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
   (ring/create-resource-handler {:path "/"})
   (ring/create-default-handler)))

(defn handler-init []
  (let [site-config (-> "system.edn" io/resource slurp read-string :dev :handler/ring)
        s ^String (:cookie-secret site-config)
        cookie-store (cookie/cookie-store {:key (.getBytes s)})
        app (-> (http/ring-handler
                 (http/router routes options)
                 default-routes
                 {:executor sieppari/executor})

                (defaults/wrap-defaults
                 (assoc-in site-config [:session :store] cookie-store))

                ;; For Kaocha testing through port 1818, at least."
                (wrap-cors :access-control-allow-origin [#"http://localhost:1818"]
                           :access-control-allow-methods [:get :put :post :delete]))]
    app))

(defstate app
  "Reitit Ring handler (a self-sufficient 'app' sans listening on port)."
  :start (handler-init))
