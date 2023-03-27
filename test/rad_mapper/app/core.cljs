(ns rad-mapper.app.core
  (:require
   [ajax.core               :refer [GET]]
   [helix.core    :as helix :refer [defnc $]]
   [promesa.core  :as p]
   ["react-dom"   :as react-dom]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def svr-prefix "http://localhost:3000")

(defnc app []
  ($ "div" "Now you can run tests in *cljs*."))

(defonce root (react-dom/createRoot (js/document.getElementById "app")))

(defn ^{:after-load true, :dev/after-load true} mount-root []
  (.render root ($ app)))

(defn ^:export init []
  (mount-root))

(def result-atm (atom nil))

(def test-obj
   {:ident-type "schema/name"
    :ident-val "urn:oagis-10.8.4:Nouns:Invoice"
    :request-objs "schema-object"})

(defn tryme [] ; Modern promesa with p/do, The winner!
  (let [p (p/deferred)]
    (p/do
      (log/info "Do this first")
      (p/do
        (log/info "Do this next")
        (GET "http://localhost:3000/api/graph-query" ; ToDo: Need localhost:3000 (exerciser) here?
             {:params test-obj
              :handler (fn [resp]
                         (log/info (str "CLJS-AJAX returns ok."))
                         (reset! result-atm resp)
                         (p/resolve! p resp)) ; This isn't 'returned'!
              :error-handler (fn [{:keys [status status-text]}]
                               (log/info (str "CLJS-AJAX error: status = " status " status-text= " status-text))
                               (reset! result-atm :failure!)
                               (p/reject! p (ex-info "CLJS-AJAX error on /api/graph-query"
                                                     {:status status :status-text status-text})))
              :timeout 3000})
        p ; It waits? here
        (log/info "'***$read(graph-query)' returns " (-> @result-atm str (subs 0 100) (str "...")))
        (log/info "Do this penultimately."))
      (log/info "Do this last"))))
