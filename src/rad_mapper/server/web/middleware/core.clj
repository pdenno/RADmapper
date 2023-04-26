(ns rad-mapper.server.web.middleware.core
  (:require
    [rad-mapper.server.env :as env]
    [ring.middleware.defaults :as defaults]
    [ring.middleware.cors :refer [wrap-cors]] ; experimental
    [ring.middleware.session.cookie :as cookie] ))

(defn wrap-base
  [{:keys [metrics site-defaults-config cookie-secret] :as opts}]
  (let [cookie-store (cookie/cookie-store {:key (.getBytes ^String cookie-secret)})]
    (fn [handler]
      (cond-> ((:middleware env/defaults) handler opts)
              true (defaults/wrap-defaults
                    (assoc-in site-defaults-config [:session :store] cookie-store))
              ;; This one to avoid being blocked by CORS policy: No 'Access-Control-Allow-Origin' header is present.
              ;; 1818 is Kaocha test, so ToDo: Using cond-> true here is probably not ideal.
              true (wrap-cors :access-control-allow-origin [#"http://localhost:1818"]
                              :access-control-allow-methods [:get :put :post :delete])
              ))))
