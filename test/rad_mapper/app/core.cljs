(ns rad-mapper.app.core
  (:require
   [helix.core :as helix :refer [defnc $]]
   ["react-dom" :as react-dom]))

(def svr-prefix "http://localhost:3000")

(defnc app []
  ($ "div" "Now you can run tests in *cljs*"))

(defonce root (react-dom/createRoot (js/document.getElementById "app")))

(defn ^{:after-load true, :dev/after-load true} mount-root []
  (.render root ($ app)))

(defn ^:export init []
  (mount-root))
