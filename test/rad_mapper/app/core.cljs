(ns rad-mapper.app.core
  (:require
   #_[rad-mapper.evaluate :as ev]
   #_[applied-science.js-interop :as j]
   [helix.core :as helix :refer [defnc $]]
   ["react-dom/client" :as react-dom]
   #_[taoensso.timbre :as log :refer-macros [info debug log]]))

(def svr-prefix "http://localhost:3000")

(defnc app []
  ($ "div" "Now you can run tests in *cljs*"))

(defonce root (react-dom/createRoot (js/document.getElementById "app")))

(defn ^{:after-load true, :dev/after-load true} mount-root []
  (.render root ($ app)))

(defn ^:export init []
  (mount-root))
