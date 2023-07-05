(ns exerciser-app.components.save-modal
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [ajax.core :refer [GET POST]]
   [promesa.core :as p]
   [applied-science.js-interop :as j]
   [clojure.pprint             :refer [cl-format]]
   [helix.core :refer [defnc $]]
   [helix.hooks :as hooks]
   [exerciser-app.util :refer [component-refs]]
   [exerciser-app.components.editor :refer [set-editor-text get-editor-text]]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/IconButton$default" :as IconButton]
   ["@mui/icons-material/Save$default" :as Save]
   ["@mui/material/Typography$default" :as Typography]
   ["@mui/material/Modal$default" :as Modal]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def style (clj->js
            {:position "absolute", ; as 'absolute'
             :top "50%",
             :left "50%",
             :transform "translate(-50%, -50%)",
             :width 650,
             :bgcolor "background.paper",
             :border "2px solid #000",
             :boxShadow 24,
             :p 2}))

(def white-style (clj->js {:color "background.paper"}))
(def svr-prefix "http://localhost:3000/app/")
(def diag (atom nil))

;;; https://mui.com/material-ui/react-modal/
(defnc SaveModal [{:keys [code-fn data-fn]}]
  (let [[open, set-open] (hooks/use-state false)
        [url set-url]    (hooks/use-state nil)
        [text set-text]  (hooks/use-state "")
        modal            (hooks/use-ref nil)]
    (letfn [(save-success [{:keys [save-id]}]
              (when (j/get modal :current)
                (js/console.log "User code saved at:"
                (set-url (str svr-prefix "example/" save-id))
                (set-text "To recover the work shown visit:")
                (set-open true))))
            (save-failure [status status-text]
              (when (j/get modal :current)
                (log/info "Saving example failed: status = " status " status text = " status-text)
                (set-text "Communication with the server failed.")
                (set-open true)))
            (handle-save []
              (POST "/api/example"
                    {:params {:code (code-fn) :data (data-fn)}
                     :timeout 3000
                     :handler       save-success
                     :error-handler save-failure}))
            (handle-close [] (set-open false))]
      ($ "div" {:ref modal}
         ($ IconButton {:onClick handle-save} ($ Save {:sx white-style}))
         ($ Modal {:open open
                   :onClose handle-close
                   :aria-labelledby "save-modal-title"
                   :aria-describedby "save-modal-description"}
            ($ Box {:sx style}
               ($ Typography {:id "save-modal-title" :variant "h6" :component "h6"} text)
               ($ Typography {:id "save-modal-description" :sx {:mt 20}} url)))))))
