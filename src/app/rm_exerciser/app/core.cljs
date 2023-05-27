(ns rm-exerciser.app.core
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [rad-mapper.evaluate :as ev]
   [applied-science.js-interop :as j]
   ["@codemirror/view" :as view]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/ButtonGroup$default" :as ButtonGroup]
   ["@mui/material/colors" :as colors]
   ["@mui/material/CssBaseline$default" :as CssBaseline]
   ["@mui/material/Stack$default" :as Stack]
   ["@mui/material/styles" :as styles]
   ["@mui/material/Typography$default" :as Typography]
   [promesa.core :as p]
   [rm-exerciser.app.components.editor :as editor :refer [Editor set-editor-text get-editor-text SelectExample]]
   [rm-exerciser.app.components.examples :as examples :refer [rm-examples]]
   [rm-exerciser.app.components.share :as share :refer [ShareUpDown ShareLeftRight]]
   [rm-exerciser.app.components.save-modal :refer [SaveModal]]
   [rm-exerciser.app.util :as util]
   [helix.core :as helix :refer [defnc $ <>]]
   [helix.hooks :as hooks]
   ;["react" :as react]
   ["react-dom/client" :as react-dom]
   ["react-router-dom" :as router :refer [useSearchParams]]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def svr-prefix "http://localhost:3000")
(declare get-user-data get-user-code)

(util/config-log :info) ; This was never needed before!

(def diag (atom {}))

(def exerciser-theme
  (styles/createTheme
   (j/lit {:palette {:background {:paper "#F0F0F0"}
                     :primary   colors/blue
                     :secondary colors/grey} ; Secondary here doesn't change icon color. Needs 'main' or 'A400' property.
           :typography {:subtitle1 {:fontSize 5}}

           #_#_:text {:primary "#173A5E"
                  :secondary "#46505A"}

           :components {:MuiCssBaseline {:text {:primary "#173A5E" ; This is being ignored.
                                                :secondary "#46505A"}}
                        #_#_:MuiIconButton
                        {:variants [{:props {:variant "myWhite"}
                                     :style {:color "secondary"}}]}
                        :MuiDivider
                        {:variants [{:props {:variant "activeVert"}  ; vertical divider of horizontal layout
                                     :style {:cursor "ew-resize"
                                             :color "black" ; Invisible on firefox.
                                             :width 4}}
                                    {:props {:variant "activeHoriz"} ; horizontal divider of vertical layout
                                     :style {:cursor "ns-resize"
                                             :color "black"  ; Invisible on firefox.
                                             :height 3}}]} ; For some reason looks better with 3, not 4.
                        :MuiTextField
                        {:variants [{:props {:variant "dataEditor"}
                                     :style {:multiline true}}]}}})))

(defn run-code
  "ev/processRM the source, returning a string that is either the result of processing
   or the error string that processing produced."
  [source]
  (log/info "source = " source)
  (when-some [code (not-empty (str/trim source))]
    (let [user-data (get-user-data)
          _zippy (log/info "******* For RM eval: CODE = \n" code)
          _zippy (log/info "******* For RM eval: DATA = \n" user-data)
          result (try (ev/processRM :ptag/exp code  {:pprint? false :execute? true :sci? true :user-data user-data})
                      (catch js/Error e {:failure (str "Error: " (.-message e))}))]
      result)))

(j/defn eval-cell
  "Run RADmapper on the string retrieved from the editor's state.
   Apply the result to the argument function on-result, which was is the set-result function set up by hooks/use-state."
  [on-result ^:js {:keys [state]}]
  (as-> state ?res
    (.-doc ?res)
    (str ?res)
    (run-code ?res)
    (do (swap! diag #(-> % (assoc :?res ?res) (assoc :promise? (p/promise? ?res)))) ?res)
    (if (p/promise? ?res)
      (-> ?res
          (p/then  #(with-out-str (pprint %)))
          (p/then  on-result)
          (p/catch #(-> % str on-result)))
      (-> ?res str on-result))) ;; on-result is the set-result function from hooks/use-state.
  true)                  ;; This is run for its side-effect.

(defn add-result-action
  "Return the keymap updated with the partial for :on-result, I think!" ;<===
  [{:keys [on-result]}]
  (log/info "sci-eval Extension")
  (.of view/keymap
       (j/lit
        [{:key "Mod-Enter"
          :run (partial eval-cell on-result)}])))

(defn get-props [obj]
  (when (map? (js->clj obj))
    (js->clj (or (j/get obj :props) (get obj "props")))))

(defn get-user-data
  "Return the string content of the data editor."
  []
  (if-let [s (j/get-in (get-in @util/component-refs ["data-editor" :view]) [:state :doc])]
    (.toString s)
    ""))

(defn get-user-code
  "Return the string content of the data editor."
  []
  (if-let [s (j/get-in (get-in @util/component-refs ["code-editor" :view]) [:state :doc])]
    (.toString s)
    ""))

(defn search-props
  "Return the object that has a prop that passes the test."
  [obj test]
  (let [found? (atom nil)
        cnt (atom 0)]
    (letfn [(sp [obj]
              (swap! cnt inc)
              (cond @found?                 @found?,
                    (> @cnt 500)            nil,    ; ToDo: Remove this.
                    (test obj)              (reset! found? obj),
                    (get-props obj)         (doseq [p (-> obj get-props vals)]
                                              (sp p)),
                    (vector? (js->clj obj)) (doseq [p (js->clj obj)] (sp p))))]
      (sp obj)
      @found?)))

(def top-share-fns
  "These, for convenience, keep track of what methods need be called on resizing."
  {:left-share   {:on-stop-drag-lf (partial editor/resize-finish "data-editor")}
   :right-share  {:on-resize-up    (partial editor/resize "code-editor")
                  :on-resize-dn    (partial editor/resize "result")
                  :on-stop-drag-up (partial editor/resize-finish "code-editor")
                  :on-stop-drag-dn (partial editor/resize-finish "result")}})

(defnc Top [{:keys [rm-example width height]}]
  (let [[result set-result] (hooks/use-state "Ctrl-Enter above to execute.")
        banner-height 42
        useful-height (- height banner-height)
        data-editor-height (- useful-height banner-height 20) ; ToDo: 20 (a gap before the editor starts)
        code-editor-height   (int (* useful-height 0.5))    ; <================================== Ignored?
        result-editor-height (int (* useful-height 0.5))]   ; <================================== Ignored?
    (hooks/use-effect [result] (set-editor-text "result" result))
    (hooks/use-effect :once ; Need to set :max-height of resizable editors after everything is created.
      (editor/resize-finish "code-editor" nil code-editor-height)
      (editor/resize-finish "data-editor" nil data-editor-height)
      (editor/resize-finish "result" nil result-editor-height))
    ($ Stack {:direction "column" :height useful-height}
       ($ Typography
          {:variant "h4"
           :color "white"
           :backgroundColor "primary.main"
           :padding "2px 0 2px 20px"
           :noWrap false
           :height banner-height}
          ($ Stack {:direction "row"}
             "RADmapper"
             ($ Box {:minWidth (- width 320)}) ; I'm amazed this sorta works! The 320 depends on the width of "RADmapper".
             ($ ButtonGroup
                ($ SaveModal {:code-fn #(get-user-code)
                              :data-fn #(get-user-data)}))))
       ($ ShareLeftRight
          {:left  ($ Stack {:direction "column" :spacing "10px"}
                     ($ SelectExample {:init-example (:name rm-example)})
                     ($ Editor {:name "data-editor"
                                :height data-editor-height ; Select example is about height of banner.
                                :text (:data rm-example) }))
           :right ($ ShareUpDown
                     {:init-height (- useful-height 20) ; ToDo: Not sure why the 20 is needed.
                      :up ($ Editor {:name "code-editor"
                                     :height code-editor-height
                                     :text (:code rm-example)
                                     :ext-adds #js [(add-result-action {:on-result set-result})]})
                      :dn ($ Editor {:name "result"
                                     :height result-editor-height
                                     :text result})
                      :share-fns (:right-share top-share-fns)})
           :share-fns (:left-share top-share-fns)
           :lf-pct 0.20 #_0.55 ; <=================================
           :init-width width}))))

(defnc app []
  {:helix/features {:check-invalid-hooks-usage true}}
  (let  [[width  set-width]  (hooks/use-state (j/get js/window :innerWidth))
         [height set-height] (hooks/use-state (j/get js/window :innerHeight))
         ;; https://reactrouter.com/en/6.8.2/hooks/use-search-params
         #_#_[search-params set-search-params] (useSearchParams)
         carry-dims-atm (atom {:width width :height height})]
    ;; Uncaught Error: useLocation() may be used only in the context of a <Router> component.
    ;; (js/console.log "search params = " search-params)
    (letfn [(handle-resize [& _args]
              (let [new-width  (j/get js/window :innerWidth)
                    new-height (j/get js/window :innerHeight)]
                (set-width  new-width)
                (set-height new-height)
                (reset! carry-dims-atm {:width new-width :height new-height})))]
      ;; https://www.pluralsight.com/guides/re-render-react-component-on-window-resize
      ;; React gives us a way to do this [cleanup handler] with useEffect. When passing a function to useEffect,
      ;; if that function also returns a function, that returned function will be called to perform any needed cleanup.
      ;; We can put our removeEventListener code there:
      (hooks/use-effect [width height]
        (fn [& _] (js/window.removeEventListener "resize" handle-resize)))
      (js/window.addEventListener "resize" handle-resize)
      (<> ; https://reactjs.org/docs/react-api.html#reactfragment
       ;; https://mui.com/material-ui/react-css-baseline/
       ;; ToDo: See for example https://mui.com/material-ui/customization/typography/ search for MuiCssBaseline
       ;; Use of CssBaseline removes padding/margin around application, if nothing else.
       (CssBaseline {:children #js []}) ; https://v4.mui.com/components/css-baseline/
       ($ styles/ThemeProvider
          {:theme exerciser-theme}
          ($ Top {:width  (:width  @carry-dims-atm)
                  :height (:height @carry-dims-atm)
                  :rm-example (get rm-examples 0)})))))) ; ToDo: Work required here to check whether it is called with an example UUID.

(defonce root (react-dom/createRoot (js/document.getElementById "app")))

(defn ^{:after-load true, :dev/after-load true} mount-root []
  (.render root ($ app)))

(defn ^:export init []
  (mount-root))
