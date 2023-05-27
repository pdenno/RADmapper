(ns rm-exerciser.app.components.editor
  (:require
   [clojure.string :as str]
   [rm-exerciser.app.rm-mode.parser :as parser]
   [rm-exerciser.app.rm-mode.state :as state]
   ["@codemirror/language" :refer [foldGutter syntaxHighlighting defaultHighlightStyle]]
   ["@codemirror/commands" :refer [history #_historyKeymap emacsStyleKeymap]]
   ["@codemirror/view" :as view :refer [EditorView  #_lineNumbers]]
   ["@codemirror/state" :refer [EditorState Compartment ChangeSet Transaction]]
   [applied-science.js-interop :as j]
   ;["@mui/system/sizing" :as sizing] ; ToDo: Investigate
   ["@mui/material/FormControl$default" :as FormControl]
   ["@mui/material/MenuItem$default" :as MenuItem]
   ["@mui/material/Select$default" :as Select]
   [rm-exerciser.app.util :as util]
   [rm-exerciser.app.components.examples :as examples :refer [rm-examples get-example]]
   [rm-exerciser.app.components.share :as share]
   [helix.core :as helix :refer [defnc $]]
   [helix.hooks :as hooks]
   [helix.dom :as d]
   ["react" :as react]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def diag (atom nil))

;;; https://codemirror.net/docs/ref/#state.ChangeSpec
;;; Because the selectors will be prefixed with a scope class, rules that directly match the editor's wrapper element -- to which
;;; the scope class will be added -- need to be explicitly differentiated by adding an & to the selector for that
;;; element, for example &.cm-focused.
(defn editor-theme
  "We need to be able to reconfigure the theme because '& {:max-height <height>}' must be the current height
   of the resizable editor. When the height of the text exceeds max-height, scroll bars appear.
   Without this adjustment the text runs outside the editor!"
  [height]
  (.theme EditorView
          (j/lit {".cm-editor"   {:resize   "both"         ; Supposedly these allow it to be resized.
                                  :height   "auto"         ; https://discuss.codemirror.net/t/editor-variable-height/3523
                                  :width    "auto"}        ; overflow "hidden" does nothing. (Was discussed on 3523.)
                  "&"            {:max-height (str height "px")} ; This brings in scroll-bars at the right time...
                  ".cm-scroller" {:overflow "auto"}              ; ... See https://codemirror.net/examples/styling/
                  ".cm-comment"  {:color "#9933CC"}
                  "&.cm-focused" {:outline "0 !important"} ; As above, this matches the editor's wrapper element.
                  ".cm-line"     {:padding "0 9px"
                                  :line-height "1.1"
                                  :font-size "11px"
                                  :font-family "'JetBrains Mono', monospace"}
                  ".cm-matchingBracket" {:border-bottom "1px solid var(--teal-color)"
                                         :color "inherit"}
                  ".cm-gutters"         {:background "lightgray" ;"transparent"
                                         :overflow "auto" ; scroll bars appear as needed.
                                         :border "none"}
                  ".cm-gutterElement"   {:margin-left "5px"}
                  ".cm-cursor"          {:visibility "hidden"} ; only show cursor when focused
                  "&.cm-focused .cm-cursor" {:visibility "visible"}})))

;;; By wrapping part of your configuration in a compartment, you can later replace that part through a transaction.
(defonce style-compartment (new Compartment))

(defn extensions
  [height name]
  #js[(.of style-compartment (editor-theme height))
      (history) ; This means you can undo things!
      (syntaxHighlighting defaultHighlightStyle)
      (view/drawSelection)
      (foldGutter)
      (.. EditorState -allowMultipleSelections (of true))
      ;;parser/default-extensions ; Related to fold gutter, at least. Causes 2023-02-06 "Comma bug"!
      (.of view/keymap parser/complete-keymap)
      (.of view/keymap emacsStyleKeymap #_historyKeymap)])

(defn set-editor-text [editor-name text]
  (when-let [^EditorView view (get-in @util/component-refs [editor-name :view])]
    (let [^EditorState state (j/get view :state)
          ^ChangeSpec  change (j/lit {:from 0 :to (j/get-in state [:doc :length]) :insert text})]
      (.dispatch view (j/lit {:changes change})))))

(defn get-editor-text
  "Return the string content of the data editor."
  [editor-name]
  (if-let [s (j/get-in (get-in @util/component-refs [editor-name :view]) [:state :doc])]
    (.toString s)
    ""))

(defnc SelectExample
  [{:keys [init-example]}]
  (let [[example set-example] (hooks/use-state init-example)]
    ($ FormControl {:size "small" ; small makes a tiny difference
                    :sx {:height "25%"
                         :maxHeight "25%"}}
       ($ Select {:variant "filled"
                  :sx {:style {:height "20px"}}
                  :value example
                  :onChange (fn [_e v]
                              (let [ex-name (j/get-in v [:props :value])
                                    example (get-example ex-name)]
                                (set-example ex-name)
                                (set-editor-text "result" "Ctrl-Enter above to execute.")
                                (set-editor-text "code-editor" (:code example))
                                (set-editor-text "data-editor" (:data example))))}
          (for [ex rm-examples]
            ($ MenuItem {:key (:name ex) :value (:name ex)} (:name ex)))))))

(defn resize
  "Set dimension of the EditorView for share."
  [editor-name parent width height]
  (share/resize parent width height)
  (when-let [view (get-in @util/component-refs [editor-name :view])]
    (when-let [dom (j/get view :dom)]
      (when width  (j/assoc-in! dom    [:style :width]  (str width  "px")))
      (when height
        (j/assoc-in! dom [:style :height] (str height "px"))))))

(defn resize-finish
  "In order for scroll bars to appear (and long text not to run past the end of the editor vieport),
   :max-height must be set. This is done with a transaction."
  [editor-name _elem height]
  (when-let [view (get-in @util/component-refs [editor-name :view])]
    (let [^EditorState state  (j/get view :state)
          ^StateEffect effect (.reconfigure #^Compartment style-compartment (editor-theme height))
          ^Transaction trans  (.update state (j/lit {:effects [effect]}))]
      (.dispatch view trans))))

(defnc Editor
  [{:keys [text ext-adds name height] :or {ext-adds #js []}}]
  (let [ed-ref (hooks/use-ref nil)
        txt (if (string? text) text (or (:success text) (:failure text) ""))
        view-dom (atom nil)]
    (hooks/use-effect [name]
       (when-let [parent (j/get ed-ref :current)]
         (let [editor-state (state/make-state (-> (extensions height name) (.concat ext-adds)) txt)
               view (new EditorView (j/obj :state editor-state :parent parent))]
           (reset! view-dom (j/get view :dom))
           (swap! util/component-refs #(assoc % name {:ref parent :view view})))))
    (d/div {:ref ed-ref :id name} ; style works but looks ugly because it wraps editor tightly.
           @view-dom)))
