(ns rm-exerciser.app.sci-eval ; Borrowed from nextjournal.clojure-mode.demo.sci
  (:require ["@codemirror/view" :as view]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [rad-mapper.evaluate :as ev]
            [rm-exerciser.app.rm-mode.extensions.eval-region :as eval-region]
            [taoensso.timbre :as log :refer-macros [info debug log]]))

;;; ToDo: This does the parsing again. Should I care?
(defn eval-string
  [source]
  (when-some [code (not-empty (str/trim source))]
    (log/info "eval-string: code = " code)
    (try {:result "***This result***"} ;(ev/processRM :ptag/exp code {:execute? true :sci? true})}
         (catch js/Error e
            {:error (str (.-message e))}))))

#_(j/defn eval-at-cursor [on-result ^:js {:keys [state]}]
  (some->> (eval-region/cursor-node-string state)
           (eval-string)
           (on-result))
  true)

;;; ToDo: This is among things that will probably go away (or I switch to Clerk!)
(j/defn eval-top-level [on-result ^:js {:keys [state]}]
  (some->> (eval-region/top-level-string state)
           (eval-string)
           (on-result))
  true)

(j/defn eval-cell [on-result ^:js {:keys [state]}]
  (-> (.-doc state)
      (str)
      (eval-string)
      (on-result))
  true)

(defn keymap* [modifier]
  {:eval-cell
   [{:key "Mod-Enter"
     :doc "Evaluate cell"}]
   :eval-at-cursor
   [{:key (str modifier "-Enter")
     :doc "Evaluates form at cursor"}]
   :eval-top-level
   [{:key (str modifier "-Shift-Enter")
     :doc "Evaluates top-level form at cursor"}]})

(defn extension [{:keys [modifier
                         on-result]}]
  (log/info "sci-eval Extension")
  (.of view/keymap
       (j/lit
        [{:key "Mod-Enter"
          :run (partial eval-cell on-result)}
         #_{:key (str modifier "-Enter")
          :shift (partial eval-top-level on-result)     ; ToDo: I'm using modifier='Alt'
          :run (partial eval-at-cursor on-result)}])))
