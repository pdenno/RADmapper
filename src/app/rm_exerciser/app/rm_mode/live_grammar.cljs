#_(ns rm-exerciser.app.rm-mode.live-grammar
  (:require ["@lezer/generator" :as lg]
            [shadow.resource :as rc]
            [rm-exerciser.app.rm-mode.node :as n]))

;;for dev, it's useful to build the parser in the browser
#_(def parser
  (lg/buildParser
   (rc/inline "./clojure.grammar")
   #js{:externalProp n/node-prop}))

(comment
  (.parse parser "(def foo)")
  (.parse parser "(ns foo)")
  (.parse parser "(foo bar)")
  )
