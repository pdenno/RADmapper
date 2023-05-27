(ns rad-mapper.promesa-config
  (:refer-clojure :exclude [delay do spread promise
                            await map mapcat run!
                            future let loop recur -> ->>
                            with-redefs
                            doseq])
  (:require [clojure.core :as c]
            [promesa.core :as p]
            [promesa.exec :as exec]
            [promesa.protocols :as pt]
            [sci.core :as sci]))

(def pns (sci/create-ns 'promesa.core nil))
(def ptns (sci/create-ns 'promesa.protocols nil))

(defn ^:macro do*
  "An exception unsafe do-like macro. Supposes that we are already
  wrapped in promise context so avoids defensive wrapping."
  [_ _ & exprs]
  (condp = (count exprs)
    0 `(impl/resolved nil)
    1 `(pt/-promise ~(first exprs))
    (reduce (fn [acc e]
              `(pt/-mcat (pt/-promise ~e) (fn [_#] ~acc)))
            `(pt/-promise ~(last exprs))
            (reverse (butlast exprs)))))

(defn ^:macro do
  "Execute potentially side effectful code and return a promise resolved
  to the last expression after awaiting the result of each
  expression."
  [_ _ & exprs]
  `(pt/-mcat
    (pt/-promise nil)
    (fn [_#]
      (p/do* ~@exprs))))

(defn ^:macro do!
  "A convenience alias for `do` macro."
  [_ _ & exprs]
  `(p/do ~@exprs))

(defn ^:macro let*
  "An exception unsafe let-like macro. Supposes that we are already
  wrapped in promise context so avoids defensive wrapping."
  [_ _ bindings & body]
  (assert (even? (count bindings)) (str "Uneven binding vector: " bindings))
  (c/->> (reverse (partition 2 bindings))
         (reduce (fn [acc [l r]]
                   `(pt/-mcat (pt/-promise ~r) (fn [~l] ~acc)))
                 `(p/do* ~@body))))

(defn ^:macro let
  "A `let` alternative that always returns promise and waits for all the
  promises on the bindings."
  [_ _ bindings & body]
  (if (seq bindings)
    `(pt/-mcat
      (pt/-promise nil)
      (fn [_#] (promesa.core/let* ~bindings ~@body)))
    `(p/do ~@body)))

(defn ^:macro ->
  "Like the clojure.core/->, but it will handle promises in values
  and make sure the next form gets the value realized instead of
  the promise. Example using to fetch data in the browser with CLJS:
  Example:
  (p/-> (js/fetch #js {...}) ; returns a promise
        .-body)
  The result of a thread is a promise that will resolve to the
  end of the thread chain."
  [_ _ x & forms]
  (c/let [fns (mapv (fn [arg]
                      (c/let [[f & args] (if (sequential? arg)
                                           arg
                                           (list arg))]
                        `(fn [p#] (~f p# ~@args)))) forms)]
    `(p/chain (p/promise ~x) ~@fns)))

(defn ^:macro ->>
  "Like the clojure.core/->>, but it will handle promises in values
  and make sure the next form gets the value realized instead of
  the promise. Example using to fetch data in the browser with CLJS:
  Example:
  (p/->> (js/fetch #js {...}) ; returns a promise
         .-body
         read-string
         (mapv inc)
  The result of a thread is a promise that will resolve to the
  end of the thread chain."
  [_ _ x & forms]
  (c/let [fns (mapv (fn [arg]
                      (c/let [[f & args] (if (sequential? arg)
                                           arg
                                           (list arg))]
                        `(fn [p#] (~f ~@args p#)))) forms)]
    `(p/chain (p/promise ~x) ~@fns)))

(defn ^:macro with-redefs
  "Like clojure.core/with-redefs, but it will handle promises in
  body and wait until they resolve or reject before restoring the
  bindings. Useful for mocking async APIs.
  Example:
  (defn async-func [] (p/delay 1000 :slow-original))
  (p/with-redefs [async-func (fn [] (p/resolved :fast-mock))]
    (async-func))
  The result is a promise that will resolve to the last body form and
  upon resolving restores the bindings to their original values."
  [_ _ bindings & body]
  (c/let [names (take-nth 2 bindings)
          vals (take-nth 2 (drop 1 bindings))
          orig-val-syms (c/map (comp gensym #(str % "-orig-val__") name) names)
          temp-val-syms (c/map (comp gensym #(str % "-temp-val__") name) names)
          binds (c/map vector names temp-val-syms)
          resets (reverse (c/map vector names orig-val-syms))
          bind-value (fn [[k v]]
                       (list 'clojure.core/alter-var-root (list 'var k)
                             (list 'clojure.core/constantly v)))]
    `(c/let [~@(c/interleave orig-val-syms names)
             ~@(c/interleave temp-val-syms vals)]
       ~@(c/map bind-value binds)
       (p/-> (p/do! ~@body)
             (p/finally
               (fn []
                 ~@(c/map bind-value resets)))))))

(def ^:private
  loop-run-fn (sci/new-dynamic-var '*loop-run-fn* exec/run! {:ns pns}))

;;; "Fresh" from https://github.com/funcool/promesa/blob/master/src/promesa/core.cljc#L638
(defn ^:macro loop
  [_ _ bindings & body]
  (c/let [binds (partition 2 2 bindings)
          names (c/map first binds)
          fvals (c/map second binds)
          tsym  (gensym "loop-fn-")
          res-s (gensym "res-")
          err-s (gensym "err-")
          rej-s (gensym "reject-fn-")
          rsv-s (gensym "resolve-fn-")]
    `(create
      (fn [~rsv-s ~rej-s]
        (c/let [~tsym (fn ~tsym [~@names]
                        (c/->> (promesa.core/let [~@(c/mapcat (fn [nsym] [nsym nsym]) names)] ~@body)
                               (promesa.core/fnly
                                (fn [~res-s ~err-s]
                                  ;; (prn "result" res# err#)
                                  (if (some? ~err-s)
                                    (~rej-s ~err-s)
                                    (if (recur? ~res-s)
                                      (do
                                        (promesa.exec/run!
                                         :vthread
                                         (promesa.exec/wrap-bindings
                                          ~(if (seq names)
                                             `(fn [] (apply ~tsym (:bindings ~res-s)))
                                             tsym)))
                                        nil)
                                      (~rsv-s ~res-s)))))))]
          (promesa.exec/run!
           :vthread
           (promesa.exec/wrap-bindings
            ~(if (seq names)
               `(fn [] (~tsym ~@fvals))
               tsym))))))))

;;; "Fresh" from https://github.com/funcool/promesa/blob/master/src/promesa/core.cljc#L674
(defn ^:macro recur
  [_ _ & args]
  `(->Recur [~@args]))

(defn ^:macro doseq
  "Simplified version of `doseq` which takes one binding and a seq, and
  runs over it using `promesa.core/run!`"
  [_ _ [binding xs] & body]
  `(p/run!
    (fn [~binding]
      (p/do ~@body))
    ~xs))

(defn ^:macro future
  "Analogous macro to `clojure.core/future` that returns promise
  instance instead of the `Future`. Exposed just for convenience and
  works as an alias to `thread`."
  [_ _ & body]
  `(p/thread-call :default (^:once fn [] ~@body)))

(def promesa-namespace
  {;'*loop-run-fn* loop-run-fn
   '->            (sci/copy-var -> pns)
   '->>           (sci/copy-var ->> pns)
   'all           (sci/copy-var p/all pns)
   'any           (sci/copy-var p/any pns)
   'catch         (sci/copy-var p/catch pns)
   'chain         (sci/copy-var p/chain pns)
   'create        (sci/copy-var p/create pns)
   'deferred      (sci/copy-var p/deferred pns)
   'delay         (sci/copy-var p/delay pns)
   'do            (sci/copy-var do! pns)
   'do*           (sci/copy-var do* pns) ; my 10.0.663, then commented
   'do!           (sci/copy-var do! pns)
;   'error         (sci/copy-var p/error pns) ; Not in my 10.0.663 ?
   'finally       (sci/copy-var p/finally pns)
   'future        (sci/copy-var future pns)
   'thread-call   (sci/copy-var p/thread-call pns)
   'handle        (sci/copy-var p/handle pns)
   'let           (sci/copy-var let pns)
   'let*          (sci/copy-var let* pns)   ; My 10.0.663
   'loop          (sci/copy-var loop pns)
   'map           (sci/copy-var p/map pns)
   'mapcat        (sci/copy-var p/mapcat pns)
   'promise       (sci/copy-var p/promise pns)
   'promise?      (sci/copy-var p/promise? pns)
   'race          (sci/copy-var p/race pns)
   'recur         (sci/copy-var recur pns)
   'reject!       (sci/copy-var p/reject! pns)
   'rejected      (sci/copy-var p/rejected pns)
   'resolve!      (sci/copy-var p/resolve! pns)
   'resolved      (sci/copy-var p/resolved pns)
   'run!          (sci/copy-var p/run! pns)
   'then          (sci/copy-var p/then pns)
   'thenable?     (sci/copy-var p/thenable? pns)
   'with-redefs   (sci/copy-var with-redefs pns)
   'wrap          (sci/copy-var p/wrap pns)
   'doseq         (sci/copy-var doseq pns)})

(def promesa-protocols-namespace
  {; '-bind (sci/copy-var pt/-bind ptns) ; Not in my 10.0.633
   '-mcat (sci/copy-var pt/-mcat ptns)  ; I'm guessing
   '-promise (sci/copy-var pt/-promise ptns)})

(def namespaces {'promesa.core promesa-namespace
                 'promesa.protocols promesa-protocols-namespace})

(def config {:namespaces namespaces})
