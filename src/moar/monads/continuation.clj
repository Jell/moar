(ns moar.monads.continuation
  (:require [moar.protocols :refer :all]))

(declare continue-with make-continuation)

(defprotocol Continuation
  (call-continuation [continuation callback]
    "invoke the continuation with a given callback"))

(deftype ContinuationMonad []
  Monad
  (wrap* [_ value] (continue-with value))
  (bind* [_ monad fun]
    (make-continuation
     (fn [callback]
       (call-continuation
        monad
        (fn [value]
          (call-continuation (fun value) callback)))))))

(def monad (ContinuationMonad.))

(deftype ContinuationFunction [function]
  clojure.lang.IFn
  (invoke [_ callback] (function callback))
  Continuation
  (call-continuation [_ callback] (function callback))
  MonadInstance
  (->monad-implementation [_] monad))

(defn make-continuation
  "construct a continuation function"
  [function]
  (ContinuationFunction. function))

(defn continue-with
  "create a continuation function that calls
   the continuation with the given value"
  [value]
  (make-continuation (fn [callback] (callback value))))

(defn callcc
  "takes a function and calls it with the current continuation
   and returns a continuation monad"
  [fun]
  (make-continuation
   (fn [callback]
     (call-continuation
      (fun (fn [value]
             (make-continuation
              (fn [_callback]
                (callback value)))))
      callback))))
