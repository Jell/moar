(ns moar.monads.continuation
  (:require [moar.protocols :refer :all]))

(deftype ContinuationFn [m-impl fun]
  MonadInstance
  (monad-implementation [_] m-impl)
  clojure.lang.IFn
  (invoke [_ callback] (fun callback)))

(defn continuation-fn [m-impl fun]
  (ContinuationFn. m-impl fun))

(defprotocol MonadContinuation
  (callcc [self fun]))

(defrecord ContinuationMonad []
  MonadContinuation
  (callcc [self fun]
    (continuation-fn self
                     (fn [callback]
                       ((fun (fn [value]
                               (continuation-fn
                                self
                                (fn [_callback]
                                  (callback value)))))
                        callback))))
  Monad
  (wrap* [self val]
    (continuation-fn self (fn [callback] (callback val))))
  (bind* [self m-val m-fun]
    (continuation-fn
     self
     (fn [callback]
       (m-val
        (fn [val]
          ((m-fun val) callback)))))))

(def monad (ContinuationMonad.))

(defrecord ContinuationTransformer [inner-monad]
  MonadContinuation
  (callcc [self fun]
    (continuation-fn self
                     (fn [callback]
                       ((fun (fn [value]
                               (continuation-fn
                                self
                                (fn [_callback]
                                  (callback value)))))
                        callback))))
  MonadTransformer
  (inner-monad* [_] inner-monad)
  (lift* [self m-val]
    (continuation-fn
     self
     (fn [callback]
       (bind* inner-monad
              m-val
              (fn [val]
                (callback val))))))
  Monad
  (wrap* [self val]
    (continuation-fn self (fn [callback] (callback val))))
  (bind* [self m-val m-fun]
    (continuation-fn
     self
     (fn [callback]
       (m-val
        (fn [val]
          ((m-fun val) callback)))))))

(defn monad-t [inner-monad]
  (ContinuationTransformer. inner-monad))
