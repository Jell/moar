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
  (run* [self m-val callback])
  (callcc [self fun]))

(defn run
  ([m-val] (run m-val identity))
  ([m-val callback]
     (run (monad-implementation m-val) m-val callback))
  ([m-impl m-val callback]
     (run* m-impl m-val callback)))

(defn- callcc-default [self fun]
  (continuation-fn
   self
   (fn [callback]
     ((fun (fn [value]
             (continuation-fn
              self
              (fn [_callback]
                (callback value)))))
      callback))))

(defn- wrap-default [self val]
  (continuation-fn self (fn [callback] (callback val))))

(defn- bind-default [self m-val m-fun]
  (continuation-fn
   self
   (fn [callback]
     (m-val
      (fn [val]
        ((m-fun val) callback))))))

(defrecord ContinuationMonad []
  MonadContinuation
  (run* [self m-val callback] (m-val callback))
  (callcc [self fun]
    (callcc-default self fun))
  Monad
  (wrap* [self val]
    (wrap-default self val))
  (bind* [self m-val m-fun]
    (bind-default self m-val m-fun)))

(def monad (ContinuationMonad.))

(defrecord ContinuationTransformer [inner-monad]
  MonadContinuation
  (run* [self m-val callback]
    (m-val (comp (partial wrap* inner-monad)
                 callback)))
  (callcc [self fun]
    (callcc-default self fun))
  Monad
  (wrap* [self val]
    (wrap-default self val))
  (bind* [self m-val m-fun]
    (bind-default self m-val m-fun))
  MonadTransformer
  (base-monad* [_] monad)
  (inner-monad* [_] inner-monad)
  (lift* [self m-val]
    (continuation-fn
     self
     (fn [callback]
       (bind* inner-monad
              m-val
              (fn [val]
                (callback val)))))))

(defn monad-t [inner-monad]
  (ContinuationTransformer. inner-monad))
