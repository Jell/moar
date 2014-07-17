(ns moar.monads.state
  (:require [moar.protocols :refer :all]))

(declare monad monad-t state-fn)

(defrecord Pair [state result])

(deftype StateFn [m-impl fun]
  MonadInstance
  (monad-implementation [_] m-impl)
  clojure.lang.IFn
  (invoke [this state] (fun state)))

(defn state-fn [m-impl fun]
  (StateFn. m-impl fun))

(defprotocol MonadState
  (pull* [impl])
  (push* [impl value]))

(defrecord StateMonadImpl []
  MonadState
  (pull* [self]
    (state-fn self (fn [state] (->Pair state state))))
  (push* [self x]
    (state-fn self (fn [state] (->Pair x nil))))
  Monad
  (wrap* [self val]
    (state-fn self (fn [state] (->Pair state val))))
  (bind* [self m-val m-fun]
    (state-fn self (fn [state]
                     (let [{ir :result is :state} (m-val state)]
                       ((m-fun ir) is)))))
  MonadTransformable
  (transform* [_ inner-monad m-val]
    (state-fn (monad-t inner-monad)
              (fn [state]
                (wrap* inner-monad (m-val state))))))

(def monad
  "monad implementation for the state monad"
  (StateMonadImpl.))

(defn pull
  ([impl] (pull* impl))
  ([impl fun]
     (bind* impl
            (pull* impl)
            (fn [result]
              (wrap* impl (fun result))))))

(defn push [impl val] (push* impl val))

(defn modify [impl fun & extra-args]
  (bind* impl
         (pull* impl)
         (fn [result]
           (push* impl
                  (apply fun (concat [result] extra-args))))))

(defrecord StateTransformerImpl [inner-monad]
  Monad
  (wrap* [self val]
    (state-fn self (fn [state]
                     (wrap* inner-monad (->Pair state val)))))
  (bind* [self m-val m-fun]
    (state-fn self (fn [state]
                     (bind* inner-monad
                            (m-val state)
                            (fn [{ir :result, is :state}]
                              ((m-fun ir) is))))))

  MonadState
  (pull* [self]
    (state-fn self (fn [state]
                     (wrap* inner-monad (->Pair state state)))))
  (push* [self x]
    (state-fn self (fn [state]
                     (wrap* inner-monad (->Pair x nil)))))

  MonadTransformer
  (inner-monad* [_] inner-monad)
  (lift* [self m-val]
    (state-fn self
              (fn [state]
                (bind* inner-monad
                       m-val
                       (fn [x]
                         (wrap* inner-monad (->Pair state x))))))))

(defn monad-t [inner-monad]
  (StateTransformerImpl. inner-monad))
