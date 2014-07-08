(ns moar.monads.maybe-t
  (:require [moar.protocols :refer :all]
            [moar.core :refer :all]
            [moar.monads.maybe :as maybe])
  (:import [moar.monads.maybe MaybeMonad]))

(declare maybe-t)

(deftype MaybeT [wrapper-impl]
  MonadTransformer
  (wrapper-impl [_] wrapper-impl)
  (base-monad* [_] maybe/monad)
  Monad
  (wrap* [self value]
    (transformer self (wrap wrapper-impl (maybe/just value))))
  (bind* [self m-val m-fun]
    (transformer
     self
     (bind* wrapper-impl
            @m-val
            (fn [maybe-value]
              (if (maybe/just? maybe-value)
                @(m-fun @maybe-value)
                (wrap wrapper-impl maybe/nothing))))))
  Object
  (equals [_ other]
    (and (instance? MaybeT other)
         (= wrapper-impl
            (.wrapper-impl other)))))

(defn maybe-t [m-impl]
  (MaybeT. m-impl))

(extend MaybeMonad
  MonadTransformable
  {:transformer-impl (fn [_ wrapper-impl]
                       (maybe-t wrapper-impl))})
