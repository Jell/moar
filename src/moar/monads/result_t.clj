(ns moar.monads.result-t
  (:require [moar.protocols :refer :all]
            [moar.core :refer :all]
            [moar.monads.result :as result])
  (:import [moar.monads.result ResultMonad]))

(deftype ResultT [wrapper-impl]
  MonadTransformer
  (wrapper-impl [_] wrapper-impl)
  (base-monad* [_] result/monad)
  Monad
  (wrap* [self value]
    (transformer self (wrap wrapper-impl (result/result value))))
  (bind* [self m-val m-fun]
    (transformer
     self
     (bind* wrapper-impl
            @m-val
            (fn [result-value]
              (if (result/success? result-value)
                @(m-fun @result-value)
                (wrap wrapper-impl result-value))))))
  Object
  (equals [_ other]
    (and (instance? ResultT other)
         (= wrapper-impl
            (.wrapper-impl other)))))

(defn result-t [m-impl]
  (ResultT. m-impl))

(extend ResultMonad
  MonadTransformable
  {:transformer-impl (fn [_ wrapper-impl]
                       (result-t wrapper-impl))})
