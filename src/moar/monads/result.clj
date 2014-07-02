(ns moar.monads.result
  (:require [moar.protocols :refer :all]
            [moar.core :refer :all]
            [moar.monads.transformer :refer [transformer]]))

(declare result fail wrapped-monad)

(defprotocol Result
  (success? [this]))

(deftype ResultMonad []
  MonadTransformerImpl
  (->monad-transformer [impl wrapper-impl]
    (wrapped-monad wrapper-impl))

  Monad
  (wrap* [_ value] (result value))
  (bind* [_ monad fun]
    (if (success? monad)
      (fun @monad)
      monad)))

(deftype ResultMonadWrapped [wrapper-impl]
  MonadTransformerImpl
  (->monad-transformer [impl wrapper-impl]
    (wrapped-monad wrapper-impl))

  MonadLift
  (wrapper-implementation [_] wrapper-impl)
  (lift* [this monadic-value]
    (transformer (fmap result monadic-value) this))

  Monad
  (wrap* [this value]
    (transformer (wrap wrapper-impl (result value)) this))

  (bind* [this transformer-value monadic-function]
    (transformer
     (mlet
      [result-value @transformer-value]
      (if (success? result-value)
        @(monadic-function @result-value)
        (wrap wrapper-impl result-value)))
     this)))

(def monad (ResultMonad.))

(defn wrapped-monad [wrapper-impl]
  (ResultMonadWrapped. wrapper-impl))

(deftype SuccessResult [value]
  clojure.lang.IDeref
  (deref [this] value)
  Result
  (success? [_] true)
  MonadInstance
  (->monad-implementation [_] monad)
  Object
  (equals [_ other]
    (and (instance? SuccessResult other)
         (= value @other))))

(defn result [value] (SuccessResult. value))

(deftype FailResult [value]
  clojure.lang.IDeref
  (deref [this] value)
  Result
  (success? [_] false)
  MonadInstance
  (->monad-implementation [_] monad)
  Object
  (equals [_ other]
    (and (instance? FailResult other)
         (= value @other))))

(defn fail [value]
  (FailResult. value))
