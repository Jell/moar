(ns moar.monads.result
  (:require [moar.protocols :refer :all]))

(declare result fail)

(defprotocol Result
  (success? [this]))

(deftype ResultMonad []
  Monad
  (wrap* [_ value] (result value))
  (bind* [_ monad fun]
    (if (success? monad)
      (fun @monad)
      monad)))

(def monad (ResultMonad.))

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
