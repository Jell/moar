(ns moar.monads.result
  (:require [moar.protocols :refer :all]
            [moar.core :refer :all]))

(declare t monad-t success fail)

(defprotocol Result
  (success? [this]))

(defrecord ResultMonad []
  Monad
  (wrap* [_ val] (success val))
  (bind* [_ m-val m-fun]
    (if (success? m-val)
      (m-fun @m-val)
      m-val))
  MonadTransformable
  (transform* [_ inner-monad m-val]
    (t (monad-t inner-monad)
       (wrap* inner-monad m-val))))

(def monad (ResultMonad.))

(deftype Success [value]
  clojure.lang.IDeref
  (deref [this] value)
  Result
  (success? [_] true)
  MonadInstance
  (monad-implementation [_] monad)
  Object
  (equals [_ other]
    (and (instance? Success other)
         (= value @other))))

(defn success [value] (Success. value))

(deftype Fail [value]
  clojure.lang.IDeref
  (deref [this] value)
  Result
  (success? [_] false)
  MonadInstance
  (monad-implementation [_] monad)
  Object
  (equals [_ other]
    (and (instance? Fail other)
         (= value @other))))

(defn fail [value]
  (Fail. value))

(deftype T [m-impl m-val]
  clojure.lang.IDeref
  (deref [this] m-val)
  MonadInstance
  (monad-implementation [_] m-impl)
  Object
  (equals [_ other]
    (and (instance? T other)
         (= m-val @other)
         (= m-impl (.m-impl other)))))

(defn t [m-impl m-val]
  (T. m-impl m-val))

(defrecord ResultTransformer [inner-monad]
  MonadTransformer
  (inner-monad* [_] inner-monad)
  (lift* [self m-val] (t self (fmap success m-val)))
  Monad
  (wrap* [self value]
    (t self (wrap* inner-monad (success value))))
  (bind* [self m-val m-fun]
    (t self
       (bind* inner-monad
              @m-val
              (fn [result-value]
                (if (success? result-value)
                  @(m-fun @result-value)
                  (wrap* inner-monad result-value)))))))

(defn monad-t [m-impl]
  (ResultTransformer. m-impl))
