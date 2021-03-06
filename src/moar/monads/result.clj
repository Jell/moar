(ns moar.monads.result
  (:require [moar.protocols :refer :all]
            [moar.infer :refer [infer]]
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
  Functor
  (fmap* [self fun] (success (fun value)))
  MonadInstance
  (monad-implementation [_] monad)
  Object
  (equals [_ other]
    (and (instance? Success other)
         (= value @other))))
(infer Success Applicative)

(defn success [value] (Success. value))

(deftype Fail [value]
  clojure.lang.IDeref
  (deref [this] value)
  Result
  (success? [_] false)
  Functor
  (fmap* [self _] self)
  MonadInstance
  (monad-implementation [_] monad)
  Object
  (equals [_ other]
    (and (instance? Fail other)
         (= value @other))))
(infer Fail Applicative)

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
(infer T Functor)
(infer T Applicative)

(defn t [m-impl m-val]
  (T. m-impl m-val))

(defrecord ResultTransformer [inner-monad]
  MonadTransformer
  (base-monad* [_] monad)
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
                  (wrap* inner-monad result-value))))))
  MonadTransformable
  (transform* [self nested-inner-monad m-val]
    (let [inner-transformed
          (transform* inner-monad nested-inner-monad @m-val)]
      (t (monad-t (monad-implementation inner-transformed))
         inner-transformed))))

(defn monad-t [m-impl]
  (ResultTransformer. m-impl))
