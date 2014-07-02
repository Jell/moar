(ns moar.monads.maybe
  (:require [moar.protocols :refer :all]
            [moar.core      :refer :all]
            [moar.monads.transformer :refer [transformer]]))

(declare just nothing wrapped-monad maybe-or)

(defprotocol Maybe
  (just? [this]))

(deftype MaybeMonad []
  Monad
  (wrap* [_ value] (just value))
  (bind* [_ monad fun]
    (if (just? monad)
      (fun @monad)
      nothing))

  MonadTransformerImpl
  (->monad-transformer [impl wrapper-impl]
    (wrapped-monad wrapper-impl))

  MonadPlus
  (mzero* [_] nothing)
  (mplus* [_ monad-a monad-b]
    (maybe-or (monad-a monad-b))))

(deftype MaybeMonadWrapped [wrapper-impl]
  MonadTransformerImpl
  (->monad-transformer [impl wrapper-impl]
    (wrapped-monad wrapper-impl))

  Monad
  (wrap* [this value]
    (transformer (wrap wrapper-impl (just value)) this))
  (bind* [this transformer-value monadic-function]
    (transformer
     (mlet
      [maybe-value @transformer-value]
      (if (just? maybe-value)
        @(monadic-function @maybe-value)
        (wrap wrapper-impl nothing)))
     this))

  MonadLift
  (wrapper-implementation [this] wrapper-impl)
  (lift* [this monadic-value]
    (transformer (fmap just monadic-value) this))

  MonadPlus
  (mzero* [this] (transformer (wrap wrapper-impl nothing) this))
  (mplus* [this transformer-a transformer-b]
   (letfn [(return [maybe-value]
              (transformer (wrap wrapper-impl maybe-value) this))]
      (mlet
       [maybe-value-a @transformer-a
        maybe-value-b @transformer-b]
       (return (mplus maybe-value-a maybe-value-b))))))

(def monad (MaybeMonad.))

(defn wrapped-monad [wrapper-impl]
  (MaybeMonadWrapped. wrapper-impl))

(deftype Just [value]
  clojure.lang.IDeref
  (deref [this] value)
  Maybe
  (just? [_] true)
  MonadInstance
  (->monad-implementation [_] monad)
  Object
  (equals [_ other]
    (and (instance? Just other)
         (= value @other))))

(defn just [value] (Just. value))

(deftype Nothing []
  ;; clojure.lang.IDeref
  ;; (deref [this] (throw (Exception. "Can't deref nothing")))
  Maybe
  (just? [_] false)
  MonadInstance
  (->monad-implementation [_] monad)
  Object
  (equals [_ other] (instance? Nothing)))

(def nothing (Nothing.))

(defn maybe-or [maybe-a maybe-b]
  (case [(just? maybe-a) (just? maybe-b)]
    [false false] nothing
    [ true false] maybe-a
    [false  true] maybe-b
    [ true  true] maybe-a))
