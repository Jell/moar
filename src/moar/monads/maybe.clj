(ns moar.monads.maybe
  (:require [moar.protocols :refer :all]
            [moar.core :refer :all]))

(declare t monad-t just nothing maybe-or)

(defprotocol Maybe
  (just? [this]))

(defrecord MaybeMonad []
  Monad
  (wrap* [_ value] (just value))
  (bind* [_ monad fun] (if (just? monad) (fun @monad) nothing))
  MonadPlus
  (mzero* [_] nothing)
  (mplus* [_ m-val-a m-val-b] (maybe-or m-val-a m-val-b))
  MonadTransformable
  (transform* [_ inner-monad m-val]
    (t (monad-t inner-monad)
       (wrap inner-monad m-val))))

(def monad (MaybeMonad.))

(deftype Just [value]
  clojure.lang.IDeref
  (deref [this] value)
  Maybe
  (just? [_] true)
  MonadInstance
  (monad-implementation [_] monad)
  Object
  (equals [_ other]
    (and (instance? Just other)
         (= value @other))))

(defn just [value] (Just. value))

(deftype Nothing []
  Maybe
  (just? [_] false)
  MonadInstance
  (monad-implementation [_] monad))

(def nothing (Nothing.))

(defn maybe-or [maybe-a maybe-b]
  (case [(just? maybe-a) (just? maybe-b)]
    [false false] nothing
    [ true false] maybe-a
    [false  true] maybe-b
    [ true  true] maybe-a))

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

(defrecord MaybeTransformer [inner-monad]
  Monad
  (wrap* [self value]
    (t self (wrap inner-monad (just value))))
  (bind* [self m-val m-fun]
    (t self (bind* inner-monad
                   @m-val
                   (fn [maybe-value]
                     (if (just? maybe-value)
                       @(m-fun @maybe-value)
                       (wrap inner-monad nothing))))))
  MonadTransformer
  (inner-monad* [_] inner-monad)
  (lift* [self m-val]
    (t self (fmap just m-val))))

(defn monad-t [inner-monad]
  (MaybeTransformer. inner-monad))
