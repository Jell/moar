(ns moar.monads.maybe
  (:require [moar.protocols :refer :all]))

(declare just nothing)
(defprotocol Maybe
  (just? [this]))

(deftype MaybeMonad []
  Monad
  (wrap [_ value] (just value))
  (bind* [_ monad fun]
    (if (just? monad)
      (fun @monad)
      nothing))
  MonadPlus
  (mzero [_] nothing)
  (mplus* [_ monad-a monad-b]
    (case [(just? monad-a) (just? monad-b)]
      [false false] nothing
      [ true false] monad-a
      [false  true] monad-b
      [ true  true] monad-a)))

(def monad (MaybeMonad.))

(deftype Just [value]
  clojure.lang.IDeref
  (deref [this] value)
  Maybe
  (just? [_] true)
  MonadInstance
  (->monad-implementation [_] monad))

(defn just [value] (Just. value))

(deftype Nothing []
  ;; clojure.lang.IDeref
  ;; (deref [this] (throw (Exception. "Can't deref nothing")))
  Maybe
  (just? [_] false)
  MonadInstance
  (->monad-implementation [_] monad))

(def nothing (Nothing.))
