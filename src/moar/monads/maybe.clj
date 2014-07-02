(ns moar.monads.maybe
  (:require [moar.protocols :refer :all]))

(declare just nothing)
(defprotocol Maybe
  (just? [this]))

(deftype MaybeMonad []
  Monad
  (wrap* [_ value] (just value))
  (bind* [_ monad fun]
    (if (just? monad)
      (fun @monad)
      nothing))
  MonadPlus
  (mzero* [_] nothing)
  (mplus* [_ m-val-a m-val-b]
    (case [(just? m-val-a) (just? m-val-b)]
      [false false] nothing
      [ true false] m-val-a
      [false  true] m-val-b
      [ true  true] m-val-a)))

(def monad (MaybeMonad.))

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
