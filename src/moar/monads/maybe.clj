(ns moar.monads.maybe
  (:require [moar.protocols :refer :all]))

(declare just nothing maybe-or)
(defprotocol Maybe
  (just? [this]))

(deftype MaybeMonad []
  Monad
  (wrap* [_ value] (just value))
  (bind* [_ monad fun] (if (just? monad) (fun @monad) nothing))
  MonadPlus
  (mzero* [_] nothing)
  (mplus* [_ m-val-a m-val-b] (maybe-or m-val-a m-val-b)))

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
  ;; clojure.lang.IDeref
  ;; (deref [this] (throw (Exception. "Can't deref nothing")))
  Maybe
  (just? [_] false)
  MonadInstance
  (monad-implementation [_] monad)
  Object
  (equals [_ other] (instance? Nothing)))

(def nothing (Nothing.))

(defn maybe-or [maybe-a maybe-b]
  (case [(just? maybe-a) (just? maybe-b)]
    [false false] nothing
    [ true false] maybe-a
    [false  true] maybe-b
    [ true  true] maybe-a))
