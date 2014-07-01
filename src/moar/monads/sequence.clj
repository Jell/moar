(ns moar.monads.sequence
  (:require [moar.protocols :refer :all]))

(defn- flatten*
  "Like #(apply concat %), but fully lazy: it evaluates each sublist
   only when it is needed."
  [ss]
  (lazy-seq
   (when-let [s (seq ss)]
     (concat (first s) (flatten* (rest s))))))

(deftype SequenceMonad []
  Monad
  (wrap* [_ value] (list value))
  (bind* [_ monad fun]
    (flatten* (map fun monad)))
  MonadPlus
  (mzero* [_] (list))
  (mplus* [_ monad-a monad-b]
    (flatten* [monad-a monad-b])))

(def monad (SequenceMonad.))

(extend-protocol MonadInstance
  clojure.lang.LazySeq
  (->monad-implementation [_] monad)
  clojure.lang.IPersistentList
  (->monad-implementation [_] monad))
