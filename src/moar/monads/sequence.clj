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
  (wrap* [_ val] (list val))
  (bind* [_ m-val m-fun]
    (flatten* (map m-fun m-val)))
  MonadPlus
  (mzero* [_] (list))
  (mplus* [_ m-val-a m-val-b]
    (flatten* [m-val-a m-val-b])))

(def monad (SequenceMonad.))

(extend-protocol MonadInstance
  clojure.lang.LazySeq
  (monad-implementation [_] monad)
  clojure.lang.IPersistentList
  (monad-implementation [_] monad))
