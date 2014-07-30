(ns moar.monads.sequence
  (:require [moar.protocols :refer :all]
            [moar.core :refer :all]
            [moar.infer :refer [infer]]))

(defn- flatten*
  "Like #(apply concat %), but fully lazy: it evaluates each sublist
   only when it is needed."
  [ss]
  (lazy-seq
   (when-let [s (seq ss)]
     (concat (first s) (flatten* (rest s))))))

(defrecord SequenceMonad []
  Monad
  (wrap* [_ val] (list val))
  (bind* [_ m-val m-fun]
    (flatten* (map m-fun m-val)))
  MonadPlus
  (mzero* [_] (list))
  (mplus* [_ m-val-a m-val-b]
    (flatten* [m-val-a m-val-b])))

(def monad (SequenceMonad.))

(extend clojure.lang.LazySeq
  Functor
  {:fmap* (fn [self fun] (map fun self))}
  MonadInstance
  {:monad-implementation (fn [_] monad)})
(derive clojure.lang.LazySeq :moar.infer/monad-instance)
(infer clojure.lang.LazySeq Applicative)

(extend clojure.lang.IPersistentList
  Functor
  {:fmap* (fn [self fun] (map fun self))}
  MonadInstance
  {:monad-implementation (fn [_] monad)})
(derive clojure.lang.IPersistentList :moar.infer/monad-instance)
(infer clojure.lang.IPersistentList Applicative)
