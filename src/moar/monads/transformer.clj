(ns moar.monads.transformer
  (:require [moar.protocols :refer :all]
            [moar.core :refer :all]))

(deftype Transformer [monadic-value wrapped-impl]
  clojure.lang.IDeref
  (deref [this] monadic-value)
  MonadInstance
  (->monad-implementation [_] wrapped-impl))

(defn transformer [monadic-value wrapped-impl]
  (Transformer. monadic-value wrapped-impl))

(defn reduce-monad-impl-vector
  [coll]
  {:pre [(<= 2 (count coll))]}
  (reduce ->monad-transformer (reverse coll)))

(defn transform-impl [coll]
  (reduce-monad-impl-vector coll))

(defn lift [impl monadic-value]
  (lift* impl monadic-value))

(defn lower [impl monadic-value]
  (transformer
   (wrap (wrapper-implementation impl) monadic-value)
   impl))
