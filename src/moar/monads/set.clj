(ns moar.monads.set
  (:require [clojure.set]
            [moar.protocols :refer :all]))

(deftype SetMonad []
  Monad
  (wrap* [_ val] #{val})
  (bind* [_ m-val m-fun]
    (apply clojure.set/union (map m-fun m-val)))
  MonadPlus
  (mzero* [_] #{})
  (mplus* [_ m-val-a m-val-b]
    (clojure.set/union m-val-a m-val-b)))

(def monad (SetMonad.))

(extend-protocol MonadInstance
  clojure.lang.IPersistentSet
  (monad-implementation [_] monad))
