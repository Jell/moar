(ns moar.monads.set
  (:require [clojure.set]
            [moar.protocols :refer :all]))

(deftype SetMonad []
  Monad
  (wrap* [_ value] #{value})
  (bind* [_ monad fun]
    (apply clojure.set/union (map fun monad)))
  MonadPlus
  (mzero* [_] #{})
  (mplus* [_ monad-a monad-b]
    (clojure.set/union monad-a monad-b)))

(def monad (SetMonad.))

(extend-protocol MonadInstance
  clojure.lang.IPersistentSet
  (->monad-implementation [_] monad))
