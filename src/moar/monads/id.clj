(ns moar.monads.id
  (:require [moar.protocols :refer :all]))

(declare id)

(deftype IDMonad []
  Monad
  (wrap* [_ val] (id val))
  (bind* [_ m-val m-fun]
    (m-fun @m-val)))

(def monad (IDMonad.))

(deftype ID [value]
  clojure.lang.IDeref
  (deref [this] value)
  MonadInstance
  (->monad-implementation [_] monad)
  Object
  (equals [_ other]
    (and (instance? ID other) (= value @other))))

(defn id
  "returns a ID monad instance containing the given value"
  [value]
  (ID. value))
