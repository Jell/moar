(ns moar.protocols)

(defprotocol MonadInstance
  "A monadic value should satisfy this protocol"
  (->monad-implementation [t]
    "Returns the implementation of the underlying monad"))

(defprotocol Monad
  (wrap* [impl value])
  (bind* [impl monad function]))

(defprotocol MonadPlus
  (mzero* [impl])
  (mplus* [impl monad-a monad-b]))

(defprotocol MonadLift
  (lift* [impl monadic-value])
  (wrapper-implementation [impl]))

(defprotocol MonadTransformerImpl
  (->monad-transformer [impl wrapper-impl]))
