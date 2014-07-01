(ns moar.protocols)

(defprotocol MonadInstance
  "A monadic value should satisfy this protocol"
  (->monad-implementation [t]
    "Returns the implementation of the underlying monad"))

(defprotocol Monad
  (wrap [impl value]
    "Wraps a value in a monad")
  (bind* [impl monad function]
    "Applies a function returning a monad
    to a monad of the same kind"))

(defprotocol MonadPlus
  (mzero [impl]
    "Invariant point of a monad")
  (mplus* [impl monad-a monad-b]
    "Mean of combining two monads"))

(defprotocol MonadLift
  (lift* [impl arity function]))

(defprotocol MonadTransformerImpl
  (->monad-transformer [impl]))
