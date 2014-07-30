(ns moar.protocols)

(defprotocol MonadInstance
  "A monadic value should satisfy this protocol"
  (monad-implementation [t]
    "Returns the implementation of the underlying monad"))

(defprotocol Functor
  (fmap* [m-val fun]))

(defprotocol Applicative
  (pure* [f-val val])
  (fapply* [f-fun f-vals]))

(defprotocol Monad
  (wrap* [impl val])
  (bind* [impl m-val m-fun]))

(defprotocol MonadPlus
  (mzero* [impl])
  (mplus* [impl m-val-a m-val-b]))

(defprotocol MonadTransformable
  (transform* [self inner-monad m-val]))

(defprotocol MonadTransformer
  (base-monad* [self])
  (inner-monad* [self])
  (lift* [self m-val]))
