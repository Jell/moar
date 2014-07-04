(ns moar.protocols)

(defprotocol MonadInstance
  "A monadic value should satisfy this protocol"
  (monad-implementation [t]
    "Returns the implementation of the underlying monad"))

(defprotocol Monad
  (wrap* [impl val])
  (bind* [impl m-val m-fun]))

(defprotocol MonadPlus
  (mzero* [impl])
  (mplus* [impl m-val-a m-val-b]))

(defprotocol MonadTransformer
  (wrap-t [impl m-fun]))
