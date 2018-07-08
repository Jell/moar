(ns moar.protocols
  (:require [clojure.core.typed :as t]))

(t/defprotocol Functor
  (fmap* [m-val fun]))

(t/defprotocol MonadInstance
  "A monadic value should satisfy this protocol"
  (monad-implementation [t]))

(t/defprotocol Monad
  (wrap* [impl val])
  (bind* [impl m-val m-fun]))

(t/defprotocol MonadPlus
  (mzero* [impl])
  (mplus* [impl m-val-a m-val-b]))

(t/defprotocol
  MonadTransformer
  (base-monad* [self])
  (inner-monad* [self])
  (lift* [self m-val]))

(t/defprotocol MonadTransformable
  (transform* [self inner-monad m-val]))

(t/ann-protocol [[v :variance :covariant]]
  Functor
  fmap* [(Functor v) [v -> v] -> (Functor v)])

(t/defprotocol Applicative
  (pure* [f-val val])
  (fapply* [f-fun f-vals]))

(t/ann-protocol [domain]
  Applicative
  pure* [(Applicative domain) domain -> (Applicative domain)]
  fapply* (t/All [x ...]
            [(Applicative [x ... x -> domain])
             (Applicative x) ... x
             -> (Applicative domain)]))
