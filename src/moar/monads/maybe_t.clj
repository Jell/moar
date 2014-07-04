(ns moar.monads.maybe-t
  (:require [moar.protocols :refer :all]
            [moar.core :refer :all]
            [moar.monads.maybe :as maybe]))

(deftype MaybeT [wrapped-impl]
  MonadTransformer
  (wrapped-impl [_] wrapped-impl)
  (base-monad [_] maybe/monad)
  Monad
  (wrap* [self value]
    (transformer self (wrap wrapped-impl (maybe/just value))))
  (bind* [self monad fun]
    (transformer
     self
     (bind @monad
           (fn [maybe-value]
             (if (maybe/just? maybe-value)
               (let [new-monad (fun @maybe-value)]
                 (if (transformer? new-monad)
                   @new-monad new-monad))
               (wrap wrapped-impl maybe/nothing))))))
  Object
  (equals [_ other]
    (and (instance? MaybeT other)
         (= wrapped-impl
            (.wrapped-impl other)))))

(defn maybe-t [m-impl]
  (MaybeT. m-impl))
