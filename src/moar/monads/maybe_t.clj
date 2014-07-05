(ns moar.monads.maybe-t
  (:require [moar.protocols :refer :all]
            [moar.core :refer :all]
            [moar.monads.maybe :as maybe]))

(deftype MaybeT [wrapper-impl]
  MonadTransformer
  (wrapper-impl [_] wrapper-impl)
  (base-monad [_] maybe/monad)
  Monad
  (wrap* [self value]
    (transformer self (wrap wrapper-impl (maybe/just value))))
  (bind* [self monad fun]
    (transformer
     self
     (bind @monad
           (fn [maybe-value]
             (if (maybe/just? maybe-value)
               (let [new-monad (fun @maybe-value)]
                 (if (transformer? new-monad)
                   @new-monad new-monad))
               (wrap wrapper-impl maybe/nothing))))))
  Object
  (equals [_ other]
    (and (instance? MaybeT other)
         (= wrapper-impl
            (.wrapper-impl other)))))

(defn maybe-t [m-impl]
  (MaybeT. m-impl))
