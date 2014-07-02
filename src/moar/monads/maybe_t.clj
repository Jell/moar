(ns moar.monads.maybe-t
  (:require [moar.protocols :refer :all]
            [moar.core :refer :all]
            [moar.monads.maybe :as maybe]))

(declare run-maybe-t)

(deftype MaybeT [wrapped-impl]
  Monad
  (wrap* [_ value]
    (run-maybe-t wrapped-impl (wrap wrapped-impl (maybe/just value))))
  (bind* [self monad fun]
    (run-maybe-t
     wrapped-impl
     (bind @monad
           (fn [maybe-value]
             (if (maybe/just? maybe-value)
               (let [new-monad (fun @maybe-value)]
                 (if (monad-instance? self new-monad)
                   @new-monad
                   new-monad))
               (wrap wrapped-impl maybe/nothing))))))
  Object
  (equals [_ other]
    (and (instance? MaybeT other)
         (= wrapped-impl
            (.wrapped-impl other)))))

(defn maybe-t [impl]
  (MaybeT. impl))

(deftype RunMaybeT [impl value]
  clojure.lang.IDeref
  (deref [this] value)
  MonadInstance
  (->monad-implementation [_] (maybe-t impl))
  Object
  (equals [_ other]
    (and (instance? RunMaybeT other)
         (same-monad? impl (.impl other))
         (= value @other))))

(defn run-maybe-t [impl value]
  (RunMaybeT. impl value))
