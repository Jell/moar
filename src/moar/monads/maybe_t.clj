(ns moar.monads.maybe-t
  (:require [moar.protocols :refer :all]
            [moar.core :refer :all]
            [moar.monads.maybe :as maybe]))

(declare run-maybe-t)

(deftype MaybeT [outer-monad]
  Monad
  (wrap* [_ value]
    (run-maybe-t outer-monad (wrap outer-monad (maybe/just value))))
  (bind* [_ monad fun]
    (run-maybe-t
     outer-monad
     (bind @monad
           (fn [maybe-value]
             (if (maybe/just? maybe-value)
               (fun @maybe-value)
               (wrap outer-monad maybe/nothing))))))
  Object
  (equals [_ other]
    (and (instance? MaybeT other)
         (= outer-monad
            (.outer-monad other)))))

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
