(ns moar.core)

(defprotocol Monad
  (wrap [impl value])
  (bind* [impl monad function]))

(defprotocol MonadImpl
  (->monad-implementation [t]))

(defn bind
  ([monad function]
     (bind* (->monad-implementation monad) monad function))
  ([impl monad function]
     (bind* impl monad function)))

(defprotocol MonadPlus
  (mzero [impl])
  (mplus [impl monad-a monad-b]))

(defprotocol MonadLift
  (lift [impl function]))

(defprotocol MonadTransformerImpl
  (->monad-transformer [impl]))

(declare just nothing)
(defprotocol Maybe
  (just? [this]))

(deftype MaybeImpl []
  Monad
  (wrap [_ value] (just value))
  (bind* [_ monad fun]
    (if (just? monad)
      (fun @monad)
      nothing)))

(def maybe-impl (MaybeImpl.))

(deftype Just [value]
  clojure.lang.IDeref
  (deref [this] value)
  Maybe
  (just? [_] true)
  MonadImpl
  (->monad-implementation [_] maybe-impl))

(deftype Nothing []
  ;; clojure.lang.IDeref
  ;; (deref [this] (throw (Exception. "Can't deref nothing")))
  Maybe
  (just? [_] false)
  MonadImpl
  (->monad-implementation [_] maybe-impl))

(defn just [value] (Just. value))
(def nothing (Nothing.))

(defn- mdo* [[head & tail :as body]]
  {:pre [(>= (count body) 1)]}
  (if (empty? tail)
    head
    `(bind ~head (fn [_#] ~(mdo* tail)))))

(defmacro mdo [& body]
  (mdo* body))

(mdo (just 5)
     nothing
     (throw (Exception.)))

(just? nothing)

(bind (just 5) (fn [x] (just (* 2 x))))
