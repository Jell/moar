(ns moar.core
  (:require [moar.protocols :refer :all]
            [moar.monads.maybe :as maybe]))

(defn monad-instance?
  "Checks whether monads have the given implementation"
  [impl & monads]
  (every? #(and (satisfies? MonadInstance %)
                (= impl (->monad-implementation %)))
          monads))

(defn same-monad?
  "Checks whether two monads have the same implementation"
  [monad-a monad-b]
  (and
   (satisfies? MonadInstance monad-a)
   (satisfies? MonadInstance monad-b)
   (= (->monad-implementation monad-a)
      (->monad-implementation monad-b))))

(defn wrap
  "Wraps a value in a monad"
  [impl value]
  (wrap* impl value))

(defn bind
  "Applies a function returning a monad to a monad of the same kind"
  ([monad function]
     (bind (->monad-implementation monad) monad function))
  ([impl monad function]
     {:post [(same-monad? monad %)]}
     (bind* impl monad function)))

(defn >>=
  "Applies bind sequentially to n functions"
  [monad & functions]
  (reduce bind monad functions))

(defn- >>-body [[head & tail :as body]]
  {:pre [(>= (count body) 1)]}
  (if (empty? tail)
    head
    `(bind ~head (fn [_#] ~(>>-body tail)))))

(defmacro >>
  "Chain actions returning discarding intermediate results"
  [& actions]
  (>>-body actions))

(defn mzero
  "Invariant point of a monad"
  [impl]
  (mzero* impl))

(defn- mlet-body
  [[binding expr & rest :as bindings] body]
  {:pre [(even? (count bindings))]}
  (cond
   (empty? bindings) `(>> ~@body)
   (= binding :let)  `(let ~expr ~(mlet-body rest body))
   :else             `(>>= ~expr (fn [~binding] ~(mlet-body rest body)))))

(defmacro mlet
  "TODO: document"
  [bindings & body]
  (mlet-body bindings body))

(defn mplus
  "Mean of combining two monads"
  ([monad-a monad-b]
     (mplus (->monad-implementation monad-a) monad-a monad-b))
  ([impl monad-a monad-b]
     {:pre [(monad-instance? impl monad-a monad-b)]}
     (mplus* impl monad-a monad-b)))
