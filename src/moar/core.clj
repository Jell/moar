(ns moar.core
  (:require [moar.protocols :refer :all]
            [moar.monads.maybe :as maybe]
            [moar.monads.id :refer [id]]))

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
     {:pre [(satisfies? MonadInstance monad)]}
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

(defn mplus
  "Mean of combining two monads"
  ([monad-a monad-b]
     (mplus (->monad-implementation monad-a) monad-a monad-b))
  ([impl monad-a monad-b]
     {:pre [(monad-instance? impl monad-a monad-b)]}
     (mplus* impl monad-a monad-b)))

(defn- mlet-body
  [[binding expr & rest :as bindings] body]
  {:pre [(even? (count bindings))]}
  (cond
   (empty? bindings) `(>> ~@body)
   (= binding :let)  `(let ~expr ~(mlet-body rest body))
   :else             `(>>= ~expr (fn [~binding] ~(mlet-body rest body)))))

(defmacro mlet
  "Evaluates the body in a lexical context where the symbols in
  bindings are bound to the return value of their respective
  monads. A :let modifier can be used to bind non-monadic values.

  Example:
  (mlet [x (just 5) y (just 2) :let [z (+ x y)]] (just z))"
  [bindings & body]
  (mlet-body bindings body))

(defn fmap
  "takes a function and a monad and
   returns a new monad with that function applied
   to the value

   Example:
   (= (just 2) (fmap inc (just 1)))"
  [fun monad]
  {:pre [(satisfies? MonadInstance monad)]}
  (mlet
   [value monad]
   (wrap (->monad-implementation monad) (fun value))))

(defn m-sequence
  "given a collection of monadic values
   it returns a monadic value where the value
   is a collection of the values in the given
   monadic values

   Example:
   (= (m-sequence [(id 1) (id 2)]) (id [1 2]))"
  [collection]
  (if (empty? collection)
    collection
    (let [impl (->monad-implementation (first collection))]
      (reduce
       (fn [m item]
         (mlet
          [col m
           v   item]
          (wrap impl (conj col v))))
       (wrap impl [])
       collection))))

(defn map-m
  "given a collection and a function
   from items in that collection to a monadic value
   map-m returns a monad containing a list of those values

   Example:
   (= (id [2 3]) (map-m (comp id inc) [1 2]))"
  [fun collection]
  (m-sequence (map fun collection)))

(defn join
  "given a nested monad value
   join lifts the value up and gives you a flat monadic value

   Example:
   (= (join (id (id 1))) (id 1))"
  [monad]
  (>>= monad identity))

(defn extract-m
  "given a map where the values are monad values
   it returns a monad where the value is a map
   and the values are the values of the input
   monads

   Example:
   (= (id {:a 1 :b 2}) (extract-m {:a (id 1) :b (id 2)}))"
  [map]
  (if (empty? map)
    map
    (let [impl (->monad-implementation (first (vals map)))]
      (reduce
       (fn [m [key item]]
         (mlet
          [res-map m
           value   item]
          (wrap impl (assoc res-map key value))))
       (wrap impl {})
       map))))
