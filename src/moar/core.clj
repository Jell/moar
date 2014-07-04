(ns moar.core
  (:require [moar.protocols :refer :all]
            [moar.monads.id :refer [id]]))

(defn monad-instance?
  "Checks whether monads have the given implementation"
  [m-impl & m-vals]
  (every? #(and (satisfies? MonadInstance %)
                (= m-impl (monad-implementation %)))
          m-vals))

(defn same-monad?
  "Checks whether two monads have the same implementation"
  [m-val-a m-val-b]
  (and
   (satisfies? MonadInstance m-val-a)
   (satisfies? MonadInstance m-val-b)
   (= (monad-implementation m-val-a)
      (monad-implementation m-val-b))))

(defn wrap
  "Wraps a value in a monad"
  [m-impl val]
  (wrap* m-impl val))

(defn bind
  "Applies a function returning a monad to a monad of the same kind"
  ([m-val m-fun]
     {:pre [(satisfies? MonadInstance m-val)]}
     (bind (monad-implementation m-val) m-val m-fun))
  ([m-impl m-val m-fun]
     {:post [(same-monad? m-val %)]}
     (bind* m-impl m-val m-fun)))

(defn >>=
  "Applies bind sequentially to n functions"
  [m-val & m-funs]
  (reduce bind m-val m-funs))

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
  [m-impl]
  (mzero* m-impl))

(defn mplus
  "Mean of combining two monads"
  ([m-val-a m-val-b]
     (mplus (monad-implementation m-val-a) m-val-a m-val-b))
  ([impl m-val-a m-val-b]
     {:pre [(monad-instance? impl m-val-a m-val-b)]}
     (mplus* impl m-val-a m-val-b)))

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
  [fun m-val]
  {:pre [(satisfies? MonadInstance m-val)]}
  (mlet
   [value m-val]
   (wrap (monad-implementation m-val) (fun value))))

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
    (let [impl (monad-implementation (first collection))]
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
  [m-fun collection]
  (m-sequence (map m-fun collection)))

(defn join
  "given a nested monad value
   join lifts the value up and gives you a flat monadic value

   Example:
   (= (join (id (id 1))) (id 1))"
  [m-val]
  (>>= m-val identity))

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
    (let [impl (monad-implementation (first (vals map)))]
      (reduce
       (fn [m [key item]]
         (mlet
          [res-map m
           value   item]
          (wrap impl (assoc res-map key value))))
       (wrap impl {})
       map))))

(deftype Transformer [t-impl m-val]
  clojure.lang.IDeref
  (deref [this] m-val)
  MonadInstance
  (monad-implementation [_] t-impl)
  Object
  (equals [_ other]
    (and (instance? Transformer other)
         (= m-val @other))))

(defn transformer [t-impl m-val]
  (Transformer. t-impl m-val))

(defn transformer? [x]
  (instance? Transformer x))

(defn intermediate-monads [m-impl-a m-impl-b]
  {:pre [(satisfies? Monad m-impl-a)
         (satisfies? Monad m-impl-b)]}
  (if (= m-impl-a m-impl-b)
    []
    (if (satisfies? MonadTransformer m-impl-a)
      (let [m-impl (wrapped-impl m-impl-a)]
        (conj (intermediate-monads m-impl m-impl-b)
              m-impl-a))
      [])))

(defn lift-value
  [m-impl val]
  {:pre [(satisfies? Monad m-impl)]}
  (if (satisfies? MonadInstance val)
    (reduce
     (fn [m-val m-next]
       (transformer
        m-next
        (fmap (partial wrap (base-monad m-next)) m-val)))
     val
     (intermediate-monads m-impl (monad-implementation val)))
    (wrap m-impl val)))

(defn lift
  [m-impl fun]
  {:pre [(satisfies? Monad m-impl)]}
  (fn [& args]
    (lift-value m-impl (apply fun args))))
