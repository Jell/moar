(ns moar.core
  (:require [moar.utils :refer :all]
            [moar.protocols :refer :all]))

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

(defn base-monad
  "Returns the base monad for a monad transformer, or the monad itself
  otherswise"
  [m-impl]
  (if (satisfies? MonadTransformer m-impl)
    (base-monad* m-impl) m-impl))

(defn same-base-monad? [m-impl-a m-impl-b]
  (= (base-monad m-impl-a)
     (base-monad m-impl-b)))

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
  "Evaluates the body in a lexical context where the symbols in bindings
  are bound to the return value of their respective monads. A :let
  modifier can be used to bind non-monadic values.

  Example:
  (mlet [x (just 5) y (just 2) :let [z (+ x y)]] (just z))"
  [bindings & body]
  (mlet-body bindings body))

(defn fmap
  "takes a function and a monad and returns a new monad with that
  function applied to the value

  Example:
  (= (just 2) (fmap inc (just 1)))"
  [fun m-val]
  (fmap* m-val fun))

(defn pure [f-val val]
  (pure* f-val val))

(defn fapply [f-fun & f-vals]
  (fapply* f-fun f-vals))

(defn mapply
  ([m-fun m-vals]
     (mapply (monad-implementation m-fun) m-fun m-vals))
  ([m-impl m-fun m-vals]
     (bind m-impl
           m-fun
           (fn [fun]
             ((fn inner-fun [m-vals vals]
                (if (seq m-vals)
                  (bind m-impl (first m-vals)
                        (fn [val] (inner-fun (rest m-vals)
                                             (conj vals val))))
                  (wrap m-impl (apply fun vals))))
              m-vals [])))))

(defn m-sequence
  "given a collection of monadic values it returns a monadic value where
  the value is a collection of the values in the given monadic values

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
  "given a collection and a function from items in that collection to a
  monadic value map-m returns a monad containing a list of those values

  Example:
  (= (id [2 3]) (map-m (comp id inc) [1 2]))"
  [m-fun collection]
  (m-sequence (map m-fun collection)))

(defn join
  "given a nested monad value join lifts the value up and gives you a
  flat monadic value

  Example:
  (= (join (id (id 1))) (id 1))"
  [m-val]
  (>>= m-val identity))

(defn extract-m
  "given a map where the values are monad values it returns a monad
  where the value is a map and the values are the values of the input
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

(defn inner-monad [m-impl]
  (inner-monad* m-impl))

(defn lift
  "Lifts a monadic value whose monad is a wrapper of m-impl to a monadic
  value of m-impl"
  [m-impl m-val]
  {:pre [(satisfies? MonadTransformer m-impl)
         (monad-instance? (inner-monad m-impl) m-val)]
   :post [(partial monad-instance? m-impl)]}
  (lift* m-impl m-val))

(defn chain-lift [m-impls m-val]
  (reduce (fn [m-val-sub m-impl]
            (lift m-impl m-val-sub))
          m-val m-impls))

(defn transform
  ([inner-monad m-val]
     {:pre [(satisfies? Monad inner-monad)
            (satisfies? MonadInstance m-val)]}
     (transform (monad-implementation m-val)
                inner-monad
                m-val))
  ([outer-monad inner-monad m-val]
     {:pre [(satisfies? MonadTransformable outer-monad)
            (satisfies? Monad inner-monad)
            (satisfies? MonadInstance m-val)]}
     (transform* outer-monad inner-monad m-val)))

(defn lower [m-impl m-val]
  {:pre [(satisfies? MonadInstance m-val)
         (satisfies? MonadTransformable
                     (monad-implementation m-val))
         (satisfies? MonadTransformer m-impl)]
   :post [(partial monad-instance? m-impl)]}
  (transform* (monad-implementation m-val)
              (inner-monad m-impl)
              m-val))

(defn monad-transformers-chain
  [m-impl]
  {:pre [(satisfies? Monad m-impl)]}
  (if (satisfies? MonadTransformer m-impl)
    (conj (monad-transformers-chain (inner-monad m-impl))
          m-impl)
    [m-impl]))

(defn morph-nth
  ([n m-impl m-val]
     (morph-nth n m-impl (monad-implementation m-val) m-val))
  ([n m-impl-a m-impl-b m-val]
     {:pre [(>= n 0)
            (satisfies? MonadTransformer m-impl-a)
            (satisfies? Monad m-impl-b)]
      :post [(monad-instance? m-impl-a %)]}
     (let [{:keys [before found after]}
           (split-with-nth-coll
            n
            (monad-transformers-chain m-impl-b)
            (monad-transformers-chain m-impl-a)
            :compare-with same-base-monad?)]
       (if found
         (cond->> m-val
                  (seq before) (transform (last before))
                  (seq after) (chain-lift after))
         (throw
          (Exception.
           (str "the "
                (class m-impl-b)
                " monad is not present"
                (if (> n 0) (str " at index " n))
                " in the monad chain of m-impl")))))))

(defn morph
  "Transforms a monadic value m-val whose monad is anywhere in the monad
  transformers chain of m-impl into a monadic value of m-impl"
  [m-impl m-val]
  (morph-nth 0 m-impl m-val))
