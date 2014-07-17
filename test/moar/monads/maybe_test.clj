(ns moar.monads.maybe-test
  (:require [clojure.test :refer :all]
            [moar.core :refer :all]
            [moar.monads.sequence :as sequence]
            [moar.monads.maybe :as maybe :refer [just nothing]]))

(deftest monadic-laws
  (let [monad maybe/monad
        return (partial wrap monad)]

    (testing "left identity"
      (are [f a]
        (= (bind (return a) f) (f a))
        (fn [x] (just x))       'hi
        (fn [x] (just (* 2 x))) 3
        (fn [_] nothing)        :hello))

    (testing "right identity"
      (are [m]
        (= (bind m return) m)
        (just 'hi)
        (just 5)
        (just nil)
        nothing))

    (testing "associativity"
      (are [m f g]
        (= (bind (bind m f) g)
           (bind m (fn [x] (bind (f x) g))))
        (just 5) #(just (inc %))  #(just (* 2 %))
        nothing  #(just (inc %))  #(just (* 2 %))
        (just 5) (fn [_] nothing) #(just (* 2 %))
        (just 5) #(just (* 2 %))  (fn [_] nothing)
        (just 5) (fn [_] nothing) (fn [_] nothing)
        nothing  (fn [_] nothing) (fn [_] nothing)))))

(deftest maybe-transformer-tests
  (let [monad (-> sequence/monad maybe/monad-t)
        return (partial wrap monad)]

    (is (= (list (just 1))
           @(return 1)))

    (is (= (list (just 2))
           @(bind (return 1) (comp return inc))))

    (is (= (list (just 3))
           @(>>= (return 1)
                 (comp return inc)
                 (comp return inc))))

    (is (= (list (just 4) (just 5) nothing (just 6))
           @(bind (maybe/t monad
                           (list (just 3)
                                 (just 4)
                                 nothing
                                 (just 5)))
                  (comp return inc))))

    (is (= (list (just 2))
           @(fmap inc (return 1))))))
