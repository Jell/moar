(ns moar.monads.maybe-test
  (:require [clojure.test :refer :all]
            [moar.core :refer :all]
            [moar.monads.sequence :as sequence]
            [moar.monads.maybe :as maybe :refer [monad just nothing monad-t]]))

(deftest monadic-laws
  (testing "left identity"
    (are [f a]
      (= (bind (wrap monad a) f) (f a))
      (fn [x] (just x))       'hi
      (fn [x] (just (* 2 x))) 3
      (fn [_] nothing)        :hello))
  (testing "right identity"
    (are [m]
      (= (bind m (partial wrap monad)) m)
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
      nothing  (fn [_] nothing) (fn [_] nothing))))

(deftest maybe-transformer-tests
  (let [monad-t (maybe/monad-t sequence/monad)]
    (is (= (list (just 1))
           @(wrap monad-t 1)))
    (is (= (list (just 2))
           @(bind (wrap monad-t 1)
                  (fn [x] (wrap monad-t (inc x))))))
    (is (= (list (just 3))
           @(>>= (wrap monad-t 1)
                 (fn [x] (wrap monad-t (inc x)))
                 (fn [x] (wrap monad-t (inc x))))))
    (is (= (list (just 4) (just 5) nothing (just 6))
           @(bind (maybe/t monad-t
                           (list (just 3)
                                 (just 4)
                                 nothing
                                 (just 5)))
                  (fn [x] (wrap monad-t (inc x))))))
    (is (= (list (just 2))
           @(fmap inc (wrap monad-t 1))))))
