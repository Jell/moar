(ns moar.monads.sequence-test
  (:require [clojure.test :refer :all]
            [moar.core :refer :all]
            [moar.monads.sequence :refer [monad]]))

(deftest monadic-laws
  (testing "left identity"
    (are [f a]
      (= (bind (wrap monad a) f) (f a))
      (fn [x] (list x))       'hi
      (fn [x] (list x x))     'hi
      (fn [x] (list (* 2 x))) 3
      (fn [_] '())        :hello))
  (testing "right identity"
    (are [m]
      (= (bind m (partial wrap monad)) m)
      (list)
      (list 'hi)
      (list nil)
      (list 1 2 3)))
  (testing "associativity"
    (are [m f g]
      (= (bind (bind m f) g)
         (bind m (fn [x] (bind (f x) g))))
      (list 5)     #(list (inc %) %)  #(list (* 2 %))
      (list 1 2 3) #(list (inc %) %)  #(list (* 2 %))
      (list 1 2 3) list               #(list (* 2 %))
      (list)       #(list (inc %) %)  #(list (* 2 %))
      (list 5)     (fn [_] (list))    #(list (* 2 %))
      (list 5)     #(list (* 2 %))    (fn [_] (list))
      (list 5)     (fn [_] (list))    (fn [_] (list))
      (list)       (fn [_] (list))    (fn [_] (list)))))
