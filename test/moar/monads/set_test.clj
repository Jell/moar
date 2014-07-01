(ns moar.monads.set-test
  (:require [clojure.test :refer :all]
            [moar.core :refer :all]
            [moar.monads.set :refer [monad]]))

(deftest monadic-laws
  (testing "left identity"
    (are [f a]
      (= (bind (wrap monad a) f) (f a))
      (fn [x] #{x})         'hi
      (fn [x] #{x (* 2 x)}) 3
      (fn [_] #{})          :hello))
  (testing "right identity"
    (are [m]
      (= (bind m (partial wrap monad)) m)
      #{}
      #{'hi}
      #{nil}
      #{1 2 3}))
  (testing "associativity"
    (are [m f g]
      (= (bind (bind m f) g)
         (bind m (fn [x] (bind (f x) g))))
      #{5}     (fn [x] #{(inc x) x})  (fn [x] #{(* 2 x)})
      #{1 2 3} (fn [x] #{(inc x) x})  (fn [x] #{(* 2 x) x})
      #{1 2 3} (fn [_] #{})           (fn [x] #{(* 2 x)})
      #{}      (fn [x] #{(inc x) x})  (fn [x] #{(* 2 x)})
      #{5}     (fn [_] #{})           (fn [x] #{(* 2 x)})
      #{5}     (fn [x] #{(* 2 x) x})  (fn [_] #{})
      #{5}     (fn [_] #{})           (fn [_] #{})
      #{}      (fn [_] #{})           (fn [_] #{}))))
