(ns moar.monads.continuation-test
  (:use [clojure.test]
        [moar.monads.continuation])
  (:require [moar.core :as m]))

(deftest continuation-tests

  (testing "can compose continuations"
    (let [c (m/mlet
             [a (continue-with 1)
              b (continue-with 2)]
             (continue-with (+ a b)))]
      (is (= 3 (call-continuation c identity)))))

  (testing "can use callcc"
    (let [result
          (call-continuation
           (m/mlet
            [x (callcc (fn [cont] (continue-with cont)))]
            (x (fn [_] (continue-with ::val))))
           identity)]
      (is (= result ::val)))))
