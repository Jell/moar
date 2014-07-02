(ns moar.monads.result-test
  (:use [clojure.test]
        [moar.monads.result])
  (:require [moar.core :as m]))

(deftest result-monad
  (testing "threads successfull values"
    (let [r (m/mlet
             [a (result 1)
              b (result 2)]
             (result (+ a b)))]
      (is (success? r))
      (is (= 3 @r))
      (is (= r (result 3)))))

  (testing "short circuit's if a failing value is introdused"
    (let [r (m/mlet
             [a (result 1)
              b (result 2)
              c (fail :some-reason)]
             (result (+ a b)))]
      (is (not (success? r)))
      (is (= :some-reason @r))
      (is (= r (fail :some-reason))))))
