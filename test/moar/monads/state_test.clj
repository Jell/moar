(ns moar.monads.state-test
  (:use [clojure.test]
        [moar.monads.state])
  (:require [moar.core :as m]))

(deftest state-monad

  (testing "modifying state"
    (is (= [12 :test]
           (run-state (m/>>
                       (mod-state inc)
                       (mod-state + 5 5)
                       (state-v :test))
                      1))))

  (testing "getting and setting state"
    (is (= [11 11]
           (run-state (m/>>
                       (set-state 10)
                       (mod-state inc)
                       get-state)
                      :ingored))))

  (testing "bindings values"
    (is (= [{:test "val"} 7]
           (run-state (m/mlet
                             [a (state-v 1)
                              b (state-v 2)
                              c (state-v 4)]
                             (mod-state assoc :test "val")
                             (state-v (+ a b c)))
                            {})))))
