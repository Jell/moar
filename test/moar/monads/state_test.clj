(ns moar.monads.state-test
  (:use [clojure.test]
        [moar.monads.state])
  (:require [moar.core :as m]))

(deftest modification-to-state
  (is (= [12 :test]
         (run-state (m/>>
                     (mod-state inc)
                     (mod-state + 5 5)
                     (state-v :test))
                    1))))

(deftest get-set-state
  (is (= [11 11]
         (run-state (m/>>
                     (set-state 10)
                     (mod-state inc)
                     get-state)
                    :ingored))))
