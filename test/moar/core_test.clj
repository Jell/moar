(ns moar.core-test
  (:require [clojure.test :refer :all]
            [moar.core :as m]
            [moar.monads.state :as state]))

(deftest mlet-tests
  (= [{:test "val"} 7]
     (state/run-state (m/mlet
                       [a (state/state-v 1)
                        b (state/state-v 2)
                        :let [v (+ a b)]
                        c (state/state-v 4)]
                       (state/mod-state assoc :test "val")
                       (state/state-v (+ v c)))
                      {})))
