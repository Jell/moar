(ns moar.monads.state-test
  (:require [clojure.test :refer :all]
            [moar.monads.state :as state]
            [moar.core :refer :all]
            [moar.monads.maybe :as maybe]))

(deftest state-monad
  (let [monad state/monad
        return (partial wrap monad)
        pull (partial state/pull monad)
        push (partial state/push monad)
        modify (partial state/modify monad)]

    (testing "modifying state"
      (is (= (state/->Pair 12 :test)
             ((>> (modify inc)
                  (modify + 5 5)
                  (return :test))
              1))))

    (testing "getting and setting state"
      (is (= (state/->Pair 11 11)
             ((>> (push 10)
                  (modify inc)
                  (pull))
              :ignored))))

    (testing "bindings values"
      (is (= (state/->Pair {:test "val"} 7)
             ((mlet [a (return 1)
                     b (return 2)
                     c (return 4)]
                    (modify assoc :test "val")
                    (return (+ a b c)))
              {}))))))

(deftest state-t-monad
  (let [monad  (-> maybe/monad state/monad-t)
        return (partial wrap monad)
        pull   (partial state/pull monad)
        push   (partial state/push monad)
        modify (partial state/modify monad)]

    (testing "modifying state"
      (is (= (maybe/just (state/->Pair 12 :test))
             ((>> (modify inc)
                  (modify + 5 5)
                  (return :test))
              1))))

    (testing "getting and setting state"
      (is (= (maybe/just (state/->Pair 11 11))
             ((>> (push 10)
                  (modify inc)
                  (pull))
              :ignored))))

    (testing "bindings values"
      (is (= (maybe/just (state/->Pair {:test "val"} 7))
             ((mlet [a (return 1)
                     b (return 2)
                     c (return 4)]
                    (modify assoc :test "val")
                    (return (+ a b c)))
              {}))))

    (testing "lifting and lowering"
      (is (= (maybe/just (state/->Pair :state :value))
             ((return :value) :state)
             ((lower monad (wrap state/monad :value)) :state)
             ((lift monad (wrap maybe/monad :value)) :state))))))
