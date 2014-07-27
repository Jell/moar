(ns moar.monads.state-test
  (:require [clojure.test :refer :all]
            [moar.monads.state :as state]
            [moar.monads.result :as result]
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

(deftest nested-state-transformer-test
  (let [monad (-> result/monad
                  state/monad-t
                  state/monad-t)
        return (partial wrap monad)
        pull   (partial state/pull state/monad)
        modify (partial state/modify state/monad)

        pull-a   (comp (partial morph-nth 0 monad) pull)
        modify-a (comp (partial morph-nth 0 monad) modify)
        pull-b   (comp (partial morph-nth 1 monad) pull)
        modify-b (comp (partial morph-nth 1 monad) modify)]

    (is (= (result/success
            (state/->Pair {:x 3}
                          (state/->Pair {:y 2} 5)))
           (((>> (modify-a update-in [:x] inc)
                 (modify-b update-in [:y] inc)
                 (mlet [a (pull-a :x)
                        b (pull-b :y)]
                       (return (+ a b))))
             {:y 1})
            {:x 2})))))
