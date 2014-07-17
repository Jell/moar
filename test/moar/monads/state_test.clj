(ns moar.monads.state-test
  (:require [clojure.test :refer :all]
            [moar.monads.state :refer :all]
            [moar.core :refer :all]
            [moar.monads.maybe :as maybe]))

(deftest state-monad
  (testing "modifying state"
    (is (= (->Pair 12 :test)
           ((>> (modify monad inc)
                  (modify monad + 5 5)
                  (wrap monad :test))
            1))))

  (testing "getting and setting state"
    (is (= (->Pair 11 11)
           ((>> (push monad 10)
                  (modify monad inc)
                  (pull monad))
            :ignored))))

  (testing "bindings values"
    (is (= (->Pair {:test "val"} 7)
           ((mlet [a (wrap monad 1)
                     b (wrap monad 2)
                     c (wrap monad 4)]
                    (modify monad assoc :test "val")
                    (wrap monad (+ a b c)))
            {})))))

(deftest state-t-monad
  (let [m (monad-t maybe/monad)]

    (testing "modifying state"
      (is (= (maybe/just (->Pair 12 :test))
             ((>> (modify m inc)
                  (modify m + 5 5)
                  (wrap m :test))
              1))))

    (testing "getting and setting state"
      (is (= (maybe/just (->Pair 11 11))
             ((>> (push m 10)
                  (modify m inc)
                  (pull m))
              :ignored))))

    (testing "bindings values"
      (is (= (maybe/just (->Pair {:test "val"} 7))
             ((mlet [a (wrap m 1)
                     b (wrap m 2)
                     c (wrap m 4)]
                    (modify m assoc :test "val")
                    (wrap m (+ a b c)))
              {}))))))
