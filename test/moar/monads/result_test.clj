(ns moar.monads.result-test
  (:use [clojure.test]
        [moar.monads.result])
  (:require [moar.core :refer :all]
            [moar.monads.state :as state]))

(deftest result-monad
  (testing "threads successfull values"
    (let [r (mlet
             [a (result 1)
              b (result 2)]
             (result (+ a b)))]
      (is (success? r))
      (is (= 3 @r))
      (is (= r (result 3)))))

  (testing "short circuit's if a failing value is introdused"
    (let [r (mlet
             [a (result 1)
              b (result 2)
              c (fail :some-reason)]
             (result (+ a b)))]
      (is (not (success? r)))
      (is (= :some-reason @r))
      (is (= r (fail :some-reason))))))

(def result-state (monad-t state/monad))

(deftest result-monad-transformer-test
  (testing "basic wrapping/unwrapping"
    (is (= (state/->Pair 1 (result 1))
           (@(wrap result-state 1) 1))))

  (testing "basic bind call"
    (is (= (state/->Pair 1 (result 2))
           (@(fmap inc (wrap result-state 1)) 1))))

  (testing "can lift state side effects into result monad"
    (let [return (partial wrap result-state)
          trans (mlet [a (return 1)
                       b (lift result-state (>> (state/modify state/monad inc)
                                                (wrap state/monad 2)))
                       x (lower result-state (fail (+ a b)))]
                      (return "never runned"))
          res (@trans 5)]
      (is (= (state/->Pair 6 (fail 3)) res)))))
