(ns moar.monads.result-test
  (:require [clojure.test :refer :all]
            [moar.core :refer :all]
            [moar.monads.result :as result :refer [success fail success?]]
            [moar.monads.state :as state]))

(deftest result-monad
  (testing "threads successfull values"
    (let [r (mlet
             [a (success 1)
              b (success 2)]
             (success (+ a b)))]
      (is (success? r))
      (is (= 3 @r))
      (is (= r (success 3)))))

  (testing "short circuits if a failing value is introduced"
    (let [r (mlet
             [a (success 1)
              b (success 2)
              c (fail :some-reason)]
             (success (+ a b)))]
      (is (not (success? r)))
      (is (= :some-reason @r))
      (is (= r (fail :some-reason))))))

(deftest result-monad-transformer-test
  (let [monad (-> state/monad result/monad-t)
        return (partial wrap monad)]

    (testing "basic wrapping/unwrapping"
      (is (= (state/->Pair 1 (success 1))
             (@(return 1) 1))))

    (testing "basic bind call"
      (is (= (state/->Pair 1 (success 2))
             (@(fmap inc (return 1)) 1))))

    (testing "can lift state side effects into result monad"
      (let [trans (mlet [a (return 1)
                         b (lift monad (>> (state/modify state/monad inc)
                                           (wrap state/monad 2)))
                         x (lower monad (fail (+ a b)))]
                        (return "never runned"))
            res (@trans 5)]
        (is (= (state/->Pair 6 (fail 3)) res))))))
