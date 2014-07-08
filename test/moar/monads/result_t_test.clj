(ns moar.monads.result-t-test
  (:require [clojure.test :refer :all]
            [moar.core :refer :all]
            [moar.monads.state :as state]
            [moar.monads.result :refer [result fail]]
            [moar.monads.result-t :refer [result-t]]))

(def result-state (result-t state/monad))

(deftest quick-testing
  (testing "basic wrapping/unwrapping"
    (is (= (state/run-state @(wrap result-state 1) 1)
           [(result 1) 1])))

  (testing "basic bind call"
    (is (= (state/run-state @(fmap inc (wrap result-state 1)) 1)
           [(result 2) 1])))

  (testing "can lift state side effects into result monad"
    (let [return (partial wrap result-state)
          trans (mlet [a (return 1)
                       b (morph result-state (>> (state/mod-state inc)
                                                 (state/state-v 2)))
                       x (morph result-state (fail (+ a b)))]
                      (return "newer runned"))
          res (state/run-state @trans 5)]
      (is (= res [(fail 3) 6])))))
