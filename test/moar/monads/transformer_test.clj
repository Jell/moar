(ns moar.monads.transformer-test
  (:require [clojure.test :refer :all]
            [moar.core :refer :all]
            [moar.monads.transformer :refer :all]
            [moar.monads.state  :as state]
            [moar.monads.maybe  :as maybe]
            [moar.monads.result :as result :refer [result fail]]))

(def state-maybe  (transform-impl [state/monad maybe/monad]))
(def state-result (transform-impl [state/monad result/monad]))

(deftest transformer-lifting
  (testing "basic wrapping unwrapping"
    (is (= (state/run-state @(wrap state-maybe 1) 1)
           [(maybe/just 1) 1])))

  (testing "basic bind call"
    (is (= (state/run-state @(fmap inc (wrap state-maybe 1)) 1)
           [(maybe/just 2) 1])))

  (testing "can lift state side effects into maybe monad"
    (let [return (partial wrap state-maybe)
          transformer (mlet [a (return 1)
                             b (lift state-maybe (>> (state/mod-state inc)
                                                     (state/state-v 2)))
                             c (mzero state-maybe) ;; short circuit's here
                             d (lift state-maybe (>> (state/mod-state inc) (state/state-v 4)))]
                            (return (+ a b c d)))
          res (state/run-state @transformer 5)]
      (is (= res [maybe/nothing 6]))))

  (testing "can lift state side effects into result monad"
    (let [return (partial wrap state-result)
          transformer (mlet [a (return 1)
                             b (lift state-result (>> (state/mod-state inc)
                                                      (state/state-v 2)))
                             x (lower state-result (fail (+ a b)))]
                            (return "newer runned"))
          res (state/run-state @transformer 5)]
      (is (= res [(result/fail 3) 6])))))
