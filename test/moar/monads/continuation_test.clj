(ns moar.monads.continuation-test
  (:require [clojure.test :refer :all]
            [moar.monads.continuation :as continuation]
            [moar.core :refer :all]))

(deftest continuation-tests
  (let [monad continuation/monad
        return (partial wrap monad)
        callcc (partial continuation/callcc monad)]

    (testing "can compose continuations"
      (let [c (mlet
               [a (return 1)
                b (return 2)]
               (return (+ a b)))]
        (is (= 3 (c identity)))))

    (testing "can use callcc"
      (let [result
            ((mlet
              [x (callcc (fn [cont] (return cont)))]
              (x (fn [_] (return ::val))))
             identity)]
        (is (= result ::val))))))
