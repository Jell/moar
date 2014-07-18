(ns moar.monads.continuation-test
  (:require [clojure.test :refer :all]
            [moar.monads.continuation :as continuation]
            [moar.monads.maybe :as maybe]
            [moar.monads.state :as state]
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

(deftest continuation-transformer
  (let [monad  (continuation/monad-t state/monad)
        return (partial wrap monad)
        callcc (partial continuation/callcc monad)
        modify (comp (partial lift monad)
                     (partial state/modify state/monad))
        pull   (comp (partial lift monad)
                     (partial state/pull state/monad))]

    (testing "stateful loops!"
      (let [my-loop
            (>> (callcc (fn [cont]
                          (modify assoc :next
                                  (partial cont nil))))
                (modify update-in [:count] inc)
                (mlet [n    (pull :count)
                       next (pull :next)]
                      (modify update-in [:log] conj n)
                      (if (< n 5)
                        (next)
                        (>> (modify dissoc :next)
                            (return ::done)))))]

        (is (= (state/->Pair {:count 5 :log [1 2 3 4 5]} ::done)
               ((my-loop (partial wrap state/monad))
                {:count 0 :log []})))))))
