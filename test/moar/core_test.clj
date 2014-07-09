(ns moar.core-test
  (:require [clojure.test :refer :all]
            [moar.core :refer :all]
            [moar.protocols :refer :all]
            [moar.monads.maybe :as maybe]
            [moar.monads.maybe-t :refer [maybe-t]]
            [moar.monads.result :as result]
            [moar.monads.result-t :refer [result-t]]
            [moar.monads.sequence :as sequence]
            [moar.monads.id :refer [id]]))

(deftest mlet-tests
  (testing "binding id monads"
    (is (= 9 @(mlet
               [a (id 2)
                b (id 3)
                :let [v (+ a b)]
                c (id 4)]
               (id (+ v c)))))))

(deftest fmap-tests
  (testing "fmapping over id monads"
    (is (= 3 @(fmap inc (id 2))))))

(deftest m-sequence-tests
  (testing "calling sequence across id monads"
    (is (= (m-sequence [(id 1) (id 2)]) (id [1 2])))))

(deftest map-m-tests
  (testing "map-m over id monads"
    (is (= (id [2 3]) (map-m (comp id inc) [1 2])))))

(deftest join-test
  (is (= (join (id (id 1))) (id 1))))

(deftest extract-m-test
  (is (= (id {:a 1 :b 2})
         (extract-m {:a (id 1) :b (id 2)}))))

(deftest basic-lifting
  (let [monad (maybe-t (result-t sequence/monad))
        return (partial wrap monad)]
    (is (= (bind (return 1) (lift-f monad inc))
           (return 2)))))

(deftest advanced-lifting
  (let [monad (monads-stack sequence/monad
                            result/monad
                            maybe/monad)
        return (partial wrap monad)]
    (is (= (return 2)
           (bind (return 1)
                   (lift-f monad (lift-f sequence/monad inc)))))
    (is (= (return 2)
           (lift monad 2)
           (lift monad (list 2))))
    (is (= (return 6)
           ((lift-m monad +) (return 1) (return 2) (return 3))))
    (is (= (lift monad (list 1 2 3))
           ((lift-m monad list)
            (return 1) (return 2) (return 3))))))

(deftest morphing
  (let [monad (maybe-t (result-t sequence/monad))
        return (partial wrap monad)]
    (is (= (return 1)
           (morph monad (result/result 1))
           (morph monad (maybe/just 1))
           (morph monad (list 1))))
    (is (= (return 5)
           (>>= (return 1)
                (morph-f monad #(maybe/just (inc %)))
                (morph-f monad #(result/result (inc %)))
                (morph-f monad #(list (inc %)))
                (lift-f monad inc))))))

(deftest nested-morphing
  (let [monad (maybe-t (maybe-t maybe/monad))
        return (partial wrap monad)]
    (is (= (maybe/just (maybe/just maybe/nothing))
           @@(morph monad maybe/nothing)
           @@(morph-nth 0 monad maybe/nothing)))

    (is (= (maybe/just maybe/nothing)
           @@(morph-nth 1 monad maybe/nothing)))

    (is (= maybe/nothing
           @@(morph-nth 2 monad maybe/nothing))))

  (let [monad (maybe-t (result-t maybe/monad))
        return (partial wrap monad)]
    (is (= (maybe/just (result/result maybe/nothing))
           @@(morph monad maybe/nothing)
           @@(morph-nth 0 monad maybe/nothing)))

    (is (= maybe/nothing
           @@(morph-nth 1 monad maybe/nothing)))))
