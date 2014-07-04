(ns moar.core-test
  (:require [clojure.test :refer :all]
            [moar.core :as m]
            [moar.protocols :refer :all]
            [moar.monads.maybe :as maybe]
            [moar.monads.maybe-t :refer [maybe-t]]
            [moar.monads.sequence :as sequence]
            [moar.monads.id :refer [id]]))

(deftest mlet-tests
  (testing "binding id monads"
    (is (= 9 @(m/mlet
               [a (id 2)
                b (id 3)
                :let [v (+ a b)]
                c (id 4)]
               (id (+ v c)))))))

(deftest fmap-tests
  (testing "fmapping over id monads"
    (is (= 3 @(m/fmap inc (id 2))))))

(deftest m-sequence-tests
  (testing "calling sequence across id monads"
    (is (= (m/m-sequence [(id 1) (id 2)]) (id [1 2])))))

(deftest map-m-tests
  (testing "map-m over id monads"
    (is (= (id [2 3]) (m/map-m (comp id inc) [1 2])))))

(deftest join-test
  (is (= (m/join (id (id 1))) (id 1))))

(deftest extract-m-test
  (is (= (id {:a 1 :b 2})
         (m/extract-m {:a (id 1) :b (id 2)}))))

(deftest basic-lifting
  (let [monad (maybe-t (maybe-t sequence/monad))
        return (partial m/wrap monad)]
    (is (= (m/bind (return 1) (m/lift monad inc))
           (return 2)))))

(deftest advanced-lifting
  (let [monad (maybe-t (maybe-t sequence/monad))
        return (partial m/wrap monad)]
    (is (= (return 2)
           (m/bind (return 1)
                   (m/lift monad (m/lift sequence/monad inc)))))
    (is (= (return 2)
           (m/lift-value monad 2)
           (m/lift-value monad (list 2))))))
