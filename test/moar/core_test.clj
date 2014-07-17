(ns moar.core-test
  (:require [clojure.test :refer :all]
            [moar.core :refer :all]
            [moar.protocols :refer :all]
            [moar.monads.maybe :as maybe]
            [moar.monads.result :as result]
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
