(ns moar.core-test
  (:require [clojure.test :refer :all]
            [moar.core :as m]
            [moar.monads.id :refer [id]]))

(deftest mlet-tests
  (testing "binding id monads"
    (is (= 9 @(m/mlet
               [a (id 2)
                b (id 3)
                :let [v (+ a b)]
                c (id 4)]
               (id (+ v c)))))))
