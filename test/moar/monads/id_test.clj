(ns moar.monads.id-test
  (:use [clojure.test]
        [moar.monads.id])
  (:require [moar.core :as m]))

(deftest id-tests
  (is (= 9
         @(m/mlet
           [a (id 2)
            b (id 3)
            c (id 4)]
           (id (+ a b c))))))
