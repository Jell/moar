(ns moar.monads.maybe-t-test
  (:require [clojure.test :refer :all]
            [moar.core :refer :all]
            [moar.monads.sequence :as sequence]
            [moar.monads.maybe :refer [just nothing]]
            [moar.monads.maybe-t :refer [maybe-t run-maybe-t]]))

(def monad (maybe-t sequence/monad))

(deftest quick-testing
  (is (= @(wrap monad 1)
         (list (just 1))))
  (is (= @(bind (wrap monad 1)
                (fn [x] (list (just (inc x)))))
         (list (just 2))))
  (is (= @(>>= (wrap monad 1)
               (fn [x] (list (just (inc x))))
               (fn [x] (list (just (inc x)))))
         (list (just 3)))))
