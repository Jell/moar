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
                (fn [x] (wrap monad (inc x))))
         (list (just 2))))
  (is (= @(>>= (wrap monad 1)
               (fn [x] (wrap monad (inc x)))
               (fn [x] (wrap monad (inc x))))
         (list (just 3))))
  (is (= @(bind (run-maybe-t sequence/monad
                             (list (just 3)
                                   (just 4)
                                   nothing
                                   (just 5)))
                (fn [x] (wrap monad (inc x))))
         (list (just 4) (just 5) nothing (just 6)))))
