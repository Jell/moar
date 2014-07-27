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

(deftest morphing
  (let [monad (-> sequence/monad
                  result/monad-t
                  maybe/monad-t)
        return (partial wrap monad)]
    (testing "Morphing simple monads"
      (is (= (return 1)
             (morph monad (result/success 1))
             (morph monad (maybe/just 1))
             (morph monad (list 1)))))
    (testing "Morphing monad transformers"
      (is (= (return 1)
             (morph monad (wrap (-> result/monad maybe/monad-t) 1))
             (morph monad (wrap (-> sequence/monad result/monad-t) 1)))))))

(deftest nested-morphing
  (testing "Morphing nested simple monads"
    (let [monad (-> maybe/monad
                    maybe/monad-t
                    maybe/monad-t)
          return (partial wrap monad)]
      (is (= (return 1)
             (morph-nth 0 monad (maybe/just 1))
             (morph-nth 1 monad (maybe/just 1))
             (morph-nth 2 monad (maybe/just 1))))
      (is (= maybe/nothing
             @@(morph-nth 0 monad maybe/nothing)))
      (is (= (maybe/just maybe/nothing)
             @@(morph-nth 1 monad maybe/nothing)))
      (is (= (maybe/just (maybe/just maybe/nothing))
             @@(morph-nth 2 monad maybe/nothing)))))
  (testing "Morphing nested monad transformers (WHY?!)"
    (let [monad (-> maybe/monad
                    result/monad-t
                    maybe/monad-t
                    result/monad-t)
          sub-monad (-> maybe/monad
                        result/monad-t)]

      (is (= maybe/nothing
             @@@(morph-nth 0 monad (morph sub-monad maybe/nothing))))
      (is (= (maybe/just (result/success maybe/nothing))
             @@@(morph-nth 1 monad (morph sub-monad maybe/nothing)))))))
