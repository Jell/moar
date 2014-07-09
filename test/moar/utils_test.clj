(ns moar.utils-test
  (:require [clojure.test :refer :all]
            [moar.utils :refer :all]))

(deftest split-with-last-test
  (is (= '((1 :a 1 2) :a (1 2 3))
         (split-with-last (partial = :a)
                          [1 :a 1 2 :a 1 2 3])))
  (is (= '(() :a (1 2 3))
         (split-with-last (partial = :a)
                          [:a 1 2 3])))
  (is (= '((:a) :a (1 2 3))
         (split-with-last (partial = :a)
                          [:a :a 1 2 3])))
  (is (= '(() nil (1 2 3))
         (split-with-last (partial = :a)
                          [1 2 3])))
  (is (= '((1 2 3) :a ())
         (split-with-last (partial = :a)
                          [1 2 3 :a]))))

(deftest split-with-nth-from-last-test
  (let [example (list :a 1 :a 1 2 :a 1 2 3 :a)
        pred (partial = :a)]
    (is (= '((:a 1 :a 1 2 :a 1 2 3) :a ())
           (split-with-nth-from-last 0 pred example)))
    (is (= '((:a 1 :a 1 2) :a (1 2 3 :a))
           (split-with-nth-from-last 1 pred example)))
    (is (= '((:a 1) :a (1 2 :a 1 2 3 :a))
           (split-with-nth-from-last 2 pred example)))
    (is (= '(() :a (1 :a 1 2 :a 1 2 3 :a))
           (split-with-nth-from-last 3 pred example)))
    (is (= '(() nil (:a 1 :a 1 2 :a 1 2 3 :a))
           (split-with-nth-from-last 4 pred example)))))
