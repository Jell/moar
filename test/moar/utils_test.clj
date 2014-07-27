(ns moar.utils-test
  (:require [clojure.test :refer :all]
            [moar.utils :refer :all]))

(deftest split-with-coll-test
  (is (= {:before '(1)
          :found '(:a :b)
          :after '(1 2 :a :b 1 2 3)}
         (split-with-coll [:a :b]
                          '(1 :a :b 1 2 :a :b 1 2 3))))
  (testing "given compare-with"
    (is (= {:before '(1 2 15)
            :found '(15 25)
            :after '(15 25)}
           (split-with-coll [10 20]
                            '(1 2 15 15 25 15 25)
                            :compare-with <)))))

(deftest split-with-nth-coll-test
  (is (= {:before '(1)
          :found '(:a :b)
          :after '(1 2 :a :b 1 2 3)}
         (split-with-nth-coll 0 [:a :b]
                          '(1 :a :b 1 2 :a :b 1 2 3))))
  (is (= {:before '(1 :a :b 1 2)
          :found '(:a :b)
          :after '(1 2 3)}
         (split-with-nth-coll 1 [:a :b]
                              '(1 :a :b 1 2 :a :b 1 2 3))))
  (is (= {:before '(1 :a :b 1 2 :a :b)
          :found nil
          :after nil}
         (split-with-nth-coll 2 [:a :b]
                              '(1 :a :b 1 2 :a :b 1 2 3)))))
