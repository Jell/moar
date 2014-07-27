(ns moar.utils
  (:require [net.cgrand.seqexp :as se]))

(defn split-with-coll
  [sub-coll coll & {:keys [compare-with]}]
  (let [comp-fn (or compare-with =)]
    (-> (se/exec
         (se/cat (se/as :before (se/*? (constantly true)))
                 (se/as :found (apply se/cat
                                  (map (partial partial comp-fn)
                                       sub-coll)))
                 (se/as :after (se/*? (constantly true))))
         coll)
        (select-keys [:found :before :after]))))

(defn split-with-nth-coll
  [n sub-coll coll & {:keys [compare-with]}]
  {:pre [(>= n 0)]}
  (let [{:keys [before found after] :as result}
        (split-with-coll sub-coll coll :compare-with compare-with)]
    (if (zero? n)
      result
      (let [{before-b :before found-b :found after-b :after}
            (split-with-nth-coll (dec n) sub-coll after
                                 :compare-with compare-with)]
        {:before (concat before found before-b)
         :found found-b
         :after after-b}))))
