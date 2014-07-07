(ns moar.utils)

(defn split-with-last [fun coll]
  (let [[after rest] (split-with (complement fun) (reverse coll))
        [[x & equals] before] (split-with fun rest)]
    [(reverse (concat equals before)) x (reverse after)]))
