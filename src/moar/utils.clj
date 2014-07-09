(ns moar.utils)

(defn split-with-last [fun coll]
  (let [[after rest] (split-with (complement fun) (reverse coll))
        [[x & equals] before] (split-with fun rest)]
    [(reverse (concat equals before)) x (reverse after)]))

(defn split-with-nth-from-last [n fun coll]
  {:pre [(>= n 0)]}
  (let [[before x after] (split-with-last fun coll)]
    (if (zero? n)
      [before x after]
      (let [[before-b x-b after-b]
            (split-with-nth-from-last (dec n) fun before)]
        [before-b x-b (concat after-b (list x) after)]))))
