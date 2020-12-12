(ns advent-of-code-2020.day03
  (:require [advent-of-code-2020.utils :as utils]))


(defn read-input
  []
  (let [m (utils/read-lines (comp (map vec)) "day03.txt")]
    {:width (count (first m))
     :height (count m)
     :map m}))


(defn vec+
  [a b]
  (mapv + a b))


(defn wrap-x
  [[x y] w]
  [(mod x w) y])


(def tree \#)


(defn count-trees
  [slope]
  (let [{w :width h :height m :map} (read-input)]
    (loop [[x y :as pos] [0 0]
           tree-count 0]
      (if (> y h)
        tree-count
        (recur (wrap-x (vec+ pos slope) w)
               (if (= tree (get-in m [y x]))
                 (inc tree-count)
                 tree-count))))))


#_(prn (solve-part1))
(defn solve-part1
  []
  (count-trees [3 1]))


#_(prn (solve-part2))
(defn solve-part2
  []
  (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
       (map count-trees)
       (reduce *)))

