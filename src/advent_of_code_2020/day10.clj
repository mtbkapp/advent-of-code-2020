(ns advent-of-code-2020.day10
  (:require [advent-of-code-2020.utils :as utils]))


(def test-input-1 [16 10 15 5 1 11 7 19 6 12 4])
(def test-input-2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32
                   25 35 8 17 7 9 4 2 34 10 3])
(def input (utils/read-lines (map #(Long/valueOf %)) "day10.txt"))


(defn add-ends
  [xs]
  (into xs [0 (+ 3 (last xs))]))

(defn solve-part-1*
  [adapters]
  (->> input
       (apply sorted-set)
       add-ends
       (partition 2 1)
       (map (fn [[x y]] (- y x)))
       frequencies))


#_(prn (solve-part-1 input))
(defn solve-part-1
  [adapters]
  (let [{ones 1 threes 3} (solve-part-1* adapters)]
    (* ones threes)))
