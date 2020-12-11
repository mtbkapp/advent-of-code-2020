(ns advent-of-code-2020.day10
  (:require [advent-of-code-2020.utils :as utils]
            [clojure.test :refer :all]))


(def test-input-1 [16 10 15 5 1 11 7 19 6 12 4])
(def test-input-2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32
                   25 35 8 17 7 9 4 2 34 10 3])
(def input (utils/read-lines (map #(Long/valueOf %)) "day10.txt"))


(defn add-ends
  [ss]
  (into ss [0 (+ 3 (last ss))]))

(defn solve-part-1*
  [adapters]
  (->> adapters
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


(defn init 
  [input]
  (let [mx (reduce max input)]
    (sort (into input [0 (+ mx 3)]))))


(defn next-adapters
  [[a :as adapters]]
  (filter (fn [[x :as na]]
            (if (not-empty na)
              (<= 1 (- x a) 3)))
          (map #(drop % adapters) [1 2 3])))


(declare count-ways)

(defn count-ways*
  [input]
  (if (< (count input) 2)
    1
    (reduce + (map count-ways (next-adapters input)))))

(def count-ways (memoize count-ways*))


#_(prn (solve-part2 test-input-1))
#_(prn (solve-part2 test-input-2))
#_(prn (time (solve-part2 input)))
(defn solve-part2
  [input]
  (count-ways (init input)))

