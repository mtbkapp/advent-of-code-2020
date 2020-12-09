(ns advent-of-code-2020.day9
  (:require [advent-of-code-2020.utils :as utils]))


(defn group-input
  [input preamble-size]
  (->> input
       (partition preamble-size 1)
       (partition 2 1)
       (map (juxt (comp vec first) (comp last second)))))


(defn two-combos
  [xs]
  (mapcat (comp (fn [[x & xs]]
                  (map (fn [y] [x y]) xs))
                (fn [n] (drop n xs)))
          (range (dec (count xs)))))


(defn valid?
  [[g sum]]
  (some (fn [[x y]]
          (= sum (+ x y))
          )
        (two-combos g)))


(def real-input (utils/read-lines (map #(Long/valueOf %)) "day9.txt"))
(def test-input [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

#_(prn (solve-part1 real-input 25))
#_(solve-part1 test-input 5)
(defn solve-part1
  [input preamble-size]
  (loop [[g & gs] (group-input input preamble-size)]
    (when (some? g)
      (if (valid? g)
        (recur gs)
        (second g)))))


(defn all-contiguous
  [input]
  (sequence (mapcat #(partition % 1 input))
            (range 2 (inc (count input)))))


#_(prn (solve-part2 real-input (solve-part1 real-input 25)))
#_(solve-part2 test-input (solve-part1 test-input 5))
(defn solve-part2
  [input part1]
  (if-let [cs (some #(if (= part1 (apply + %)) %)
                    (all-contiguous input))]
    (+ (reduce min cs) (reduce max cs))))

