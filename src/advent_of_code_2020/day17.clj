(ns advent-of-code-2020.day17
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(def test-input
  ".#.
..#
###")

(def real-input (slurp (io/resource "day17.txt")))


(defn read-input
  [input dims]
  (-> #{}
      (into (comp (map-indexed 
                    (fn [y row]
                      (map-indexed 
                        (fn [x c]
                          (if (= \# c)
                            (into [x y] (repeat (- dims 2) 0))))
                        row)))
                  cat)
            (string/split-lines input))
      (disj nil)))


(defn adjacent-3d
  [cells]
  (for [[x y z] cells
        dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        :when (not (and (zero? dx)
                        (zero? dy)
                        (zero? dz)))]
    [(+ x dx) (+ y dy) (+ z dz)]))


(defn next-state
  [find-adj cells]
  (reduce (fn [alive [adj freq]]
            (if (or (and (contains? cells adj) (<= 2 freq 3))
                    (and (not (contains? cells adj)) (= 3 freq)))
              (conj alive adj)
              alive))
          #{}
          (frequencies (find-adj cells))))


(defn nth-state
  [start n find-adj]
  (->> (iterate (partial next-state find-adj) start)
       (drop n)
       (first)))

#_(prn (solve-part1 test-input))
#_(prn (solve-part1 real-input))
(defn solve-part1
  [input]
  (count (nth-state (read-input input 3) 6 adjacent-3d)))


(defn adjacent-4d
  [cells]
  (for [[x y z w] cells
        dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        dw [-1 0 1]
        :when (not (and (zero? dx)
                        (zero? dy)
                        (zero? dz)
                        (zero? dw)))]
    [(+ x dx) (+ y dy) (+ z dz) (+ w dw)]))


#_(prn (solve-part2 test-input))
#_(prn (solve-part2 real-input))
(defn solve-part2
  [input]
  (count (nth-state (read-input input 4) 6 adjacent-4d)))
