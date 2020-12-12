(ns advent-of-code-2020.day05
  (:require [advent-of-code-2020.utils :as utils]
            [clojure.set :as sets]))


(defn bin-part
  [start end [i & is]]
  (if (nil? i)
    start
    (let [delta (quot (inc (- end start)) 2)]
      (if (= :upper i)
        (recur (+ start delta) end is)
        (recur start (- end delta) is)))))


(defn read-ln
  [line]
  {:row (map {\F :lower \B :upper} (take 7 line))
   :col (map {\L :lower \R :upper} (drop 7 line ))})


(defn get-seat
  [{:keys [row col]}]
  {:row (bin-part 0 127 row)
   :col (bin-part 0 8 col)})


(defn seat-id
  [{:keys [row col]}]
  (+ (* row 8) col))


(def xform (comp (map read-ln)
                 (map get-seat)
                 (map seat-id)))


#_(prn (solve-part1))
(defn solve-part1
  []
  (reduce max (utils/read-lines xform "day05.txt")))


(defn adj-seats
  [{:keys [row col]}]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :let [x (+ col dx)
              y (+ row dy)]
        :when (and (not= [x y] [col row])
                   (< -1 x 8)
                   (< -1 y 128))]
    {:row y :col x}))


(defn all-adj-taken
  [taken-seats seat]
  (every? #(contains? taken-seats %)
          (adj-seats seat)))


#_(prn (solve-part2))
(defn solve-part2
  []
  (let [taken-seats (set (utils/read-lines (comp (map read-ln)
                                                 (map get-seat))
                                           "day05.txt"))]
    (-> (for [row (range 128)
              col (range 8)
              :let [seat {:row row :col col}]
              :when (and (all-adj-taken taken-seats seat)
                         (not (contains? taken-seats seat)))]
          seat)
        first
        seat-id)))

