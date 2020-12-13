(ns advent-of-code-2020.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(def real-input (slurp (io/resource "day12.txt")))

(def test-input "F10
N3
F7
R90
F11")


(def start-state
  {:pos [0 0]
   :heading :E})


(defn read-input
  [input]
  (into []
        (map (fn [line]
               [(keyword (subs line 0 1))
                (Long/valueOf (subs line 1))]))
        (string/split-lines input)))


(def next-left
  {:N :W
   :W :S
   :S :E
   :E :N})


(def next-right
  {:N :E
   :E :S
   :S :W
   :W :N})


(defn update-heading
  [state dir times]
  (if (zero? times)
    state
    (recur (update state
                   :heading
                   (if (= :L dir) next-left next-right))
           dir
           (dec times))))


(def unit-vectors
  {:N [0 -1]
   :E [1 0]
   :W [-1 0]
   :S [0 1]})


(defn vec+
  [vx vy]
  (mapv + vx vy))


(defn vec*
  [v m]
  (mapv (partial * m) v))


(defn update-position
  [state dir mag]
  (update state
          :pos
          #(vec+ % (vec* (get unit-vectors dir) mag))))


(defn exec-instr
  [{:keys [heading] :as state} [op mag]]
  (cond (contains? unit-vectors op) (update-position state op mag)
        (= op :F) (update-position state heading mag)
        :else (update-heading state op (/ mag 90))))


(defn mdist
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))


#_(prn (solve-part1 real-input))
#_(prn (solve-part1 test-input))
(defn solve-part1
  [input]
  (-> (reduce exec-instr start-state (read-input input))
      :pos
      mdist))


(defn move-waypoint
  [state dir mag]
  (update state
          :waypoint
          vec+
          (vec* (get unit-vectors dir) mag)))


(defn rot-left
  [[x y]]
  [y (* -1 x)])


(defn rot-right
  [[x y]]
  [(* -1 y) x])


(defn rotate-waypoint
  [state dir times]
  (if (zero? times)
    state
    (recur (update state :waypoint (if (= :L dir) rot-left rot-right))
           dir
           (dec times))))


(defn move-ship
  [{:keys [waypoint] :as state} mag]
  (update state
          :ship
          vec+
          (vec* waypoint mag)))


(defn exec-instr2
  [state [op mag]]
  (cond (contains? unit-vectors op) (move-waypoint state op mag)
        (contains? #{:L :R} op) (rotate-waypoint state op (/ mag 90))
        :else (move-ship state mag)))


(def start-state2
  {:waypoint [10 -1]
   :ship [0 0]})


#_(prn (solve-part2 test-input))
#_(prn (solve-part2 real-input))
(defn solve-part2
  [input]
  (-> (reduce exec-instr2 start-state2 (read-input input))
      :ship
      mdist))
