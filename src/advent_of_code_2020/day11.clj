(ns advent-of-code-2020.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(def test-input 
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")


(def real-input (slurp (io/resource "day11.txt")))

(defn init
  [input]
  (let [lines (string/split-lines input)
        seats (transduce
                (comp (map-indexed (fn [y line] 
                                     (map-indexed (fn [x c] [x y c]) line))) 
                      cat)
                (completing
                  (fn [acc [x y c]]
                    (cond (= \L c) (assoc acc [x y] false)
                          (= \# c) (assoc acc [x y] true)
                          :else acc)))
                {}
                lines)]
    {:seats seats
     :height (count lines)
     :width (count (first lines))}))


(defn adj-seats
  [{:keys [seats] :as state} [x y :as seat]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :let [nx (+ x dx)
              ny (+ y dy)
              np [nx ny]]
        :when (and (not= np seat)
                   (contains? seats np))]
    np))


(defn count-adj-occupied
  [{:keys [seats] :as state} seat]
  (frequencies (map seats (adj-seats state seat))))


(defn next-state-fn
  [occupied-counter upper-bound]
  (fn [state]
    (update state
            :seats
            #(reduce (fn [next-seats [seat curr-state]]
                       (let [occupied (get (occupied-counter state seat) true 0)]
                         (assoc next-seats
                                seat 
                                (cond (and (not curr-state)
                                           (zero? occupied)) true
                                      (and curr-state
                                           (<= upper-bound occupied)) false
                                      :else curr-state))))
                     {}
                     %))))


(defn count-occupied
  [{:keys [seats]}]
  (count (filter val seats)))


(defn stabilize
  [state xform]
  (let [nxt (xform state)]
    (if (= state nxt)
      nxt
      (recur nxt xform))))


#_(prn (solve-part1 test-input))
#_(prn (solve-part1 real-input))
(defn solve-part1
  [input]
  (count-occupied (stabilize (init input) (next-state-fn count-adj-occupied 4))))


(defn vec*
  [mag v]
  (mapv (partial * mag) v))


(defn vec+
  [vx vy]
  (mapv + vx vy))


(defn in-bounds?
  [[x y] [width height]]
  (and (< -1 x width)
       (< -1 y height)))


(defn occupied-in-dir?
  "Returns
    true - if there is a seat in the given direction that is occupied
    false - if there is a seat in the given direction that is not occupied
    nil - if there is no seat in the given direction
  no this isn't the best api"
  [{:keys [seats width height]} seat dir]
  (loop [mag 1]
    (let [pos (vec+ seat (vec* mag dir))]
      (if (in-bounds? pos [width height])
        (if (contains? seats pos)
          (get seats pos)
          (recur (inc mag)))))))


(def dirs (for [dx [-1 0 1]
                dy [-1 0 1]
                :when (not (and (zero? dx) (zero? dy)))]
            [dx dy]))


(defn seen-from
  [state seat]
  (frequencies
    (into []
          (comp (map #(occupied-in-dir? state seat %))
                (remove nil?))
          dirs)))


#_(prn (solve-part2 test-input))
#_(time (prn (solve-part2 real-input)))
(defn solve-part2
  [input]
  (count-occupied (stabilize (init input) (next-state-fn seen-from 5))))

