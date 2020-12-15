(ns advent-of-code-2020.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(def real-input (slurp (io/resource "day13.txt")))
(def test-input "939
7,13,x,x,59,x,31,19\n")


(defn read-input
  [input] 
  (let [[top bottom] (string/split-lines input)]
    {:start (Long/valueOf top)
     :buses (->> (string/split bottom #",")
                 (remove (partial = "x"))
                 (map #(Long/valueOf %)))}))

(defn departs?
  [bus t]
  (zero? (mod t bus)))


(defn find-first
  [start buses]
  (let [b (some (fn [bus]
                  (if (departs? bus start)
                    bus))
                buses)]
    (if (some? b)
      [start b]
      (recur (inc start) buses))))


#_(prn (solve-part1 test-input))
#_(prn (solve-part1 real-input))
(defn solve-part1
  [input]
  (let [{:keys [start buses]} (read-input input)
        [t bus] (find-first start buses)]
    (* bus (- t start))))


; part 2
;
; find min t such that:
; t > 939
; (t + 0) % 7 = 0
; (t + 1) % 13 = 0
; (t + 4) % 59 = 0
; (t + 6) % 31 = 0
; (t + 7) % 19 = 0

; since 59 is the highest, adjust equations so only multiples of 59 are tested

; t > 939 + 4
; (t - 4) % 7 = 0
; (t - 3) % 13 = 0
; (t + 0) % 59 = 0
; (t + 2) % 31 = 0
; (t + 3) % 19 = 0

(mod (+ 939 (- 59 54)) 59)


; if too slow maybe something can be done with gcd 

#_(prn (read-input2 test-input))
#_(prn (read-input2 real-input))
(defn read-input2
  [input]
  (let [[top bottom] (string/split-lines input)
        buses (->> (map-indexed (fn [i bus]
                                  [i (if (not= "x" bus) (Long/valueOf bus))]
                                  )
                                (string/split bottom #","))
                   (filter (comp some? second)))]
    {:start (Long/valueOf top)
     :buses buses
     :max-bus (reduce (fn [[_ x :as a] [_ y :as b]]
                        (if (and (some? y) (< x y))
                          b
                          a))
                      buses)}))

(defn find-start
  [min-t max-offset max-id]
  (let [adjust-min-t (+ min-t max-offset)]
    (+ adjust-min-t (- max-id (mod adjust-min-t max-id)))))


(defn adjust-offsets
  [max-offset buses]
  (map (fn [[offset bus-id]]
         [(- offset max-offset) bus-id])
       buses))


(defn good-t-for-bus?
  [[offset bus-id :as bus] t]
  (zero? (mod (+ t offset) bus-id)))


#_(prn (solve-part2 real-input))
#_(prn (solve-part2 test-input))
(defn solve-part2
  [input]
  (let [{:keys [start buses max-bus]} (read-input2 input)
        [max-offset max-id] max-bus
        adj-buses (adjust-offsets max-offset buses)]
    (loop [t (find-start start max-offset max-id)]
      (if (every? #(good-t-for-bus? % t) adj-buses)
        (- t max-offset)
        (recur (+ t max-id))))))


