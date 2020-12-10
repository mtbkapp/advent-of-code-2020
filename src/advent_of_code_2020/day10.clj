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


; part2 idea
; * any adapter chain must be sorted
; * start with using them all then do a bfs from there
; * adjacent states can be formed by removing any single adapter while keeping constraints

; * is there a way to calculate the number from knowing which can be removed?
; * I don't think so becuase removing one or another will produce different possibilities on the next step

#_(vec (chain test-input-1))
(defn chain
  [xs]
  (let [arr (long-array (sort xs))
        chain (long-array (+ 2 (count arr)))]
    (System/arraycopy arr 0 chain 1 (count arr))
    (aset chain 0 0)
    (aset chain (dec (count chain)) (+ 3 (last arr)))
    chain))


(defn remove-adapter
  [chain i]
  (let [new-chain (long-array (dec (count chain)))]
    (System/arraycopy chain 0 new-chain 0 i)
    (System/arraycopy chain (inc i) new-chain i (- (count chain) i 1))
    new-chain))

(deftest test-remove-adapter
  (is (= [1 3 5 7 10] (vec (remove-adapter (chain [1 3 5 7]) 0))))
  (is (= [0 3 5 7 10] (vec (remove-adapter (chain [1 3 5 7]) 1))))
  (is (= [0 1 5 7 10] (vec (remove-adapter (chain [1 3 5 7]) 2))))
  (is (= [0 1 3 7 10] (vec (remove-adapter (chain [1 3 5 7]) 3))))
  (is (= [0 1 3 5 10] (vec (remove-adapter (chain [1 3 5 7]) 4))))
  (is (= [0 1 3 5 7] (vec (remove-adapter (chain [1 3 5 7]) 5)))))


(defn can-remove?
  [chain i]
  (and (< 0 i (dec (count chain)))
       (<= 1 (- (aget chain (inc i)) (aget chain (dec i))) 3)))


#_(adjacent (chain test-input-1))
(defn adjacent
  [chain]
  (reduce (fn [adj i]
            (if (can-remove? chain i)
              (conj adj (remove-adapter chain i))
              adj))
          []
          (range (count chain))))


(count (bfs (chain test-input-1)))
(defn bfs
  ([start] (bfs #{} (conj (clojure.lang.PersistentQueue/EMPTY) start)))
  ([visited q]
   (if (empty? q)
     visited
     (let [v (peek q)]
       (recur (conj visited v)
              (into (pop q) (adjacent v)))))))
