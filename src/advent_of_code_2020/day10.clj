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


#_(map vec (adjacent (chain test-input-1)))
(defn adjacent
  [chain]
  (reduce (fn [adj i]
            (if (can-remove? chain i)
              (conj adj (remove-adapter chain i))
              adj))
          []
          (range (count chain))))


#_(count (bfs (chain test-input-1)))
#_(count (bfs (chain test-input-2)))
#_(prn (count (bfs (chain input))))
(defn bfs
  ([start] (bfs #{} (conj (clojure.lang.PersistentQueue/EMPTY) start)))
  ([visited q]
   (if (empty? q)
     visited
     (let [v (peek q)
           vv (vec v)]
       (if (contains? visited vv)
         (recur visited (pop q))
         (recur (conj visited vv)
                (into (pop q) (adjacent v))))))))


(def test-input-arrs
  #{[0 1 4 5 6 7 10 11 12 15 16 19 22]
    [0 1 4 5 6 7 10 12 15 16 19  22]
    [0 1 4 5 7 10 11 12 15 16 19  22]
    [0 1 4 5 7 10 12 15 16 19  22]
    [0 1 4 6 7 10 11 12 15 16 19  22]
    [0 1 4 6 7 10 12 15 16 19 22]
    [0 1 4 7 10 11 12 15 16 19  22]
    [0 1 4 7 10 12 15 16 19 22]})






(sort test-input-1)

#_(find-next 4 (rest (rest (sort test-input-1))))
(defn find-next
  [curr rest-adapters]
  (take-while (fn [a]
                (<= 1 (- a curr) 3))
              rest-adapters))


(add-ends (apply sorted-set test-input-1))
(defn build-graph
  [input]
  (reduce (fn [g input]
            
            )
          (add-ends (apply sorted-set input))))


; how many paths are there in a graph from the plug to the device through the adapters?


(defn build-input2 
  [input]
  (let [mx (reduce max input)]
    (sort (into input [0 (+ mx 3)]))))

(defn next-adapters
  [[a :as adapters]]
  (filter (fn [[x :as na]]
            (if (not-empty na)
              (<= 1 (- x a) 3)))
          (map #(drop % adapters) [1 2 3])))

#_(next-adapters (vector 0 1 4 5 6 7 10)) ; [[1 4 5 6 7 10]]
#_(next-adapters (vector 1 4 5 6 7 10)) ; [[4 5 6 7 10]] 
#_(next-adapters (vector 4 5 6 7 10)) ;[[5 6 7 10] [6 7 10] [7 10]]
#_(next-adapters [6 7 10])
#_(next-adapters [19 22])
#_(next-adapters [22])


#_(go (vector 0 1 4 5 6 7 10 11 12 15 16 19 22))
#_(go (vector 10 11 12 15 16 19 22))
#_(go (vector 19 22))
#_(go (vector 22))
#_(go (vector))

#_(go (build-input2 test-input-1))
#_(time (go (build-input2 test-input-2)))
#_(prn (time (go (build-input2 input))))

(declare go)
(defn go*
  [input]
  (if (< (count input) 2)
    1
    (reduce + (map go (next-adapters input)))))
(def go (memoize go*))




