(ns advent-of-code-2020.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [loco.core :as loco]
            [loco.constraints :as lc]))


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
; (t + 0) % 7 = 0
; (t + 1) % 13 = 0
; (t + 4) % 59 = 0
; (t + 6) % 31 = 0
; (t + 7) % 19 = 0


; after reading a bit of https://0xdf.gitlab.io/adventofcode2020/13
; I learned that the system of equations can be rewritten from:
; 
; t + i = 0 mod b , where = means congruent not equal
; t = -i mod b
; t = b - i mod b , cuz modular arithmetic works like that
; 
; so there is a system of congruences that needs to be solved.
; t = 7 - 0 (mod 7) 
; t = 13 - 1 (mod 13) 
; t = 59 - 4 (mod 59) 
; t = 31 - 6 (mod 31) 
; t = 19 - 7 (mod 19) 

; The same website references the Chinese Remainder Theorem.
; The theorem uses the Euclid's extended algorithm which I found here: 
; https://brilliant.org/wiki/modular-arithmetic/#modular-arithmetic-multiplicative-inverses
; and translated from python to Clojure.

(defn egcd
  [a b]
  (loop [a a
         b b
         x 0
         y 1
         u 1
         v 0]
    (if (not= a 0)
      (let [q (quot b a)
            r (mod b a)
            m (- x (* u q))
            n (- y (* v q))]
        (recur r
               a
               u
               v
               m
               n))
      [b x y])))


(defn mod-inv
  [a m]
  (let [[gcd x y] (egcd a m)]
    (if (= gcd 1)
      (mod x m))))


#_(prn (chinese-rem (:buses (read-input2 test-input))))
#_(prn (chinese-rem (:buses (read-input2 real-input))))

; This code follows the algorithm described here:
; https://brilliant.org/wiki/chinese-remainder-theorem/#solving-systems-of-congruences
(defn chinese-rem
  [buses]
  (let [bus-ids (map second buses)
        offsets (map first buses)
        product (reduce * bus-ids)
        y (map #(quot product %) bus-ids)
        z (map (fn [yi ni] (mod-inv yi ni)) y bus-ids)
        xs (map (fn [i bus yi zi] (* (- bus i) yi zi)) offsets bus-ids y z)]
    (mod (reduce + xs) product)))
