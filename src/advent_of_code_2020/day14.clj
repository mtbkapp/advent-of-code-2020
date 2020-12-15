(ns advent-of-code-2020.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as insta]
            )
  (:import [java.util BitSet]))


(set! *warn-on-reflection* true)

(defn build-mask
  [input]
  [(Long/valueOf (string/replace input "X" "0") 2)
   (Long/valueOf (string/replace input "X" "1") 2)])


(defn mask
  [x [ones-mask zeros-mask]]
  (-> x
      (bit-or ones-mask)
      (bit-and zeros-mask)))


(def parse-line*
  (insta/parser
    "instr = mask | mem
    mask = <'mask = '> #'(0|1|X)+' 
    mem = <'mem['> number <'] = '> number
    <number> = #'[0-9]+'"))


(defn parse-line
  [line]
  (let [[_ [op arg0 arg1]] (parse-line* line)]
    (if (= :mask op)
      [op (build-mask arg0)]
      [op (Long/valueOf ^String arg0) (Long/valueOf ^String arg1)])))


(def start-state
  {:mask (build-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
   :mem {}})


(defn exec
  [computer [op arg0 arg1]]
  (if (= :mask op)
    (assoc computer :mask arg0)
    (assoc-in computer [:mem arg0] (mask arg1 (:mask computer)))))


(defn sum-mem
  [{:keys [mem]}]
  (reduce + (vals mem)))


(def test-program
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(def real-program (slurp (io/resource "day14.txt")))


#_(prn (solve-part1 real-program))
#_(prn (solve-part1 test-program))
(defn solve-part1
  [input]
  (transduce
    (map parse-line)
    (completing
      exec 
      sum-mem)
    start-state
    (string/split-lines input)))


(defn positions-of
  ([mask v] (positions-of (reverse mask) v 0 []))
  ([[m & ms] v i ps]
   (if (nil? m)
     ps
     (recur ms v (inc i) (if (= m v) (conj ps i) ps)))))


(defn build-addr
  [base floater-val floater-positions]
  (let [base-bs (BitSet/valueOf (long-array [base]))
        floater-val-bs (BitSet/valueOf (long-array [floater-val]))]
    (loop [i 0 [fi & fs] floater-positions]
      (if (nil? fi)
        (first (seq (.toLongArray base-bs)))
        (do
          (.set base-bs ^long fi (.get floater-val-bs i)) 
          (recur (inc i) fs))))))


(defn build-floater-addrs
  [mask base]
  (let [ones-mask (Long/valueOf (string/replace mask "X" "0") 2)
        base2 (bit-or base ones-mask)
        floater-positions (positions-of mask \X)]
    (map #(build-addr base2 % floater-positions)
         (range 0 (Math/pow 2 (count floater-positions))))))


(defn parse-line2
  [line]
  (let [[_ [op arg0 arg1]] (parse-line* line)]
    (if (= :mask op)
      [op arg0]
      [op (Long/valueOf ^String arg0) (Long/valueOf ^String arg1)])))


(defn exec2
  [state [op arg0 arg1]]
  (if (= :mask op)
    (assoc state :mask arg0)
    (update state :mem
            #(reduce
               (fn [mem addr]
                 (assoc mem addr arg1))
               %
               (build-floater-addrs (:mask state) arg0)))))


#_(prn (solve-part2 real-program))
#_(prn (solve-part2 test-program2))
(defn solve-part2
  [input]
  (transduce
    (map parse-line2)
    (completing
      exec2
      sum-mem)
    {:mem {}}
    (string/split-lines input)))
