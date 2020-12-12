(ns advent-of-code-2020.day08
  (:require [advent-of-code-2020.utils :as utils]))


(def pattern #"(acc|jmp|nop) (\+|\-)([0-9]+)")


(defn decode-instr
  [instr]
  (let [[_ op dir mag :as x] (re-matches pattern instr)]
    [(keyword op) ((if (= "-" dir) - identity) (Long/valueOf mag))]))


(defn read-input
  []
  (utils/read-lines (map decode-instr) "day08.txt"))


(defn run-prg
  [prg]
  (loop [pc 0 a 0 executed #{}]
    (cond (contains? executed pc) [:loop a pc]
          (= (count prg) pc) [:terminate a pc]
          (not (<= 0 pc (dec (count prg)))) [:out-of-bounds a pc]
          :else (let [[op arg] (get prg pc)
                      ne (conj executed pc)]
                  (cond (= :nop op) (recur (inc pc) a ne)
                        (= :acc op) (recur (inc pc) (+ a arg) ne)
                        (= :jmp op) (recur (+ pc arg) a ne))))))


#_(prn (solve-part1))
(defn solve-part1
  []
  (run-prg (read-input)))


(defn flip-op
  [prg last-change]
  (let [i (inc last-change)
        [op] (get prg i)]
    (if (= :acc op)
      (recur prg i)
      [i (update-in prg [i 0] {:jmp :nop :nop :jmp})])))


#_(prn (solve-part2))
(defn solve-part2
  []
  (let [prg (read-input)]
    (loop [last-change -1]
      (let [[change test-prg] (flip-op prg last-change)
            [result a pc] (run-prg test-prg)]
        (if (= result :terminate)
          [change a]
          (recur change))))))
