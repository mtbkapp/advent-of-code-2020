(ns advent-of-code-2020.day01
  (:require [clojure.java.io :as io]))


(defn read-entries
  []
  (with-open [rdr (io/reader (io/resource "day01.txt"))]
    (into []
          (map #(Long/valueOf %))
          (line-seq rdr))))



#_(prn (solve-part-1 (read-entries)))
(defn solve-part-1
  [entries]
  (let [es (read-entries)
        [x y] (first (for [x entries
                           y entries
                           :when (and (not= x y)
                                      (= 2020 (+ x y)))]
                       [x y]))]
    [x y (* x y)]))


#_(prn (solve-part-2 (read-entries)))
(defn solve-part-2
  [entries]
  (let [[x y z] (first (for [x entries
                             y entries
                             z entries
                             :when (and (not= x y) 
                                        (not= y z)
                                        (not= x z)
                                        (= 2020 (+ x y z)))]
                         [x y z]))]
    [x y z (* x y z)]))


; faster way?
; sorted = sort(input) 
; for x in input:
;   y = binary_search(sorted, 2020 - x):
;   if y is found:
;      return x * y
;
; part2?
; for x in input:
;   y = 2020 - x
;   repeat above but substitute 2020 for y
