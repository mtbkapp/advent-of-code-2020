(ns advent-of-code-2020.day25
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]))


(defn xform
  [sub loop-size]
  (loop [s loop-size
         v 1]
    (if (= 0 s)
      v
      (recur (dec s)
             (mod (* v sub) 20201227)))))

(defn find-loop-size
  [pk]
  (loop [s 0 v 1]
    (if (= pk v)
      s
      (recur (inc s)
             (mod (* v 7) 20201227)))))


(defn part1
  []
  (let [pk0 18356117
        pk1 5909654
        ls0 (find-loop-size pk0)
        ls1 (find-loop-size pk1)]
    (prn (xform pk0 ls1)
         (xform pk1 ls0))))
