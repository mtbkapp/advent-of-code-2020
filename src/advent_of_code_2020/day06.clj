(ns advent-of-code-2020.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as sets]))



(def input (slurp (io/resource "day06.txt")))


(defn sum-groups
  [input group-fn]
  (->> (string/split input #"\n\n")
       (map #(string/split % #"\n"))
       (map #(map set %))
       (map #(apply group-fn %))
       (map count)
       (reduce +)))


; part 1
#_(sum-groups input sets/union)

; part 2
#_(sum-groups input sets/intersection)
