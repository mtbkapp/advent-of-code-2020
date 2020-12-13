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


(defn solve-part1
  [input]
  (let [{:keys [start buses]} (read-input input)
        [t bus] (find-first start buses)]

    
    ))

