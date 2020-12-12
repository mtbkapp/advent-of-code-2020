(ns advent-of-code-2020.day07
  (:require [advent-of-code-2020.utils :as utils]
            [clojure.string :as string]
            [instaparse.core :as insta]))


(def parse*
  (insta/parser
    "line = color <' bags contain '> (<no-bags> | bag-list) <'.'>
    bag-list = bag-clause | bag-clause <', '> bag-list
    bag-clause = number <' '> color <' '> <bag>
    bag = 'bag' 's'?
    no-bags = 'no other bags'
    color = word | word <' '> word | word <' '> word <' '> word 
    word = #'[a-z]+' 
    number = #'[1-9][0-9]*'"))


(defn un-nest
  [[label x xs]]
  (if (nil? xs)
    [x]
    (cons x (un-nest xs))))


(defn parse-line
  [line]
  (let [[_ color bag-list] (parse* line)
        to-color #(mapv second (rest %))]
    (when (some? bag-list)
      [(to-color color)
       (map (fn [[_ [_ n] color]]
              [(Long/valueOf n) (to-color color)])
            (un-nest bag-list))])))


(def read-xform (comp (map parse-line) (remove nil?)))


(defn build-edges
  [input]
  (mapcat (fn [[src dests]]
            (map (fn [[n dest]]
                   [src dest])
                 dests))
          input))


(defn build-graph
  [edges]
  (reduce (fn [g [left right]]
            (update g right (fnil conj #{}) left))
          {}
          edges))


(defn bfs 
  [graph start]
  (loop [visited #{}
         q (conj (clojure.lang.PersistentQueue/EMPTY) start)]
    (if (empty? q)
      visited
      (let [v (peek q)]
        (recur (conj visited v)
               (reduce conj (pop q) (get graph v)))))))


(defn read-input
  ([]
   (utils/read-lines read-xform "day07.txt"))
  ([input]
   (into [] read-xform (string/split-lines input))))


#_(prn (solve-part1))
(defn solve-part1
  []
  (-> (read-input)
      (build-edges)
      (build-graph)
      (bfs ["shiny" "gold"])
      (disj ["shiny" "gold"])
      (count)))


(defn count-bags*
  [g start]
  (inc (reduce (fn [c [n color]]
                 (+ c (* n (count-bags* g color))))
               0
               (get g start))))


(defn count-bags
  [g start]
  (dec (count-bags* g start)))


#_(prn (solve-part2))
(defn solve-part2
  []
  (count-bags (into {} (read-input)) ["shiny" "gold"]))
