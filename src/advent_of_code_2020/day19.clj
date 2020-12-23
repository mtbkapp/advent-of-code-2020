(ns advent-of-code-2020.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as insta]))

(def real-input (slurp (io/resource "day19.txt")))


(defn extract-rule
  [rule-line]
  (let [[id body] (string/split rule-line #": ")]
    [(Long/valueOf id) body]))


(defn rules->instaparse
  [rules overrides]
  (->> rules
       string/split-lines
       (map extract-rule)
       (sort-by first)
       (map (fn [[id body]]
              (str id " = " (get overrides id body))))
       (string/join \newline)
       insta/parser))


(defn count-valid-messages
  [input overrides]
  (let [[rule-section msg-section] (string/split input #"\n\n")
        parser (rules->instaparse rule-section overrides) 
        messages (string/split-lines msg-section)]
    (->> messages
         (map (comp type parser))
         (remove (partial = instaparse.gll.Failure))
         count)))


(defn solve-part1
  []
  (count-valid-messages real-input {}))


(defn solve-part2
  []
  (count-valid-messages real-input {8 "42 | 42 8"
                                    11 "42 31 | 42 11 31"}))

; This is too easy with a parser generator like instaparse. Come back and write
; my own parser?
