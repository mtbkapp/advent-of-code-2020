(ns advent-of-code-2020.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as insta])
  (:import [instaparse.gll Failure]))


(def real-input (slurp (io/resource "day18.txt")))

(count (string/split-lines real-input))

(def numbers #{\1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn tokenize
  [line]
  (sequence (comp (remove (partial = \space))
                  (map #(if (contains? numbers %)
                          (Long/valueOf (str %))
                          %)))
            line))


(def operators
  {\* *
   \+ +})


(declare calc)


(defn calc*
  [result [[sym rhs :as op] & ops :as d]]
  (if (nil? op)
    result
    (recur ((get operators sym)
            result
            (if (number? rhs) rhs (calc rhs)))
           ops)))


(defn calc
  [[t & ts]]
  (calc* (if (number? t) t (calc t))
         (partition 2 ts)))


(defn group
  ([tokens] (group tokens [] []))
  ([[t & ts] stack curr]
   (if (nil? t)
     curr
     (cond (= \( t) (recur ts (conj stack curr) [])
           (= \) t) (recur ts (pop stack) (conj (peek stack) curr))
           :else (recur ts stack (conj curr t))))))


#_(prn (solve-part1 real-input))
(defn solve-part1
  [input]
  (->> (string/split-lines real-input)
       (map (comp calc group tokenize))
       (reduce +)))


; Grammar adapted from pg 50 of Programming Language Pragmatics III Edition by Micheal Scott
(def parse*
  (insta/parser "expr = term | expr '*' term
                term = factor | term '+' factor
                factor = number | factor | <'('> expr <')'>
                <number> = #'[1-9]'"))


(defn parse-line
  [line]
  (parse* (string/replace line " " "")))



(def evaluate nil)
(defmulti evaluate
  (fn [[node-name & args]]
    [node-name (count args)]))

(defmethod evaluate [:factor 1]
  [[_ arg]]
  (if (string? arg)
    (Long/valueOf arg)
    (evaluate arg)))

(defmethod evaluate [:term 1]
  [[_ arg]]
  (evaluate arg))

(defmethod evaluate [:term 3]
  [[_ arg0 _ arg1]]
  (+ (evaluate arg0) (evaluate arg1)))

(defmethod evaluate [:expr 1]
  [[_ arg]]
  (evaluate arg))

(defmethod evaluate [:expr 3]
  [[_ arg0 _ arg1]]
  (* (evaluate arg0) (evaluate arg1)))


#_(prn (solve-part2 real-input))
(defn solve-part2
  [input]
  (->> (string/split-lines input)
       (map parse-line)
       (map evaluate)
       (reduce +)))
