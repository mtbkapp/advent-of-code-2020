(ns advent-of-code-2020.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as insta]))


(def test-input
  "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")


(def real-input (slurp (io/resource "day16.txt")))

#_(parse real-input)
(def parse*
  (insta/parser
    "input = rule-list <'\n\n'> your-ticket <'\n\n'> nearby-tickets
    rule-list = rule | rule <'\n'> rule-list
    rule = name <': '> range <' or '> range
    name = #'[A-Za-z\\s]+'
    range = number <'-'> number
    number = #'[0-9]+'
    your-ticket = <'your ticket:\n'> ticket 
    ticket = number | number <','> ticket
    nearby-tickets = <'nearby tickets:\n'> ticket-list
    ticket-list = ticket | ticket <'\n'> ticket-list"))


(defn un-nest
  [[label x xs]]
  (if (nil? xs)
    [x]
    (cons x (un-nest xs))))


(defn parse-numbers
  [xs]
  (map (fn [[_ s]] (Long/valueOf s)) xs))


(defn build-rule
  [[_ [_ rule-name] & ranges]]
  {:name rule-name
   :ranges (map (comp parse-numbers rest) ranges)})


(defn parse
  [input]
  (let [[_ rule-list [_ your-ticket] [_ nearby-tickets]] (parse* (string/trim input))]
    {:rules (map build-rule (un-nest rule-list))
     :your-ticket (parse-numbers (un-nest your-ticket))
     :nearby-tickets (map (comp parse-numbers un-nest) (un-nest nearby-tickets))}))


(defn find-invalid
  [{:keys [rules nearby-tickets]}]
  (remove (fn [x]
            (some (fn [[low high]]
                    (<= low x high))
                  (mapcat :ranges rules)))
          (mapcat identity nearby-tickets)))


#_(prn (solve-part1 test-input))
#_(prn (solve-part1 real-input))
(defn solve-part1
  [input]
  (reduce + (find-invalid (parse input))))


(defn valid-value?
  [{[[a b] [c d]] :ranges :as rule} x]
  (or (<= a x b)
      (<= c x d)))


(defn invalid-ticket? 
  [rules ticket]
  (some (fn [x]
          (every? (complement #(valid-value? % x)) rules))
        ticket))


(defn remove-invalid
  [{:keys [rules] :as parsed-input}]
  (update parsed-input
          :nearby-tickets
          (partial remove (partial invalid-ticket? rules))))


(defn build-columns
  [nearby-tickets]
  (reduce (fn [cols row]
            (map (fn [col x]
                   (conj col x))
                 cols
                 row))
          (repeat (count (first nearby-tickets)) [])
          nearby-tickets))


(defn add-columns
  [{:keys [nearby-tickets] :as input}]
  (assoc input :cols (build-columns nearby-tickets)))


(def test-input2
  "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9")


(defn find-potential-fields 
  [{:keys [cols rules] :as input}]
  (map (fn [c]
         (into #{}
               (comp (filter (fn [rule] 
                               (every? (partial valid-value? rule) c)))
                     (map :name))
               rules))
       cols))


(defn find-next
  [cols processed]
  (some (fn [c]
          (if (and (= 1 (count c))
                   (not (contains? processed (first c))))
            (first c)))
        cols))


(defn done?
  [cols]
  (every? #(= 1 (count %)) cols))


(defn remove-found
  [cols field]
  (map #(if (= #{field} %) % (disj % field))  cols))


(defn assign-fields
  [parsed-input]
  (loop [cols (->> parsed-input
                   remove-invalid
                   add-columns
                   find-potential-fields) 
         processed #{}]
    (if (done? cols) 
      (map first cols)
      (let [c (find-next cols processed)]
        (recur (remove-found cols c) 
               (conj processed c))))))


#_(prn (solve-part2 real-input))
(defn solve-part2
  [input]
  (let [{:keys [your-ticket] :as parsed-input} (parse input)]
    (transduce
      (comp (map-indexed vector)
            (filter #(string/starts-with? (second %) "departure"))
            (map first)
            (map (partial nth your-ticket)))
      *
      (assign-fields parsed-input))))

