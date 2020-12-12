(ns advent-of-code-2020.day04
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [instaparse.core :as insta]))


(def parse*
  (insta/parser
    "rec-list = rec <rec-separator> rec-list | rec
    rec-separator = '\n\n'
    rec = entry <entry-separator> rec | entry
    entry = name <':'> value
    name = #'[a-z]+' 
    value = #'[0-9a-zA-Z#]+'
    entry-separator = ' '+ | '\n'"))


(defn un-nest
  [[label x xs]]
  (if (nil? xs)
    [x]
    (cons x (un-nest xs))))


(defn parse
  [input]
  (->> (parse* input)
       (un-nest)
       (map un-nest)
       (map #(reduce (fn [m [_ [_ k] [_ v]]]
                       (assoc m (keyword k) v))
                     {}
                     %))))


(defn read-input
  ([] (read-input (slurp (io/resource "day04.txt"))))
  ([input]
   (parse (string/trim input))))


(defn count-valid
  [valid? input]
  (get (frequencies (map valid? input)) true))


(def required-fields
  #{:byr :iyr :eyr :hgt :hcl :ecl :pid})


#_(prn (solve-part1))
(defn solve-part1
  []
  (count-valid #(sets/subset? required-fields (set (keys %))) (read-input)))


(defn in-range
  [min-v max-v]
  (fn [v]
    (try
      (<= min-v (Long/valueOf v) max-v)
      (catch NumberFormatException _
        false))))


(defn matches 
  [re]
  #(some? (re-matches re %)))


(spec/def ::byr (in-range 1920 2002))

(spec/def ::iyr (in-range 2010 2020))

(spec/def ::eyr (in-range 2020 2030))

(spec/def ::hgt
  (let [cm-valid? (in-range 150 193)
        in-valid? (in-range 59 76)]
    (fn [hgt]
      (when-let [[_ mag unit] (re-matches #"([1-9][0-9]*)(cm|in)" hgt)]
        (if (= "cm" unit)
          (cm-valid? mag)
          (in-valid? mag))))))

(spec/def ::hcl (matches #"#[0-9a-f]{6}"))

(spec/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(spec/def ::pid (matches #"[0-9]{9}"))

(spec/def ::passport 
  (spec/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]))


#_(prn (solve-part2))
(defn solve-part2
  []
  (count-valid #(spec/valid? ::passport %) (read-input)))

