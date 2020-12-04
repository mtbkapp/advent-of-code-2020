(ns advent-of-code-2020.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as insta]))


(
 (insta/parser
   "entry_list = entry | entry <entry_separator> entry_list
   entry = name <':'> value
   name = #'[a-z]+' 
   value = #'[0-9a-zA-Z#]+'
   entry_separator = ' ' | '\n'
   ")
 "iyr:2013 hcl:#ceb3a1
hgt:151cm"
 )


(count (string/split 
  (slurp (io/resource "day4.txt"))
  #"\n\n"
  ))


(defn read-input
  []
  (as-> (slurp (io/resource "day4.txt")) x
    (string/split x #"\n\n")
    (map #(string/replace % #"\n" " ") x)
    )
  )
