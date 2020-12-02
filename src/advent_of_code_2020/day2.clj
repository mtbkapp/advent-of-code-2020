(ns advent-of-code-2020.day2
  (:require [advent-of-code-2020.utils :as utils]
            [instaparse.core :as insta]))


(def parser*
  (insta/parser 
    "LINE = NUMBER <'-'> NUMBER <' '> CHAR <': '> PASSWORD
    NUMBER = #'[1-9]' #'[0-9]'*
    CHAR = #'[a-z]'
    PASSWORD = #'[a-z]'+"))

 
(defn parser
  [input]
  (let [[_ [_ & xs] [_ & ys] [_ chr] [_ & pwd]] (parser* input)]
    {:char chr
     :x (Long/valueOf (apply str xs))
     :y (Long/valueOf (apply str ys))
     :password pwd}))


(defn sled-rental-valid?
  [{c :char mn :x mx :y pwd :password :as parse}]
  (<= mn (get (frequencies pwd) c 0) mx))


(defn count-valid
  [valid?]
  (-> (utils/read-lines (comp (map parser)
                              (map valid?))
                        "day2.txt")
      (frequencies)
      (get true)))


#_(prn (solve-part1))
(defn solve-part1
  []
  (count-valid sled-rental-valid?))


(defn toboggan-valid?
  [{c :char x :x y :y pwd :password :as parse}]
  (let [xc (= c (nth pwd (dec x)))
        yc (= c (nth pwd (dec y)))]
    (or (and xc (not yc))
        (and (not xc) yc))))


#_(prn (solve-part2))
(defn solve-part2
  []
  (count-valid toboggan-valid?))

