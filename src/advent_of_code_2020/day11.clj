(ns advent-of-code-2020.day11
  (:require [advent-of-code-2020.utils :as utils]
            [clojure.string :as string]))


(def test-input 
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")


; read the input and produce a set of coordinates

(clojure.pprint/pprint
  (transduce
    (comp (map-indexed (fn [y line] 
                         (map-indexed (fn [x c] [x y c]) line))) 
          cat)
    (completing
      (fn [acc [x y c]]
        (if (= \L c)
          (conj acc [x y])
          acc)))
    #{}
    (string/split-lines test-input)))
