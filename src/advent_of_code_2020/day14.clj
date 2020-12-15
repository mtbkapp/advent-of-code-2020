(ns advent-of-code-2020.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as insta]
            ))


(defn build-mask
  [input]
  [(Long/valueOf (string/replace input "X" "0") 2)
   (Long/valueOf (string/replace input "X" "1") 2)])


(defn mask
  [x [ones-mask zeros-mask]]
  (-> x
      (bit-or ones-mask)
      (bit-and zeros-mask)))


;mask = 1100X10X01001X111001X00010X00100X011
;mem[24821] = 349

(defn parse-line*
  [[op arg0 arg1 :as x]]
  op 
  #_(if (= :mask op)
    [op (build-mask arg0)]
    [op (Long/valueOf arg0) (Long/valueOf arg1)]))


(def parse-line*
  (insta/parser
    "instr = mask | mem
    mask = <'mask = '> #'(0|1|X)+' 
    mem = <'mem['> number <'] = '> number
    <number> = #'[1-9][0-9]+'"))


(defn parse-line
  [line]
  (let [[_ [op arg0 arg1]] (parse-line* line)]
    (if (= :mask op)
      [op (build-mask arg0)]
      [op (Long/valueOf arg0) (Long/valueOf arg1)])))



#_(parse-line "mask = 1100X10X01001X111001X00010X00100X011")
#_(parse-line "mem[24821] = 349")


