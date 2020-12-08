(ns advent-of-code-2020.day8
  (:require [advent-of-code-2020.utils :as utils]
            [clojure.string :as string]
            [instaparse.core :as insta]))


(def pattern #"(acc|jmp|nop) (\+|\-)([0-9]+)")


(defn decode-instr
  [instr]
  (let [[_ op dir mag :as x] (re-matches pattern instr)]
    [(keyword op) ((if (= "-" dir) - +) (Long/valueOf mag))]
    
    ))


(defn init 
  [prg]
  {:pc 0 :a 0})


(defmulti next-state*
  (fn [state [op :as instr]]
    op))

(defmethod next-state* :nop
  [state instr]
  (update state :pc inc))

(defmethod next-state* :acc
  [state [_ arg]]
  (-> state
      (update :a + arg)
      (update :pc inc)))

(defmethod next-state* :jmp
  [state [_ arg]]
  (update state :pc + arg))

(defn read-input
  []
  (utils/read-lines (map decode-instr) "day8.txt"))

#_(find-loop (read-input))
(defn find-loop
  [prg]
  (loop [{:keys [pc] :as state} (init prg) executed #{}]
    (if (contains? executed pc)
      state
      (recur (next-state* state (get prg pc))
             (conj executed pc)))))
