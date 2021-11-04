(ns advent-of-code-2020.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.logging :as log]))


(def test-input
  "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")

(def real-input (slurp (io/resource "day22.txt")))

#_(read-input test-input)
(defn read-input-deck
  [deck-input]
  (let [[id-line & card-lines] (string/split-lines deck-input)]
     (into clojure.lang.PersistentQueue/EMPTY
           (map #(Long/valueOf %))
           card-lines)))


(defn read-input
  [input]
  (let [[deck0 deck1] (string/split input #"\n\n")]
    [(read-input-deck deck0)
     (read-input-deck deck1)]))


(defn play-round
  [[deck0 deck1]]
  (let [p1-card (peek deck0)
        p1-deck (pop deck0)
        p2-card (peek deck1)
        p2-deck (pop deck1)]
    (if (< p1-card p2-card)
      [p1-deck (into p2-deck [p2-card p1-card])]
      [(into p1-deck [p1-card p2-card]) p2-deck])))


(defn prn-state
  [s]
  (prn (mapv vec s)))


(defn score-deck
  [deck]
  (transduce (map-indexed (fn [i card] 
                            (* (inc i) card)))
             +
             (reverse deck)))

(defn solve-part1 
  [[deck0 deck1 :as state]]
  (if (or (empty? deck0) (empty? deck1))
    (apply max (map score-deck state))
    (recur (play-round state))))

#_(solve-part1 (read-input test-input))
#_(solve-part1 (read-input real-input))




#_(-> (recursive-combat
        (into clojure.lang.PersistentQueue/EMPTY [9 2 6 3 1])
        (into clojure.lang.PersistentQueue/EMPTY [5 8 4 7 10]))
      calc-score
      #_(update :p1-deck seq)
      #_(update :p2-deck seq))

#_(-> (apply recursive-combat (read-input real-input))
      calc-score
      )

; doesn't finish?
; bug? need to check configs in another place for duplicate?
(defn recursive-combat
  [p1-deck p2-deck]
  (loop [configs #{}
         p1-deck p1-deck
         p2-deck p2-deck]
    (cond
      ; p2 wins 
      (empty? p1-deck)
      {:winner :p2 :p1-deck p1-deck :p2-deck p2-deck}
      ; p1 wins
      (empty? p2-deck)
      {:winner :p1 :p1-deck p1-deck :p2-deck p2-deck}
      ; infinite loop breaker 
      (contains? configs [p1-deck p2-deck])
      {:winner :p1 :p1-deck p1-deck :p2-deck p2-deck}
      ; ok, time to play a round
      :else
      (let [p1-card (peek p1-deck)
            p1-deck2 (pop p1-deck)
            p2-card (peek p2-deck)
            p2-deck2 (pop p2-deck)
            winner (if (and (<= p1-card (count p1-deck2))
                            (<= p2-card (count p2-deck2)))
                     ; winner by recursion
                     (:winner (recursive-combat p1-deck2 p2-deck2))
                     ; winner by card value
                     (if (< p2-card p1-card) :p1 :p2))
            config2 (conj configs [p1-deck p2-deck])]
        (if (= :p1 winner)
          (recur config2 (into p1-deck2 [p1-card p2-card]) p2-deck2)
          (recur config2 p1-deck2 (into p2-deck2 [p2-card p1-card])))))))

(defn calc-score
  [{:keys [winner p1-deck p2-deck]}]
  (if (= :p1 winner)
    (score-deck p1-deck)
    (score-deck p2-deck)))
