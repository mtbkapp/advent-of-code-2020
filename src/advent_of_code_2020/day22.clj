(ns advent-of-code-2020.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


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


(defn play-game
  "Given 2 decks in a vector returns the state of the decks after playing a
  complete game. The winner is the one that still has cards."
  [init-decks]
  (loop [prev-rounds #{}
         [[p1-card :as p1-deck] [p2-card :as p2-deck] :as decks] init-decks]
    (cond
      ; game is over
      (or (empty? p1-deck) (empty? p2-deck))
      decks
      ; previous state, player 1 wins
      (contains? prev-rounds decks)
      (recur prev-rounds
             [(into (pop p1-deck) [p1-card p2-card])
              (pop p2-deck)])
      ; winner by recursive game 
      (and (<= p1-card (count p1-deck))
           (<= p2-card (count p2-deck)))
      (let [[p1-sub-deck] (play-game [(pop p1-deck) (pop p2-deck)])]
        (recur
          (conj prev-rounds decks)
          (if (empty? p1-sub-deck)
            [(pop p1-deck)
             (into (pop p2-deck) [p2-card p1-card])]
            [(into (pop p1-deck) [p1-card p2-card])
             (pop p2-deck)])))
      ; regular round, player 1 wins
      (< p2-card p1-card)
      (recur 
        (conj prev-rounds decks)
        [(into (pop p1-deck) [p1-card p2-card])
         (pop p2-deck)])
      :else ; player 2 wins
      (recur
        (conj prev-rounds decks)
        [(pop p1-deck)
         (into (pop p2-deck) [p2-card p1-card])]))))


; it's wrong!, debug!
#_(prn-state (play-game (read-input test-input)))


