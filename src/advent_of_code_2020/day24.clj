(ns advent-of-code-2020.day24
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [clojure.test :refer :all]))


; https://en.wikipedia.org/wiki/Hexagonal_Efficient_Coordinate_System#Description

(spec/def :hex/coord
  (spec/cat :array int? 
            :row int? 
            :col int?))


(def move nil)
(defmulti move (fn [coord dir] dir))

(defmethod move :dir/west
  [[a r c] _]
  [a r (dec c)])

(defmethod move :dir/east
  [[a r c] _]
  [a r (inc c)])

(defmethod move :dir/north-east
  [[a r c] _]
  (if (zero? a)
    [1 (dec r) c] 
    [0 r (inc c)]))

(defmethod move :dir/north-west
  [[a r c] _]
  (if (zero? a)
    [1 (dec r) (dec c)]
    [0 r c]))

(defmethod move :dir/south-east
  [[a r c] _]
  (if (zero? a)
    [1 r c] 
    [0 (inc r) (inc c)]))

(defmethod move :dir/south-west
  [[a r c] _]
  (if (zero? a)
    [1 r (dec c)] 
    [0 (inc r) c]))


(deftest test-move
  (testing "one move all directions"
    (is (= [0 0 1] (move :dir/east [0 0 0])))
    (is (= [0 0 -1] (move :dir/west [0 0 0])))
    (is (= [0 0 2] (move :dir/north-east [1 0 1])))
    (is (= [0 0 1] (move :dir/north-west [1 0 1])))
    (is (= [0 1 2] (move :dir/south-east [1 0 1])))
    (is (= [0 1 1] (move :dir/south-west [1 0 1])))
    (is (= [1 0 1] (move :dir/east [1 0 0])))
    (is (= [1 0 -1] (move :dir/west [1 0 0])))
    (is (= [1 0 2] (move :dir/north-east [0 1 2])))
    (is (= [1 0 1] (move :dir/north-west [0 1 2])))
    (is (= [1 1 2] (move :dir/south-east [0 1 2])))
    (is (= [1 1 1] (move :dir/south-west [0 1 2]))))
  
  )

(def dirs
  {"e" :dir/east
   "se" :dir/south-east
   "sw" :dir/south-west
   "w" :dir/west
   "nw" :dir/north-west
   "ne" :dir/north-east})


(def pattern #"(e|se|sw|w|nw|ne)")

(def test-input
  "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew")


(defn parse-line
  [line]
  (map (comp dirs first)
       (re-seq pattern line)))


(defn run-seq 
  ([ds] (run-seq [0 0 0] ds))
  ([start ds]
   (reduce move start ds)))


(defn init
  [input]
  (transduce
    (map (comp run-seq parse-line))
    (completing
      (fn [black tile]
        (if (contains? black tile)
          (disj black tile)
          (conj black tile))))
    #{}
    (string/split-lines input)))


#_(part1 test-input)
#_(part1 (slurp (io/resource "day24.txt")))
(defn part1
  [input]
  (count (init input)))


(defn tile->adj-tiles
  [tile]
  (into #{}
        (map #(move tile %))
        (vals dirs)))


(defn to-white 
  [black]
  (into #{}
        (filter (fn [bt]
                  (let [c (->> (tile->adj-tiles bt)
                               (filter black)
                               (count))]
                    (or (> c 2)
                        (= c 0)))))
        black))


; there is something redundant about this solution I'd like to get rid of. But
; I'm not clever enough right now.
(defn adj-white
  [black]
  (into #{}
        (comp (mapcat tile->adj-tiles)
              (remove black))
        black))


(defn to-black
  [black]
  (into #{}
        (filter (fn [wt]
                  (->> (tile->adj-tiles wt)
                       (filter black)
                       (count)
                       (= 2))))
        (adj-white black)))


(defn step
  [black]
  (sets/difference (into black (to-black black))
                   (to-white black)))


#_(count-at-day test-input 100)
#_(count-at-day (slurp (io/resource "day24.txt")) 100)
(defn count-at-day
  [input day]
  (->> (iterate step (init input))
       (take (inc day))
       (last)
       (count)))
