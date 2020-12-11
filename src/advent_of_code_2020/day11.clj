(ns advent-of-code-2020.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as sets]
            [clojure.test :refer :all]))


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


(def real-input (slurp (io/resource "day11.txt")))

(defn init
  [input]
  (let [lines (string/split-lines input)
        seats (transduce
                (comp (map-indexed (fn [y line] 
                                     (map-indexed (fn [x c] [x y c]) line))) 
                      cat)
                (completing
                  (fn [acc [x y c]]
                    (cond (= \L c) (assoc acc [x y] false)
                          (= \# c) (assoc acc [x y] true)
                          :else acc)))
                {}
                lines)]
    {:seats seats
     :height (count lines)
     :width (count (first lines))}))


(defn render 
  [{:keys [seats height width] :as state}]
  (string/trim
    (with-out-str
      (doseq [y (range height)]
        (doseq [x (range width)]
          (print
            (let [occupied? (get seats [x y])]
              (if (some? occupied?)
                (if occupied? \# \L)
                \.))))
        (println)))))


(defn adj-seats
  [{:keys [seats] :as state} [x y :as seat]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :let [nx (+ x dx)
              ny (+ y dy)
              np [nx ny]]
        :when (and (not= np seat)
                   (contains? seats np))]
    np))


(defn count-adj-occupied
  [{:keys [seats] :as state} seat]
  (frequencies (map seats (adj-seats state seat))))


(defn next-state
  [state]
  (update state
          :seats
          #(reduce (fn [next-seats [seat curr-state]]
                     (let [occupied (get (count-adj-occupied state seat) true 0)]
                       (assoc next-seats
                              seat 
                              (cond (and (not curr-state)
                                         (zero? occupied)) true
                                    (and curr-state
                                         (<= 4 occupied)) false
                                    :else curr-state))))
                   {}
                   %)))


(defmacro compare-states 
  [s0 s1]
  `(do 
     (is (= (:width ~s0) (:width ~s1)))
     (is (= (:height ~s0) (:height ~s1)))
     (testing "has same seats"
       (let [s0ks# (set (keys (:seats ~s0)))
             s1ks# (set (keys (:seats ~s1)))
             ksd0# (sets/difference s0ks# s1ks#)
             ksd1# (sets/difference s1ks# s0ks#)]
         (is (empty? ksd0#) "things are in first not in second")
         (is (empty? ksd1#) "things are in second not in first")))
     (testing "seats have same states"
       (doseq [k# (keys (:seats ~s0))]
         (is (= (get-in ~s0 [:seats k#])
                (get-in ~s1 [:seats k#]))
             (str k#))))
     (testing "equal"
       (is (= ~s0 ~s1)))))


(def test-input1
  "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##")

(def test-input2
  "#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##")

(def test-input3
  "#.##.L#.##
#L###LL.L#
L.#.#..#..
#L##.##.L#
#.##.LL.LL
#.###L#.##
..#.#.....
#L######L#
#.LL###L.L
#.#L###.##")

(def test-input4
  "#.#L.L#.##
#LLL#LL.L#
L.L.L..#..
#LLL.##.L#
#.LL.LL.LL
#.LL#L#.##
..L.L.....
#L#LLLL#L#
#.LLLLLL.L
#.#L#L#.##")

(def test-input5
  "#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##")

(deftest compare-states
  (compare-states
    (next-state (init test-input))
    (init test-input1))
  (compare-states
    (next-state (next-state (init test-input)))
    (init test-input2))
  (compare-states
    (next-state (next-state (next-state (init test-input))))
    (init test-input3))
  (compare-states
    (next-state (next-state (next-state (next-state (init test-input)))))
    (init test-input4))
  (compare-states
    (next-state (next-state (next-state (next-state (next-state (init test-input))))))
    (init test-input5))
  (compare-states
    (next-state (init test-input5))
    (init test-input5)))


(defn count-occupied
  [{:keys [seats]}]
  (count (filter val seats)))


(defn stabilize
  [state]
  (let [nxt (next-state state)]
    (if (= state nxt)
      nxt
      (recur nxt))))


#_(prn (solve-part1 test-input))
#_(prn (solve-part1 real-input))
(defn solve-part1
  [input]
  (count-occupied (stabilize (init input))))



(defn vec*
  [mag v]
  (mapv (partial * mag) v))


(defn vec+
  [vx vy]
  (mapv + vx vy))


(defn in-bounds?
  [[x y] [width height]]
  (and (< -1 x width)
       (< -1 y height)))


(defn occupied-in-dir?
  "Returns
    true - if there is a seat in the given direction that is occupied
    false - if there is a seat in the given direction that is not occupied
    nil - if there is no seat in the given direction"
  [{:keys [seats width height]} seat dir]
  (loop [mag 1]
    (let [pos (vec+ seat (vec* mag dir))]
      (if (in-bounds? pos [width height])
        (if (contains? seats pos)
          (get seats pos)
          (recur (inc mag)))))))


(def part2-test-input1
  ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....")


(def part2-test-input2
  ".##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##.")


(def part2-test-input3
  ".............
.L.L.#.#.#.#.
.............")

(def dirs (for [dx [-1 0 1]
                dy [-1 0 1]
                :when (not (and (zero? dx) (zero? dy)))]
            [dx dy]))


(deftest test-occupied-in-dir?
  (doseq [d dirs]
    (is (nil? (occupied-in-dir? (init part2-test-input2) [3 3] d))))
  (doseq [d dirs]
    (is (true? (occupied-in-dir? (init part2-test-input1) [3 4] d))))
  (let [state (init part2-test-input3)]
    (is (false? (occupied-in-dir? state [1 1] [1 0])))
    (is (true? (occupied-in-dir? state [3 1] [1 0])))
    (is (false? (occupied-in-dir? state [3 1] [-1 0])))
    (doseq [d (disj (set dirs) [1 0])]
      (is (nil? (occupied-in-dir? state [1 1] d))))
    (doseq [d (disj (set dirs) [-1 0] [1 0])]
      (is (nil? (occupied-in-dir? state [3 1] d))))))


(defn seen-from
  [state seat]
  (frequencies
    (into []
          (comp (map #(occupied-in-dir? state seat %))
                (remove nil?))
          dirs)))


(defn next-state2
  [state]
  (update state
          :seats
          #(reduce (fn [next-seats [seat curr-state]]
                     (let [occupied (get (seen-from state seat) true 0)]
                       (assoc next-seats
                              seat 
                              (cond (and (not curr-state)
                                         (zero? occupied)) true
                                    (and curr-state
                                         (<= 5 occupied)) false
                                    :else curr-state))))
                   {}
                   %)))

(count-occupied (stabilize2 (init test-input)))
(count-occupied (stabilize2 (init real-input)))
(defn stabilize2
  [state]
  (let [nxt (next-state2 state)]
    (if (= state nxt)
      nxt
      (recur nxt))))

#_(->> (iterate next-state2 (init test-input))
       (take 100)
       (partition 2 1)
       (map #(apply = %))
       )








