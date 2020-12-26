(ns advent-of-code-2020.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]))


(defn read-tile
  [tile]
  (let [[id-line & image-lines] (string/split-lines tile)]
    {:id (Long/valueOf (second (re-matches #"^Tile ([0-9]+):$" id-line)))
     :top (first image-lines)
     :bottom (last image-lines)
     :right (apply str (map last image-lines))
     :left (apply str (map first image-lines))}))


(def test-tile "Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...")

(defn reverse-edge
  [edge]
  (apply str (reverse edge)))


(defn rotate-right
  [{:keys [id top bottom right left]}]
  {:id id
   :top (reverse-edge left)
   :right top
   :bottom (reverse-edge right)
   :left bottom})


(defn rotate-times
  [tile times]
  (if (zero? times)
    tile
    (recur (rotate-right tile) (dec times))))


(deftest test-rotate-times
  (let [tile (read-tile test-tile)]
    (is (= tile (-> tile
                    rotate-right
                    rotate-right
                    rotate-right
                    rotate-right)))
    (is (= tile (rotate-times tile 4)))))


(defn flip-horiz
  "Flip tile across the horizontal axis"
  [{:keys [id top bottom right left]}]
  {:id id
   :top bottom
   :right (reverse-edge right)
   :bottom top
   :left (reverse-edge left)})


(defn flip-vert
  "File tile across the vertical axis"
  [{:keys [id top bottom right left]}]
  {:id id
   :top (apply str (reverse top))
   :right left
   :bottom (apply str (reverse bottom))
   :left right})


(deftest test-flip
  (let [tile (read-tile test-tile)]
    (is (= tile (-> tile flip-horiz flip-horiz)))
    (is (= tile (-> tile flip-vert flip-vert)))
    (is (= (flip-vert tile)
           (-> tile flip-horiz (rotate-times 2))))
    (is (= (flip-horiz tile)
           (-> tile flip-vert (rotate-times 2))))))


(defn all-orientations
  "Return all the orientations that the given tile can be in"
  [tile]
  (let [t0 tile
        t1 (rotate-right t0)
        t2 (rotate-right t1)
        t3 (rotate-right t2)
        t4 (flip-vert t3)
        t5 (rotate-right t4)
        t6 (rotate-right t5)
        t7 (rotate-right t6)]
    [t0 t1 t2 t3 t4 t5 t6 t7]))


(def touching-sides
  {:top :bottom
   :right :left
   :bottom :top
   :left :right})


(defn adjacent-cells
  [[x y]]
  [[(- x 1) y]
   [(+ x 1) y]
   [x (- y 1)]
   [x (+ y 1)]])


(defn init-state
  [first-tile]
  {:border (set (adjacent-cells [0 0]))
   :tiles {[0 0] first-tile}})


(defn get-relative-pos
  "Get the relative position (:top :bottom :left :right) of b from a."
  [[ax ay :as a] [bx by :as b]]
  (cond (and (= ax bx) (= ay (dec by))) :top
        (and (= ax bx) (= ay (inc by))) :bottom
        (and (= ax (inc bx) ) (= ay by)) :left
        (and (= ax (dec bx)) (= ay by)) :right))


(deftest test-get-relative-pos
  (is (= :top (get-relative-pos [7 7] [7 8])))
  (is (= :bottom (get-relative-pos [7 7] [7 6])))
  (is (= :left (get-relative-pos [7 7] [6 7])))
  (is (= :right (get-relative-pos [7 7] [8 7]))))


(defn fits?
  "Does tile fit at pos given the positions of the other tiles?"
  [tiles pos tile]
  (let [adjs (filter #(contains? tiles %) (adjacent-cells pos))]
    (if (empty? adjs)
      (throw (ex-info "Bug in border probably no adjacent cells"
                      {:tiles tiles
                       :pos pos
                       :tile tile}))
      (every? (fn [adj-pos]
                (let [rel-pos (get-relative-pos pos adj-pos)
                      adj-tile (get tiles adj-pos)]
                  (= (get tile rel-pos)
                     (get adj-tile (get touching-sides rel-pos)))))
              adjs))))

(def test-tile2
  "Tile 2:
#...##.#..
..#.#..#.#
.###....#.
###.##.##.
.###.#####
.##.#....#
#...######
.....#..##
#.####...#
#.##...##.")


(def test-tile3
  "Tile 3:
..###..###
###...#.#.
..#....#..
.#.#.#..##
##...#.###
##.##.###.
####.#...#
#...##..#.
##..#.....
..##.#..#.")

(def test-tile4
  "Tile 4:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...")


(deftest test-fits?
  (is (fits? {[0 0] (read-tile test-tile2)}
             [1 0]
             (read-tile test-tile3)))
  (is (fits? {[0 0] (read-tile test-tile3)}
             [1 0]
             (read-tile test-tile4)))
  (is (not (fits? {[0 0] (read-tile test-tile3)}
                  [1 0]
                  (read-tile test-tile3)))))


(defn potential-positions
  [border tiles t]
  ; stop at first fits? ?
  (for [pos border
        tile (all-orientations t)
        :when (fits? tiles pos tile)]
    [pos tile]))


(deftest test-potential-positions
  (testing "different orientation not needed"
    (let [{:keys [border tiles]} (init-state (read-tile test-tile2))
          t (read-tile test-tile3)
          [p & ps] (potential-positions border tiles t)]
      (is (some? p))
      (is (empty? ps))
      (is (= [1 0] (first p)))
      (is (= t (second p)))))
  (testing "needs different rotation"
    (let [{:keys [border tiles]} (init-state (read-tile test-tile2))
          orig-t (read-tile test-tile3)
          t (-> orig-t flip-vert rotate-right)
          [p & ps] (potential-positions border tiles t)]
      (is (some? p))
      (is (empty? ps))
      (is (= [1 0] (first p)))
      (is (= orig-t (second p)))))
  (testing "switch pieces"
    (let [{:keys [border tiles]} (init-state (read-tile test-tile3))
          t (read-tile test-tile2)
          [p & ps] (potential-positions border tiles t)]
      (is (some? p))
      (is (empty? ps))
      (is (= [-1 0] (first p)))
      (is (= t (second p)))))
  (testing "no fit"
    (let [{:keys [border tiles]} (init-state (read-tile test-tile2))
          t (read-tile test-tile4)
          ps (potential-positions border tiles t)]
      (is (empty? ps)))))


; border = (set of cells adjacent to tiles) - (set of tile positions)
(defn integrate
  [state [pos tile]]
  (-> state
      (update :tiles assoc pos tile)
      (update :border disj pos)
      (update :border
              into
              (remove #(contains? (:tiles state) %)
                      (adjacent-cells pos)))))


(deftest test-integrate
  (let [t0 (read-tile test-tile2)
        t1 (read-tile test-tile3)
        {:keys [border tiles] :as state} (init-state t0)
        [p & ps] (potential-positions border tiles t1)
        next-state (integrate state p)]
      (is (some? p))
      (is (empty? ps))
      (is (= #{[-1 0] [1 1] [1 -1] [2 0] [0 -1] [0 1]}
             (:border next-state)))
      (is (= {[0 0] t0 [1 0] t1}
             (:tiles next-state)))))


(defn add-tile
  [{:keys [border tiles] :as state} t]
  (let [ps (potential-positions border tiles t)]
    (cond (empty? ps) [false state]
          (= 1 (count ps)) [true (integrate state (first ps))]
          :else (throw (ex-info "more than 1 potential position!"
                                {:potentials ps
                                 :state state
                                 :input-tile t})))))


#_(solve (read-tiles test-input))
(defn solve
  [[start-tile & rest-tiles]]
  (loop [state (init-state start-tile)
         queue (into (clojure.lang.PersistentQueue/EMPTY) rest-tiles)]
    (if (empty? queue)
      state
      (let [t (peek queue)
            nq (pop queue)
            [added? next-state] (add-tile state t)]
        (if added?
          (recur next-state nq)
          (recur next-state (conj nq t)))))))


#_(= (find-corners (solve (read-tiles test-input)))
     20899048083289)
(defn find-corners
  [{:keys [tiles] :as state}]
  (let [[tlx tly :as tl] (reduce (fn [[mx my :as acc] [x y :as p]]
                                   (if (or (< x mx) (< y my))
                                     p
                                     acc))
                                 (keys tiles))
        [brx bry :as br] (reduce (fn [[mx my :as acc] [x y :as p]]
                                   (if (or (< mx x) (< my y))
                                     p
                                     acc))
                                 (keys tiles))
        tr [brx tly]
        bl [tlx bry]]
    (reduce (fn [product pos]
              (* product (:id (get tiles pos))))
            1
            [tl br tr bl])))



#_(count (read-tiles test-input))
#_(count (read-tiles real-input))
(defn read-tiles
  [input]
  (map read-tile (string/split input #"\n\n")))

(def real-input (slurp (io/resource "day20.txt")))

(def test-input
  "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...")





