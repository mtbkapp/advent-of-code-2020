(ns advent-of-code-2020.day20b
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [clojure.test :refer :all]))



; Algorithm for part 1

; How many orientations can a tile have? 
; => 8
; => start, rotate, rotate, rotate, flip, rotate, rotate, rotate


; tiles behaviors 
; rotate - rotate a tile to the right
; flip - flip a tile over (just pick horizontally or vertically) 
; get-top - get the pixels along the top edge
; get-bottom
; get-left
; get-right




; main algorithm
; input: a set of tiles
; output: a board with all the tiles on it 
; read all the tiles each in a convenient data structure to support the above.
; put all tiles in a queue (in any order), Q
; pull the first tile out and place on the board
; do until Q is empty:
;   pull a tile from the Q, t
;   try to find a place on the board for t,
;     this is a cross product between orientations of t and the border space of the board
;   if a place is found, put it there, goto top of loop
;   if a place is not found, put t at the end of Q and goto top of loop
; normalize coordinates on the board
; maybe add some way to bail out in the case the tiles don't fit together?



; fits algorithm
; input: the board, a tile 
; output: any coordinate where the tile fits or nil if it doesn't fit anywhere 
; all = all orientations of the tile
; border positions = positions on the board where there isn't a tile but a tile
;                    is adjacent
; something like
; (some
;   (for [t all
;         p border-positions]
;     (if (fits? board t p)
;       p)))
;


; assumptions
; 1. each tile can only fit on the board in a single way 
; 2. there is only one way to fit the tiles together





; TILES
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

(defn square-vector?
  [v]
  (let [c (count v)]
    (and (vector? v)
         (every? vector? v)
         (every? #(= c (count %)) v))))


(spec/def ::tile (spec/keys :req-un [:tile/id :tile/pixels :tile/transform :tile/size]))
(spec/def :tile/id (spec/and string? not-empty))
(spec/def :tile/pixels square-vector?)
(spec/def :tile/size pos-int?)

(spec/def ::coord (spec/tuple int? int?))
(spec/def :tile/transform (spec/fspec :args (spec/cat :coord ::coord)
                                      :ret ::coord))


(defn read-tile
  [s]
  (let [[id-row & pixel-rows] (string/split-lines s)]
    (let [pixels (mapv vec pixel-rows)]
      {:id (second (re-matches #"^\w+\s([0-9]*):$" "Tile 4:"))
       :pixels pixels
       :transform identity
       :size (count pixels)})))


(deftest test-read-tile
  (let [{:keys [id pixels transform size] :as t} (read-tile test-tile4)]
    (is (= "4" id))
    (is (= (string/join \newline (rest (string/split-lines test-tile4)))
           (string/join \newline (map #(string/join %) pixels))))
    (is (fn? transform))
    (is (= [99 88] (transform [99 88])))
    (is (= 10 size))))


(defn get-pixel
  [{:keys [pixels transform] :as tile} coord]
  (let [[x y] (transform coord)]
    (get-in pixels [y x])))


(defn border-vec-fn
  [{:keys [size] :as tile} border]
  (case border
    :top #(vector % 0)
    :left #(vector 0 %)
    :bottom #(vector % (dec size))
    :right #(vector (dec size) %)))


(defn get-border
  [{:keys [size] :as tile} border]
  (map (comp (partial get-pixel tile)
             (border-vec-fn tile border))
       (range 0 size)))


(deftest test-get-border
  (let [tile (read-tile test-tile3)]
    (is (= (seq "..###..###") (get-border tile :top)))
    (is (= (seq ".#..#####.") (get-border tile :left)))
    (is (= (seq "..##.#..#.") (get-border tile :bottom)))
    (is (= (seq "#..##.#...") (get-border tile :right)))))


(defn append-tile-xform 
  [tile xform]
  (update tile :transform #(comp xform %)))


(defn rotate-tile
  [{:keys [size] :as tile}]
  (append-tile-xform tile 
                     (fn [[x y]]
                       [y (- (dec size) x)])))


(deftest test-rotate-tile
  (testing "single rotate"
    (let [t (read-tile test-tile3)
          tr (rotate-tile t)]
      (is (= (reverse (get-border t :left)) (get-border tr :top)))
      (is (= (get-border t :bottom) (get-border tr :left)))
      (is (= (reverse (get-border t :right)) (get-border tr :bottom)))
      (is (= (get-border t :top) (get-border tr :right)))))
  (testing "can rotate around back to start"
    (let [t0 (read-tile test-tile3)
          t1 (-> t0 rotate-tile rotate-tile rotate-tile rotate-tile)]
      (doseq [border [:top :left :bottom :right]]
        (is (= (get-border t0 border)
               (get-border t1 border))
            border)))))


(defn flip-tile
  [{:keys [size] :as tile}]
  (append-tile-xform tile 
                     (fn [[x y]] 
                       [x (- (dec size) y)])))


(deftest test-flip-tile
  (testing "single flip"
    (let [t (read-tile test-tile3)
          tf (flip-tile t)]
      (is (= (get-border t :bottom) (get-border tf :top)))
      (is (= (reverse (get-border t :left)) (get-border tf :left)))
      (is (= (get-border t :top) (get-border tf :bottom)))
      (is (= (reverse (get-border t :right)) (get-border tf :right)))))
  (testing "flip back to start"
    (let [t0 (read-tile test-tile3)
          t1 (-> t0 flip-tile flip-tile)]
      (doseq [border [:top :left :bottom :right]]
        (is (= (get-border t0 border)
               (get-border t1 border))
            border)))))


(deftest test-rotate-flip-tile
  (let [t0 (read-tile test-tile3)
        t1 (-> t0
               rotate-tile
               rotate-tile
               rotate-tile
               flip-tile
               rotate-tile
               rotate-tile
               rotate-tile
               flip-tile)]
    (doseq [border [:top :left :bottom :right]]
      (is (= (get-border t0 border)
             (get-border t1 border))
          border))))


(defn all-orientations
  [tile]
  (let [t0 tile
        t1 (rotate-tile t0)
        t2 (rotate-tile t1)
        t3 (rotate-tile t2)
        t4 (flip-tile t3)
        t5 (rotate-tile t4)
        t6 (rotate-tile t5)
        t7 (rotate-tile t6)]
    [t0 t1 t2 t3 t4 t5 t6 t7]))
