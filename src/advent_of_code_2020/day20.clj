
(ns advent-of-code-2020.day20
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [clojure.test :refer :all]))


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
      {:id (Long/valueOf (second (re-matches #"^\w+\s([0-9]*):$" id-row)))
       :pixels pixels
       :transform identity
       :size (count pixels)})))

(deftest test-read-tile
  (let [{:keys [id pixels transform size] :as t} (read-tile test-tile4)]
    (is (= 4 id))
    (is (= (string/join \newline (rest (string/split-lines test-tile4)))
           (string/join \newline (map #(string/join %) pixels))))
    (is (fn? transform))
    (is (= [99 88] (transform [99 88])))
    (is (= 10 size))))


(defn read-input
  [input]
  (map read-tile (string/split input #"\n\n")))


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


(defn tile-pixels
  [{:keys [size] :as tile}]
  (->> (map (fn [y]
              (->> (range size)
                   (map (fn [x]
                          (get-pixel tile [x y])))
                   (string/join)))
            (range size))
       (string/join "\n")))


(deftest test-rotate-tile
  (testing "transform"
    (let [{xform :transform} (rotate-tile {:transform identity :size 3})]
      (is (= (xform [0 0]) [0 2]))
      (is (= (xform [1 0]) [0 1]))
      (is (= (xform [2 0]) [0 0]))
      (is (= (xform [0 1]) [1 2]))
      (is (= (xform [1 1]) [1 1]))
      (is (= (xform [2 1]) [1 0]))
      (is (= (xform [0 2]) [2 2]))
      (is (= (xform [1 2]) [2 1]))
      (is (= (xform [2 2]) [2 0]))))
  (testing "single rotate, test-tile"
    (let [tr (rotate-tile (read-tile "Tile 3:\nABC\nDEF\nGHI"))]
      (is (= "GDA\nHEB\nIFC"
             (tile-pixels tr)))))
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
  (testing "xform"
    (let [{xform :transform} (flip-tile {:transform identity :size 3})]
      (is (= (xform [0 0]) [0 2]))
      (is (= (xform [1 0]) [1 2]))
      (is (= (xform [2 0]) [2 2]))
      (is (= (xform [0 1]) [0 1]))
      (is (= (xform [1 1]) [1 1]))
      (is (= (xform [2 1]) [2 1]))
      (is (= (xform [0 2]) [0 0]))
      (is (= (xform [1 2]) [1 0]))
      (is (= (xform [2 2]) [2 0]))))
  (testing "single flip"
    (let [tr (flip-tile (read-tile "Tile 3:\nABC\nDEF\nGHI"))]
      (is (= "GHI\nDEF\nABC"
             (tile-pixels tr)))))
  (testing "single flip borders"
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


(defn adjacent-positions
  [[x y]]
  [[(- x 1) y]
   [(+ x 1) y]
   [x (- y 1)]
   [x (+ y 1)]])


(spec/def ::board (spec/map-of ::coord ::tile))


(defn init-board
  [first-tile]
  {[0 0] first-tile})


(defn potential-positions
  [board]
  (->> (keys board)
       (mapcat adjacent-positions)
       (remove (partial contains? board))))


(defn get-adj-borders
  [[x y] [ax ay]]
  (cond (and (= x ax) (= y (inc ay))) [:top :bottom]
        (and (= x (inc ax)) (= y ay)) [:left :right]
        (and (= x ax) (= y (dec ay))) [:bottom :top]
        (and (= x (dec ax)) (= y ay)) [:right :left]))

(deftest test-get-adj-borders
  (let [p [0 0]
        top [0 -1]
        left [-1 0]
        bottom [0 1]
        right [1 0]
        bad [2 0]]
    (is (= [:top :bottom] (get-adj-borders p top)))
    (is (= [:left :right] (get-adj-borders p left)))
    (is (= [:bottom :top] (get-adj-borders p bottom)))
    (is (= [:right :left] (get-adj-borders p right)))
    (is (nil? (get-adj-borders p bad)))))


(defn fits?
  [board tile pos]
  (->> (adjacent-positions pos)
       (filter (partial contains? board))
       (every? (fn [adj-pos]
                 (let [[border adj-border] (get-adj-borders pos adj-pos)]
                   (= (get-border tile border)
                      (get-border (get board adj-pos) adj-border)))))))

(deftest test-fits?
  (let [up (read-tile "Tile 1:\n...\n...\n###")
        left (read-tile "Tile 2:\n..#\n...\n...")
        bottom (read-tile "Tile 3:\n.#.\n...\n...")
        right (read-tile "Tile 4:\n#..\n...\n...")
        fit-all (read-tile "Tile 5:\n###\n...\n.#.")
        blank (read-tile "Tile 6:\n...\n...\n...")
        fit-none (rotate-tile fit-all)
        missing-one (read-tile "Tile 7:\n###\n...\n...")
        board {[0 -1] up
               [-1 0] left
               [0 1] bottom
               [1 0] right}]
    (testing "all sides checked"
      (is (fits? board fit-all [0 0])))
    (testing "only sides that touch are checked"
      (is (fits? board blank [0 2]))
      (is (fits? board blank [-2 0]))
      (is (fits? board blank [0 -2]))
      (is (fits? board blank [2 0])))
    (testing "when no touching sides fit"
      (is (not (fits? board fit-none [0 0])))
      (is (not (fits? board missing-one [0 0]))))))


(defn find-pos
  [board tile]
  (first (for [t (all-orientations tile)
               p (potential-positions board)
               :when (fits? board t p)]
           [t p])))


(deftest test-find-pos
  (let [up (read-tile "Tile 1:\n...\n...\n###")
        left (read-tile "Tile 2:\n..#\n...\n...")
        bottom (read-tile "Tile 3:\n.#.\n...\n...")
        right (read-tile "Tile 4:\n#..\n...\n...")
        board {[0 -1] up
               [-1 0] left
               [0 1] bottom
               [1 0] right}
        fits (read-tile "Tile 5:\n###\n...\n.#.")
        fit-none (read-tile "Tile 6:\n###\n###\n###")
        expected? (fn [[ex-tile ex-pos] [actual-tile actual-pos]]
                    (and (= (tile-pixels ex-tile)
                            (tile-pixels actual-tile))
                         (= ex-pos actual-pos)))]
    (testing "a position exists"
      (is (expected? [fits [0 0]] (find-pos board fits)))
      (is (expected? [fits [0 0]] (find-pos board (-> fits rotate-tile))))
      (is (expected? [fits [0 0]] (find-pos board (-> fits rotate-tile rotate-tile flip-tile)))))
    (testing "a position does not exist"
      (is (nil? (find-pos board fit-none))))))


(defn place-tile
  [board tile p]
  (assoc board p tile))


(defn solve-puzzle
  [[first-tile & tiles]]
  (loop [q (into (clojure.lang.PersistentQueue/EMPTY) tiles)
         board (init-board first-tile)]
    (if (empty? q)
      board
      (let [next-tile (peek q)
            next-q (pop q)]
        (if-let [[t p] (find-pos board next-tile)]
          (recur next-q (place-tile board t p))
          (recur (conj next-q next-tile) board))))))


(defn find-corner
  [board corner]
  (let [op (if (= corner :top-left) < >)]
    (reduce (fn [[mx my :as acc] [x y :as p]]
              (if (or (op x mx) (op y my))
                p
                acc))
            (keys board))))


(defn find-corners
  [board]
  (let [[tlx tly] (find-corner board :top-left)
        [brx bry] (find-corner board :bottom-right)]
    {:top-left [tlx tly]
     :bottom-left [tlx bry]
     :bottom-right [brx bry]
     :top-right [brx tly]}))


(defn product-of-corner-ids
  [board]
  (transduce (map #(:id (get board %)))
             *
             (vals (find-corners board))))


(defn solve-part1
  [input]
  (-> input
      read-input
      solve-puzzle
      product-of-corner-ids))


(def real-input (slurp (io/resource "day20.txt")))


(deftest test-solve-part1
  (is (= 20899048083289 (solve-part1 test-input)))
  (is (= 66020135789767 (solve-part1 real-input))))


(defn vec+
  [v0 v1]
  (mapv + v0 v1))


(defn vec-scale
  [v s]
  (mapv #(* s %) v))


(defn combine-tiles 
  [board]
  (let [tile-size (-> board first val :size)
        [min-x min-y :as top-left] (find-corner board :top-left)
        [max-x max-y] (find-corner board :bottom-right)
        board-width (- (inc max-x) min-x)
        board-height (- (inc max-y) min-y)
        new-tile-size (- tile-size 2)
        combined-tile-size (* new-tile-size board-height)
        pixel-array (make-array Character/TYPE
                                combined-tile-size 
                                combined-tile-size)
        tile-offset (vec-scale top-left -1)]
    (assert (= board-width board-height))
    (doseq [[tile-coord tile] board]
      (doseq [y (range 1 (inc new-tile-size))]
        (doseq [x (range 1 (inc new-tile-size))]
          (let [[ax ay] (-> tile-coord
                            (vec+ tile-offset)
                            (vec-scale new-tile-size)
                            (vec+ [x y])
                            (vec+ [-1 -1]))]
            (aset pixel-array ay ax (get-pixel tile [x y]))))))
    {:id -1
     :pixels (mapv vec pixel-array)
     :transform identity
     :size combined-tile-size}))


(defn prepend-tile-xform
  [tile xform]
  (update tile :transform #(comp % xform)))


(defn remove-border
  [tile]
  (-> tile
      (prepend-tile-xform #(vec+ % [1 1]))
      (update :size - 2)))


(deftest test-remove-border
  (let [t (read-tile "Tile 1:\n#####\n#ABC#\n#DEF#\n#GHI#\n#####")
        tb (remove-border t)
        te (read-tile "Tile 2:\nABC\nDEF\nGHI")]
    (is (= (tile-pixels te)
           (tile-pixels tb))))
  (let [t1951 (read-tile "Tile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..")
        expected ".#.#..#.\n###....#\n##.##.##\n###.####\n##.#....\n...#####\n....#..#\n.####..."]
    (is (= (-> t1951 flip-tile remove-border tile-pixels)
           expected))))

(defn prep-board
  "Translates the board coordinates so the top-left is at [0 0].
  Removes the border from each tile."
  [board]
  (let [offset (vec-scale (find-corner board :top-left) -1)]
    (into {}
          (map (juxt (comp (partial vec+ offset) key)
                     (comp remove-border val)))
          board)))

(deftest test-prep-board
  (let [board (-> test-input read-input solve-puzzle)
        ordered-ids (fn [b]
                      (let [[min-x min-y] (find-corner b :top-left) 
                            [max-x max-y] (find-corner b :bottom-right)]
                        (mapcat (fn [y]
                                  (map (fn [x]
                                         (get-in b [[x y] :id]))
                                       (range min-x (inc max-x))))
                                (range min-y (inc max-y)))))
        pd-board (prep-board board)
        top-left (find-corner pd-board :top-left)]
    (is (= [0 0] top-left))
    (is (= (ordered-ids board)
           (ordered-ids pd-board)))))


(defn convert-board-to-tile
  [board]
  (let [[max-x max-y] (find-corner board :bottom-right)
        board-size (inc max-x)
        tile-size (-> board first val :size)
        size (* board-size tile-size)
        pixel-array (make-array Character/TYPE size size)]
    (assert (= max-x max-y) "board is not square!")
    (doseq [[tile-coord tile] board]
      (doseq [y (range tile-size)]
        (doseq [x (range tile-size)]
          (let [p [x y]
                [ax ay] (vec+ (vec-scale tile-coord tile-size) p)
                pixel (get-pixel tile p)]
            (aset pixel-array ay ax (get-pixel tile p))))))
    {:id -1
     :size size
     :transform identity
     :pixels (mapv vec pixel-array)}))


(def test-combined-pixels
  ".#.#..#.##...#.##..#####
###....#.#....#..#......
##.##.###.#.#..######...
###.#####...#.#####.#..#
##.#....#.##.####...#.##
...########.#....#####.#
....#..#...##..#.#.###..
.####...#..#.....#......
#..#.##..#..###.#.##....
#.####..#.####.#.#.###..
###.#.#...#.######.#..##
#.####....##..########.#
##..##.#...#...#.#.#.#..
...#..#..#.#.##..###.###
.#.#....#.##.#...###.##.
###.#...#..#.##.######..
.#.#.###.##.##.#..#.##..
.####.###.#...###.#..#.#
..#.#..#..#.#.#.####.###
#..####...#.#.#.###.###.
#####..#####...###....##
#.##..#..#...#..####...#
.#.###..##..##..####.##.
...###...##...#...#..###")


(deftest test-convert-board-to-tile 
  (let [combined-tile (-> (read-input test-input)
                          solve-puzzle
                          prep-board
                          convert-board-to-tile)]
    (is (some (fn [t]
                (= test-combined-pixels (string/trim (tile-pixels t))))
              (all-orientations combined-tile)))))


(def monster-template
  "                  #
#    ##    ##    ###
 #  #  #  #  #  #")


(def monster-coords
  (into [] 
        (comp (map-indexed (fn [y line]
                             (map-indexed (fn [x c]
                                            (if (= c \#)
                                              [x y]))
                                          line)))
              cat
              (remove nil?))
        (string/split-lines monster-template)))


(defn monster-at?
  [tile coord]
  (every? (fn [c]
            (= \# (get-pixel tile (vec+ coord c))))
          monster-coords))


(def test-monster-tile 
  "Tile 42:
......................
......................
......................
...................#..
.#....##....##....###.
..#..#..#..#..#..#....
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................")


(deftest test-monster-at?
  (let [t (read-tile test-monster-tile)]
    (is (not (monster-at? t [0 0])))
    (is (monster-at? t [1 3]))))


(defn write-pixel
  [{:keys [transform] :as tile} pos pixel]
  (let [[x y] (transform pos)]
    (assoc-in tile [:pixels y x] pixel)))


(defn mark-monsters-at
  [tile pos]
  (reduce (fn [t p]
            (write-pixel t (vec+ pos p) \0))
          tile
          monster-coords))


(defn mark-monsters
  [{:keys [size] :as tile}]
  (reduce (fn [t pos]
            (if (monster-at? tile pos)
              (mark-monsters-at t pos)
              t))
          tile
          (for [y (range size) x (range size)] [x y])))


(deftest test-mark-monsters
  (let [t (read-tile test-monster-tile)
        tr (rotate-tile t)]
    (is (every? #(not= \# %)
                (tile-pixels (mark-monsters t))))
    (is (tile-pixels tr)
        (tile-pixels (mark-monsters tr)))))


(defn roughness
  [t]
  (-> t 
      mark-monsters
      tile-pixels
      frequencies
      (get \# 0)))


(defn min-roughness
  [t]
  (transduce (map roughness)
             min
             Long/MAX_VALUE
             (all-orientations t)))


(defn solve-part2
  [input]
  (-> (read-input input)
      solve-puzzle
      prep-board
      convert-board-to-tile
      min-roughness))


(deftest test-solve-part2
  (is (= 273 (solve-part2 test-input)))
  (is (= 1537 (solve-part2 real-input))))


