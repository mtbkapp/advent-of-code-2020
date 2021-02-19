(ns advent-of-code-2020.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]))


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



(defn read-tile
  [tile]
  (let [[id-line & image-lines] (string/split-lines tile)]
    {:id (Long/valueOf (second (re-matches #"^Tile ([0-9]+):$" id-line)))
     :top (first image-lines)
     :bottom (last image-lines)
     :right (apply str (map last image-lines))
     :left (apply str (map first image-lines))
     :image-lines image-lines}))


(defn read-tiles
  [input]
  (map read-tile (string/split input #"\n\n")))


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


(defn solve-puzzle
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


(defn find-corner
  [coords gt-or-lt]
  (reduce (fn [[mx my :as acc] [x y :as p]]
            (if (or (gt-or-lt x mx) (gt-or-lt y my))
              p
              acc))
          coords))


(defn product-of-corner-ids 
  [{:keys [tiles] :as state}]
  (let [[tlx tly :as tl] (find-corner (keys tiles) <)
        [brx bry :as br] (find-corner (keys tiles) >) 
        tr [brx tly]
        bl [tlx bry]]
    (reduce (fn [product pos]
              (* product (:id (get tiles pos))))
            1
            [tl br tr bl])))


(defn solve-part1
  [input]
  (-> (read-tiles input)
      (solve-puzzle)
      (product-of-corner-ids)))


(deftest test-solve-part1
  (is (= 20899048083289 (solve-part1 test-input)))
  (is (= 66020135789767 (solve-part1 real-input))))


(defn coords-to-ids
  [{:keys [tiles] :as solution}]
  (reduce (fn [acc [coord {:keys [id]}]]
            (assoc acc coord id) )
          {}
          tiles))


(defn index-by
  [f xs]
  (reduce (fn [idx x]
            (assoc idx (f x) x))
          {}
          xs))


(defn vec-scale 
  [[x y] s]
  [(* s x) (* s y)])


(defn vec+
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


(defn normalize
  [tiles]
  (let [top-left (find-corner (keys tiles) <)
        inverse (vec-scale top-left -1)]
    (reduce (fn [acc [k t]]
              (assoc acc (vec+ k inverse) t))
            {}
            tiles)))


#_(def arr (solution->array (index-by :id (read-tiles real-input)) solution))
(defn solution->array
  [all-tiles solution]
  (let [norm-tiles (normalize (coords-to-ids solution))
        array (make-array Character/TYPE 120 120)]
    (doseq [[tile-coord tile-id] norm-tiles]
      (doseq [[y line] (map-indexed vector (:image-lines (get all-tiles tile-id)))]
        (doseq [[x c] (map-indexed vector line)]
          (let [base (vec-scale tile-coord 10)
                [cx cy] (vec+ base [x y])]
            (aset array cy cx c)))))
    array))

#_(let [freq (atom {})]
  (doseq [a (seq arr)]
    (doseq [c (seq a)]
      (swap! freq update c (fnil inc 0)) ))
  @freq)

(defn monster-pattern->coords
  [pattern]
  (->> (string/split-lines pattern)
       (mapcat (fn [y line]
                 (map-indexed (fn [x c]
                                (if (= c \#)
                                  [x y]) )
                              line))
               (range))
       (filter some?)
       set))


(def monster-pattern
  (monster-pattern->coords "                  # 
#    ##    ##    ###
 #  #  #  #  #  #"))


(defn monster-at?
  [whole-arr pos]
  (every? (fn [[x y]]
            (and (< x 120)
                 (< y 120)
                 (= \# (aget whole-arr y x))))
          (map #(vec+ pos %) monster-pattern)))


(def whole-coords
  (for [x (range 120)
        y (range 120)]
    [x y]))


#_(count-monsters arr)
(defn count-monsters
  [whole-arr]
  (reduce (fn [c pos]
            (if (monster-at? whole-arr pos)
              (inc c)
              c))
          0
          whole-coords))

; top row -> right column
; [0 0] [119 0]
; [1 0] [119 1]
; [2 0] [119 2]
; [3 0] [119 3]
; ....
; second row -> second to right column
; [0 1] [118 0]
; [1 1] [118 1]
; [2 1] [118 2]

; bottom row  -> left column
; [0 119] [0 0]
; [1 119] [0 1]
; [2 119] [0 2]

(defn rotate-array-right-vec
  [[x y]]
  [(- 119 y) x])

#_(test-rotate-array-right-vec)
(deftest test-rotate-array-right-vec
  (is (= [119 0] (rotate-array-right-vec [0 0])))
  (is (= [119 1] (rotate-array-right-vec [1 0])))
  (is (= [119 2] (rotate-array-right-vec [2 0])))
  (is (= [118 0] (rotate-array-right-vec [0 1])))
  (is (= [118 1] (rotate-array-right-vec [1 1])))
  (is (= [118 2] (rotate-array-right-vec [2 1])))
  (is (= [0 0] (rotate-array-right-vec [0 119])))
  (is (= [0 1] (rotate-array-right-vec [1 119])))
  (is (= [0 2] (rotate-array-right-vec [2 119]))))


#_(count-monsters (rotate-array-right arr))
(defn rotate-array-right
  [in]
  (let [out (make-array Character/TYPE 120 120)]
    (doseq [[x y :as pos] whole-coords]
      (let [[rx ry] (rotate-array-right-vec pos)]
        (aset out ry rx (aget in y x))))
    out))


(comment
  (def solution (-> (read-tiles real-input) (solve-puzzle)))
  (type solution)
  (keys solution)
  (coords-to-ids solution)
  (index-by :id (read-tiles real-input))

  (solution->array (index-by :id (read-tiles real-input)) solution)


  ; shows that the solution is rectangular and doesn't have missing tiles
  (let [coords (keys (:tiles solution))
        [tlx tly :as tl] (find-corner coords <)
        [brx bry :as br] (find-corner coords >)]
    (prn tl br)
    (count
    (for [x (range tlx (inc brx))
          y (range tly (inc bry))]
      [x y]
      )))

  ; scheme to find [x y] in solution?
  ; or combine all tiles 


  (-> (read-tiles real-input)
      (first)
      :image-lines
      first
      count
      )
  
  ; each tile is 10x10
  ; 12x12 tiles
  ; [-2 -5] to [9 6]

  ; normalize tiles


  ; 120 by 120 array of chars



  (def solution2 (normalize (coords-to-ids solution)))
  (def index-tiles (index-by :id (read-tiles real-input)))

  
  (let [m (make-array Character/TYPE 120 120)
        _ (aset m 100 100 (char 33))
        x (aget m 100 100)
        ]
    (prn ">>>>>>" x "<<<<<")
    )

  
  )
