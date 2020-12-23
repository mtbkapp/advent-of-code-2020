(ns advent-of-code-2020.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]))


; I'm smelling a constraint solver problem

(defn read-tile
  [tile]
  (let [[id-line & image-lines] (string/split-lines tile)]
    {:id  (second (re-matches #"^Tile ([0-9]+):$" id-line))
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


(defn rotate-right
  [{:keys [id top bottom right left]}]
  {:id id
   :top left 
   :right top
   :bottom right 
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
   :right (apply str (reverse right))
   :bottom top
   :left (apply str (reverse left))})


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
    (clojure.pprint/pprint (flip-vert tile))
    (clojure.pprint/pprint (-> tile flip-horiz (rotate-times 2)))
    #_(is (= (flip-vert tile)
           (-> tile flip-horiz (rotate-times 2))
           ))))



; is flipping vertically the same as flipping horizontally then rotating?


; algorithm 1
; assumptions:
; 1. a border of a piece only lines up with 1 other border on 1 other piece
;    or a reverse
; choose any piece to start with 
; put all others in a set or queue or something
; while there are remaining pieces:
;   choose a piece, try to align it with the current outer borders
;   if it aligns, integrate it
;   if not, put it back in the set

; if a border can align with a border on more than one piece then this won't
; work 


; challenge assumption
; are all borders unique?
; put all borders in a sequence together with the revesre, frequencies, count
; should only be 1 or 2 of each if the adjacent borders are unique
(->> (read-tiles real-input)
     (mapcat (juxt :top
                   :bottom
                   :right
                   :left
                   (comp reverse :top)
                   (comp reverse :bottom)
                   (comp reverse :right)
                   (comp reverse :left))
             (read-tiles real-input))
     frequencies
     vals
     frequencies)
; this does seem quite right

; how many unique borders are there anyway, 10 with either a . or a #, so 2^10
; 1024 unique borders
; how does our puzzle need?
; 12 on each side that do not match any other = 48
; 11 inside edges vertically and horizontall with 12 edges each so
; 2 * 11 * 12 = 264
; total = 264 + 49 = 312  
; so there are enough possible edge shapes, try it and see if it works?


; try taking one piece and see how all others can fit together with it
; if there is more than one on a single border then we're screwed

(defn find-simple-fit
  "Returns all the ways tile-x and tile-y can fit together without rotating
  or flipping either tile."
  [{:keys [free-sides] :as tile-x} {free-y :free-sides :as tile-y}]
  )

(defn all-orientations
  [tile]

  )


(defn find-fit
  "Returns all the ways tile-y can connect to tile-x. tile-x stays static. 
  tile-y can be rotated or flipped"
  [tile-x tile-y]
  #_(all-orientations tile-y)
  )



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





