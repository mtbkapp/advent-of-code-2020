(ns advent-of-code-2020.day24
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]))


; https://en.wikipedia.org/wiki/Hexagonal_Efficient_Coordinate_System#Description

(spec/def :hex/coord
  (spec/cat :array int? 
            :row int? 
            :col int?))


(defmulti move (fn [dir coord] dir))

(defmethod move :dir/west
  [_ [a r c]]
  [a r (dec c)])

(defmethod move :dir/east
  [_ [a r c]]
  [a r (inc c)])

(defmethod move :dir/north-east
  [_ [a r c]]
  (if (zero? a)
    [1 (dec r) c] 
    [0 r (inc c)]))

(defmethod move :dir/north-west
  [_ [a r c]]
  (if (zero? a)
    [1 (dec r) (dec c)]
    [0 r c]))

(defmethod move :dir/south-east
  [_ [a r c]]
  (if (zero? a)
    [1 r c] 
    [0 (inc r) (inc c)]))

(defmethod move :dir/south-west
  [_ [a r c]]
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
