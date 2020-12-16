(ns advent-of-code-2020.day15)


(defn first? 
  [spoken last-num i]
  (if-let [is (get spoken last-num)]
    (and (= 1 (count is))
         (= (dec i) (peek is)))
    true))


(defn update-spoken
  [spoken n i]
  (update spoken n conj i))


(defn init-spoken
  [starting-nums]
  (into {} 
        (map-indexed (fn [i n]
                       [n [(inc i)]]))
        starting-nums))

(defn next-n
  [spoken last-num]
  (let [spoken-at (get spoken last-num)
        i2 (peek spoken-at)
        i1 (peek (pop spoken-at))]
    (- i2 i1)))


(defn find-nth
  [starting-nums iter]
  (loop [spoken (init-spoken starting-nums) 
         last-num (last starting-nums)
         i (inc (count starting-nums))]
    (if (<= (inc iter) i)
      last-num
      (if (first? spoken last-num i)
        (recur (update-spoken spoken 0 i) 0 (inc i))
        (let [n (next-n spoken last-num)]
          (recur (update-spoken spoken n i) n (inc i)))))))


#_(prn (solve-part1 [0 3 6]))
#_(prn (solve-part1 [2 1 3]))
#_(prn (solve-part1 [1 2 3]))
#_(prn (solve-part1 [2 3 1]))
#_(prn (solve-part1 [3 2 1]))
#_(prn (solve-part1 [3 1 2]))
#_(prn (solve-part1 [14 3 1 0 9 5]))
(defn solve-part1
  [starting-nums]
  (find-nth starting-nums 2020))


#_(time (prn (solve-part2 [14 3 1 0 9 5])))
(defn solve-part2
  [starting-nums]
  (find-nth starting-nums 30000000))




