(ns advent-of-code-2020.day21
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]))


(def test-input
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")


(defn parse-line
  [line]
  (transduce
    (remove empty?)
    (fn
      ([r] (dissoc r :mode))
      ([r word]
       (if (= word "contains")
         (assoc r :mode :allergens)
         (update r (:mode r) conj word))))
    {:ingredients #{} :allergens #{} :mode :ingredients}
    (string/split line #"\s|\(|,|\)")))


(defn parse-input
  [input]
  (map parse-line (string/split-lines input)))


(defn intersect-ingredients
  [p allergens ingredients]
  (reduce
    (fn [p allergen]
      (if (contains? p allergen)
        (update p allergen sets/intersection ingredients)
        (assoc p allergen ingredients)))
    p
    allergens))


(defn update-ingredient-counts
  [counts ingredients]
  (reduce (fn [counts ingredient]
            (update counts ingredient (fnil inc 0)))
          counts
          ingredients))


(defn process-food
  [v {:keys [allergens ingredients]}]
  (-> v
      (update :allergen->ingredients
              intersect-ingredients
              allergens
              ingredients)
      (update :ingredients
              update-ingredient-counts
              ingredients)))


(defn no-allergen-ingredients 
  [{:keys [allergen->ingredients ingredients]}]
  (reduce (fn [ingredients [_ is]]
            (apply dissoc ingredients is))
          ingredients
          allergen->ingredients))


(defn process-input
  [parsed-input]
  (reduce process-food 
          {:allergen->ingredients {} :ingredients {}}
          parsed-input))


#_(part1 test-input)
#_(part1 (slurp (io/resource "day21.txt")))
(defn part1
  [input]
  (->> (process-input (parse-input input))
       (no-allergen-ingredients)
       (vals)
       (reduce +)))


(defn update-potential
  [ingredient potential]
  (reduce (fn [potential [allergen potential-ingredients]]
            (assoc potential
                   allergen
                   (disj potential-ingredients ingredient)))
          potential
          potential))


#_(part2 test-input)
#_(println (part2 (slurp (io/resource "day21.txt"))))
(defn part2
  [input]
  (let [init (:allergen->ingredients (process-input (parse-input input)))
        one-val? #(= 1 (count (val %)))]
    (loop [known {}
           q (into (clojure.lang.PersistentQueue/EMPTY)
                   (filter one-val? init))
           potential init]
      (if (empty? q)
        (string/join "," (vals (sort-by key known)))
        (let [[xa pi] (peek q)
              xi (first pi)
              next-q (pop q)
              pot (update-potential xi potential)]
          (recur (assoc known xa xi)
                 (into next-q (filter one-val?) pot)
                 pot))))))

