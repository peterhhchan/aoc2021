(ns aoc2021.day11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-long [s]  (Long/parseLong s))
(defn str-to-ints [s]
  (->> (re-seq #"\d" s)
       (mapv parse-long)))

(defn data []
  (->> (slurp "data/day11.txt")
       (str/split-lines)
       (mapv str-to-ints)))

(defn test-data []
  (->> "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"
       (str/split-lines)
       (mapv str-to-ints)))

(defn board [grid]
  ;; Debugging function
  (for [x (range 10)]
    (for [y (range 10)]
      (grid [x y]))))

(defn adjs [[x y]]
  (->> (for [a [-1 0 1]
             b [-1 0 1]]
         [(+ a x) (+ b y)])
       (filter (fn [[x y]]
                 (and (<= 0 x 9)
                      (<= 0 y 9))))))

(defn grid [input]
  (->> (for [x (range 10)
             y (range 10)]
         {[x y] (get-in input [x y])})
       (into {})))

(defn increment [grid ks]
  (reduce (fn [g k]
            (update g k inc))
          grid
          ks))


(defn step [{:keys [grid flickers step]}]
  (let [will-flicker (loop [prev-grid grid
                            new-grid  (increment grid (keys grid))]
                (let [ready (->> (filter (fn [[k v]]
                                           (and (> v 9)
                                                (<= (prev-grid k) 9))) new-grid)
                                 (map first)
                                 (mapcat adjs))]
                  (if (seq ready)
                    (recur new-grid (increment new-grid ready))
                    new-grid)))]

    {:grid     (reduce-kv (fn [c k v]
                            (assoc c k (if (> v 9) 0 v)))
                          {}
                          will-flicker)
     :flickers (+ flickers (count (filter (fn [[k v]] (> v 9)) will-flicker)))
     :step     (inc step)}))

;; 1732
(defn part1 [n]
  (let [d (data)]
    (->> {:grid (grid d)
          :flickers 0
          :step 0}
         (iterate step)
         (drop n)
         (first))))

;; 290
(defn part2 []
  (let [d (data)]
    (->> {:grid (grid d)
          :flickers 0
          :step 0}
         (iterate step)
         (partition 2 1)
         (map (fn [[a b]]
                [(select-keys a [:step :flickers])
                 (select-keys b [:step :flickers])]))
         (map (fn [[a b]]
              {:change  (- (:flickers b)
                           (:flickers a))
               :step (:step b)}) )
         (filter #(= 100 (:change %)))
         (first))))
