(ns aoc2021.day9
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-int [s]  (Integer/parseInt s))
(defn str-to-ints [s]
  (->> (re-seq #"\d" s)
       (mapv parse-int)))

(defn parse-data []
  (->> (slurp "data/day9.txt")
       (str/split-lines)
       (mapv str-to-ints)))


(defn grid [n]
  (for [x (range n)
        y (range n)]
    [x y]))

(defn basin [data]
  (->> (grid (count data))
       (map (fn [pt]
              {pt (get-in data pt)}))
       (into {})))


(defn coords-adj [[x y]]
 [[(dec x) y]
  [x      (dec y)]
  [(inc x) y]
  [x      (inc y)]])


(defn low-points [basin]
  (->> (keys basin)
       (map (fn [pos]
              (let [height (basin pos 9)]
                (if (->> (coords-adj pos)
                         (map #(get basin % 9))
                         (map #(< height %) )
                         (every? true?))
              pos))))
       (remove nil?)))

;;458
(defn part1 []
  (let [b (basin (parse-data))]
    (->> (low-points b)
         (map (comp inc b))
         (reduce +))))

(defn higher-neighbours [b pts]
  (mapcat (fn [pt]
            (->> (coords-adj pt)
                 (filter #(and (> 9 (get b % 9))
                               (> (b %) (b pt))))))
          pts))

;;1391940
(defn part2 []
  (let [b (basin (parse-data))]
    (->> (low-points b)
         (map (fn [pt]
                (->> (list pt)
                     (iterate (partial higher-neighbours b))
                     (take-while seq)
                     (apply concat)
                     (into #{}))))
         (map count)
         sort
         reverse
         (take 3)
         (reduce *))))
