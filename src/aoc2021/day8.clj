(ns aoc2021.day8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-int [s]  (Integer/parseInt s))
(defn str-to-ints [s]
  (->> (re-seq #"\d+" s)
       (map parse-int)))

(defn parse-data []
  (->> (slurp "data/day8.txt")
       (str/split-lines)
       (map #(re-seq #"\w+" %))))

;; 1 4 7 8
(defn part1 []
  (let [d (parse-data)]
  (->> d
       (map (partial drop 10))
       (map #(map frequencies %))
       (map #(map count %))
       (flatten)
       (filter #(#{2 4 3 7} %)))))

;; Day 8 Brute force apprach

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn all-permutations []
  (let [s "abcdefgh"]
    (->> (permutations s)
         (map #(zipmap % s)))))

(defn solve-line [line]
  (let [alls (all-permutations)
        sols (->> {0 "abcefg"
                   1 "cf"
                   2 "acdeg"
                   3 "acdfg"
                   4 "bcdf"
                   5 "abdfg"
                   6 "abdefg"
                   7 "acf"
                   8 "abcdefg"
                   9 "abcdfg"}
                  (reduce-kv (fn [m k v]
                               (assoc m (set v) k))
                             {}))]
    (->> alls
         (map (fn [s]
                (->> line
                     (map (fn [w]
                        (->> w
                             (map s)
                             set)))
                     (filter sols))))
         (filter #(= 14 (count %)))
         first
         (drop 10)
         (map sols)
         (apply str)
         (parse-int))))

;; 1028926
(defn part2 []
  (->> (parse-data)
       (pmap solve-line)
       (reduce +)))
