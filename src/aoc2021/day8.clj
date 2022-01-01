(ns aoc2021.day8
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

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
;;
;; Day 8 Brute force apprach
;;
(defn all-permutations []
  (let [s "abcdefgh"]
    (->> (combo/permutations s)
         (map #(zipmap % s)))))

(defn solve-line-a [digits]
  (let [default  (->> {0 "abcefg"
                       1 "cf"
                       2 "acdeg"
                       3 "acdfg"
                       4 "bcdf"
                       5 "abdfg"
                       6 "abdefg"
                       7 "acf"
                       8 "abcdefg"
                       9 "abcdfg"}
                     (reduce-kv #(assoc %1 (set %3) %2)
                                {}))
        solution (->> (all-permutations)
                      (filter (fn [p]
                                (->> (take 10 digits)
                                     (map #(set (map p %1)))
                                     (every? default))))
                      first)]
    (->> (drop 10 digits)
         (map (fn [digit]
                (-> (map solution digit)
                     set
                     default)))
         (apply str)
         (parse-int))))

;; 1028926
(defn part2a []
  (->> (parse-data)
       (pmap solve-line-a)
       (reduce +)))

;;
;; Approach 2
;;

(defn result [segments col]
  (first (filter #(= segments (count %)) col)))

(defn solve-line [line]
  (let [line    (map set line)
        display (take 10 line)
        output  (drop 10 line)

        p1   (result 2 display)
        p4   (result 4 display)
        p7   (result 3 display)
        p8   (result 7 display)
        p9   (->> (filter #(set/subset? p4 %) display)
                  (remove #{p4 p8})
                  first)

        ;; 0 or 3
        p03  (->> (filter #(set/subset? p1 %) display)
                  (remove #{p1 p4 p7 p8 p9}))
        p0   (result 6 p03)
        p3   (result 5 p03)

        ;; 2,5,6
        p256 (remove (into #{p1 p4 p7 p8 p9} p03) display)
        p6   (result 6 p256)
        p25  (remove #{p6} p256)
        p5   (first (filter #(set/subset? % p6) p25))
        p2   (first (remove #{p5} p25))


        solution   (zipmap [p0 p1 p2 p3 p4 p5 p6 p7 p8 p9] (range))]
    (->>  (map solution output)
          (apply str)
          (parse-int))))

(defn part2b []
  (->> (parse-data)
       (pmap solve-line)
       (reduce +)))
