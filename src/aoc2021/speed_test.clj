(ns aoc2021.speed-test
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [criterium.core :as crit]))

(def inputs (->> (range 1 10)
                 (mapv #(slurp (str "data/day" % ".txt")))))

(defn parse-long [s] (Long/parseLong s))

;;
;; Day 1
;;

;; 1.42 ms
(defn day1-part1 [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       ((juxt  (partial drop 1) identity))
       (apply map -)
       (filter pos?)
       count))

(defn day1-part2 [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       ((juxt  (partial drop 3) identity))
       (apply map -)
       (filter pos?)
       count))

;;
;; Day 2
;;

;;0.817 ms
(defn day2-part1 [input]
  (->> (str/split-lines input)
       (reduce (fn [[x y] s]
                 (let [n (- (long (last s)) (long \0))]
                   (cond
                     (str/starts-with? s "f")
                     [(+ x n) y]
                     (str/starts-with? s "u")
                     [x (- y n)]
                     (str/starts-with? s "d")
                     [x (+ y n)])))
               [0 0])
       (reduce *)))

;; 0.865 ms
(defn day2-part2 [input]
  (->> (str/split-lines input)
       (reduce (fn [[x y z] s]
                 (let [n (- (long (last s)) (long \0))]
                   (cond
                     (str/starts-with? s "f")
                     [(+ x n) (+ y (* z n)) z]
                     (str/starts-with? s "u")
                     [x y (- z n)]
                     (str/starts-with? s "d")
                     [x y (+ z n)])))
               [0 0 0])
       (take 2)
       (reduce *)))



(defn run-all []

  (day1-part1 (inputs 0))
  (day1-part2 (inputs 0))

  (day2-part1 (inputs 1))
  (day2-part2 (inputs 1))

  (aoc2021.day3/part1 (inputs 2))
  (aoc2021.day3/part2 (inputs 2))

  (aoc2021.day4/part1 (inputs 3))
  (aoc2021.day4/part1 (inputs 3))
)
