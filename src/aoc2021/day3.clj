(ns aoc2021.day3
  (:require [clojure.string :as s]))

(defn data [] (slurp "data/day3.txt"))

(defn parse-input [input]
  (->> (clojure.string/split-lines input)
       (map (fn [s]
              (->> (seq s)
                   (map #(- (int %) (int \0))))))))

(defn to-decimal [ns]
  (Long/parseLong (apply str ns) 2))

(defn part1 [input]
  (let [data   (parse-input input)
        n      (count data)
        counts (apply map + data)
        v1     (map #(if (< % (- n %)) 1 0) counts)
        v2     (map #(if (< % (- n %)) 0 1) counts)]
    (* (to-decimal v1)
       (to-decimal v2))))


(defn bit-to-keep-o2 [ones zeroes]
 (cond
   (= ones zeroes) 1
   (> ones zeroes) 1
   :else           0))

(defn bit-to-keep-co2 [ones zeroes]
 (cond
   (= ones zeroes) 0
   (< ones zeroes) 1
   :else           0))

(defn part2-rating [f ratings]
  (loop [rs ratings
         n  0]
    (if (= 1 (count rs))
      (to-decimal (first rs))
      (let [counts   (->> rs
                          (map #(nth % n))
                          (reduce +))
            keep-bit (f counts (- (count rs) counts))]
        (recur (filter #(= (nth  % n) keep-bit ) rs)
               (inc n))))))

(defn part2 [input]
  (let[data (parse-input input)]
    (* (part2-rating bit-to-keep-o2 data)
       (part2-rating bit-to-keep-co2 data))))
