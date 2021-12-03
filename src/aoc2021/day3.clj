(ns aoc2021.day3
  (:require [clojure.string :as s]))

(defn read-data []
  (->> (slurp "data/day3.txt")
       clojure.string/split-lines
       (map (fn [s]
              (->> (map identity s)
                   (map #(- (int %) (int \0))))))))

(defn to-decimal [ns]
  (read-string (str "2r" (apply str ns))))

(defn part1 []
  (let [data   (read-data)
        n      (count data)
        counts (apply map + data)
        v1     (map #(if (< % ( * n 0.5)) 1 0) counts)
        v2     (map #(if (< % ( * n 0.5)) 0 1) counts)]
    (* (to-decimal v1)) (to-decimal v2)))


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

(defn part2 []
  (let [data (read-data)]
    (* (part2-rating bit-to-keep-o2 data)
       (part2-rating bit-to-keep-co2 data))))
