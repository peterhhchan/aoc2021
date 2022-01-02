(ns aoc2021.day17
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 17: Trick Shot ---

(defn data [] (slurp "data/day17.txt"))

(defn parse-input []
  (->> (data)
       (re-seq #"-*\d+")
       (map read-string)))

(defn step [{:keys [vx vy pos-x pos-y max-height] :as prev}]
  (merge prev
         {:pos-x      (+ pos-x vx)
          :pos-y      (+ pos-y vy)
          :vx         (max 0 (dec vx))
          :vy         (dec vy)
          :max-height (max (+ vy pos-y) max-height)}))

(defn explore [[t-min-x t-max-x t-max-y t-min-y] x y]
  (->> (iterate step  {:init [x y]
                       :max-height -1000
                       :vx x
                       :vy y
                       :pos-x 0
                       :pos-y 0})
       (drop-while #(> (:pos-y %) t-min-y))
       (take-while #(>= (:pos-y %) t-max-y))
       (filter #(<= t-min-x (:pos-x %) t-max-x))
       first))


;; 5565 , 176 results
(defn part1 []
;;  
  (let [[_ _ max-y min-y] (parse-input)]
    (->> (range max-y (inc (Math/abs max-y)))
         (keep #(explore [0 0 max-y min-y] 0 %))
         (sort-by :max-height)
         count)))

;; 2118
(defn part2 []
  (let [[min-x max-x max-y min-y] (parse-input)]
     (->> (range max-y (Math/abs max-y))
          (keep #(explore [0 0 max-y min-y] 0 %))
          (map (comp second :init))
          (pmap (fn [y]
                  (->>  (range (inc max-x))
                        (keep #(explore [min-x max-x max-y min-y] % y))
                        count)))
          (reduce +))))
