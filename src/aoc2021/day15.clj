(ns aoc2021.day15
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.set :as set]
            [clojure.string :as str]))

;; --- Day 15: Chiton ---

(defn parse-long [s]  (Long/parseLong s))
(defn data [] (slurp "data/day15.txt"))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(re-seq #"\d" %))
       (mapv #(mapv parse-long %))))

(defn neighbors [[x y]]
  [[(inc x) y]
   [x (inc y)]
   [x (dec y)]
   [(dec x) y]])

(defn my-inc [d]
  (if (= d 9) 1 (inc d)))

(defn expand [n input]
  (->> input
       (map (fn [row]
              (->> (iterate #(map my-inc %) row)
                   (take n)
                   (apply concat))))
       (iterate (fn [rows]
                  (map #(map my-inc %) rows)))
       (take n)
       (apply concat)
       (mapv vec)))

;; Djikstra implementation
(defn min-risk [input]
  (let [n    (count input)
        grid (->> (for [x (range n)
                        y (range n)]
                    [x y])
                  (reduce #(assoc %1 %2 (get-in input %2))
                          {}))]
    (loop [visited  {}
           to-visit (priority-map [0 0] 0)]
      (if-let [total-risk (visited [(dec n) (dec n)])]
        total-risk
        (let [visited     (conj visited (first to-visit))

              ;; using a transducer improves performance from ~35s to ~4s
              tf          (comp (remove visited)
                                (filter grid)
                                (map (fn [pos]
                                       [pos (->> (neighbors pos)
                                                 (keep visited)
                                                 (apply min)
                                                 (+ (grid pos) ))])))]
          (recur visited
                 (into (pop to-visit)
                       tf
                       (neighbors (ffirst to-visit)))))))))

;;741
(defn part1 [input]
  (->> (parse-input input)
       min-risk))

;; 2976
(defn part2 [input]
  (->> (parse-input input)
       (expand 5)
       min-risk))
