(ns aoc2021.day25
  (:require [clojure.string :as str]))

(defn data []
  (str/split-lines  (slurp "data/day25.txt")))

(defn wrap [size n]
  (if (= (inc n) size)
    0
    (inc n)))

;; 427 - ~10s
(defn part1 []
  (let [input (vec (mapv vec (data)))
        h     (count input)
        w     (count (first input))]
    (loop [grid              (->> (for [x (range w)
                                        y (range h)]
                                    {[y x] (get-in input [y x])})
                                  (into {}))
           move-right?       true
           moved-previously? true
           n                 1]
      (let [new-grid (persistent!
                      (reduce
                       (fn [res [[x y] square]]
                         (cond
                           (and move-right?
                                (= square \>)
                                (= \. (or (grid [x (inc y)]) (grid [x 0]))))
                           (-> res
                               (assoc! [x y] \.)
                               (assoc! [x (wrap w y)] \>))

                           (and (not move-right?)
                                (= square \v)
                                (= \. (or (grid [(inc x) y]) (grid [0 y]))))
                           (-> res
                               (assoc! [(wrap h x) y] \v)
                               (assoc! [x y] \. ))

                           :else res))
                       (transient {})
                       grid))]
        (if (and (not moved-previously?)
                 (empty? new-grid))
          n
          (recur (merge grid new-grid)
                 (not move-right?)
                 (seq? new-grid)
                 (if (not move-right?) (inc n) n)))))))
