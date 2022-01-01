(ns aoc2021.day1)

(defn read-data []
  (->> (slurp "data/day1.txt")
       clojure.string/split-lines
       (map #(Integer/parseInt %))))

(def data (read-data))

(defn part1 []
  (->> (map - (drop 1 data) data)
       (filter pos?)
       count))

;; No need to sum each window up as the elements of the windows
;; overlap and cancel each other out
(defn part2 []
  (->> (map - (drop 3 data) data)
       (filter pos?)
       count))
