(ns aoc2021.day1)

(defn read-data []
  (->> (slurp "data/day1.txt")
       clojure.string/split-lines
       (map #(Integer/parseInt %))))

;;(def data (read-data))

(defn part1 [data]
  (->> (map - (rest data) data)
       (filter pos?)
       count))

(defn part2 [data]
  (->> (map - (rest (drop 2 data)) data)
       (filter pos?)
       count))
