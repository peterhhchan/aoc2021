(ns aoc2021.day2)

(defn read-data []
  (->> (slurp "data/day2.txt")
       clojure.string/split-lines
       (map (fn [s]
              (let [[d n] (clojure.string/split s #" ")]
                [d (Integer/parseInt n)])))))

;; -1990000
(defn part1 []
  (->> (read-data)
       (map (fn [[d n]]
              (case d
                "forward" {:x n}
                "down"    {:z (- n)}
                "up"      {:z n})))
       (apply merge-with +)
       (vals)
       (reduce *)))

;; -1975421260
(defn part2 []
  (->> (read-data)
       (map (fn [[d n]]
              (case d
                "forward"  [n 0]
                "down"     [0 (- n)]
                "up"       [0 n])))
       (reduce (fn [[h d a] [x aim]]
                 (if (zero? aim)
                   [(+ x h) (+ d (* x a)) a]
                   [h d (+ a aim)]))
               [0 0 0])
       (take 2)
       (reduce *)))
