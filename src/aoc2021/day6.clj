(ns aoc2021.day6)

(defn parse-int [s]  (Integer/parseInt s))
(defn str-to-ints [s]
  (->> (re-seq #"\d+" s)
       (map parse-int)))

(defn parse-data []
  (->> (slurp "data/day6.txt")
       (str-to-ints)))

(defn step [state]
  (->> state
       (map (fn [[k v]]
              (cond (pos? k)  {(dec k) v}
                    (zero? k) {8 v
                               6 (get state 0 0)})))
       (apply merge-with + )))

(defn count-births [n]
  (->> (parse-data)
       (frequencies)
       (iterate step)
       (drop n)
       first
       (vals)
       (reduce +)))

;;387413
(defn part1 []
  (count-births 80))

;;1738377086345
(defn part2 []
  (count-births 256))
