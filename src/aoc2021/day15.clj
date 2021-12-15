(ns aoc2021.day15
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-long [s]  (Long/parseLong s))
(defn data [] (slurp "data/day15.txt"))

(defn parse-input []
  (->> (data)
       ;; test-data
       (str/split-lines)
       (map #(re-seq #"\d" %))
       (mapv #(mapv parse-long %))))

(defn neighbors [n [x y]]
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

(defn candidates [board size costs cur]
  (->> (neighbors size cur)
       (filter board)
       (filter (fn [next]
                 (> (get costs next 999999)
                    (+ (board next) (costs cur)))))))


(defn min-cost [coords]
  (let [size  (count coords)
        board (reduce (fn [m pos]
                        (assoc m pos (get-in coords pos)))
                      {}
                      (for [x (range size) y (range size)] [x y]))]
    (loop [costs    {[0 0] 0}
           to-check (->> (neighbors size [0 0])
                         (filter board))]
      (if (seq to-check)
        (let [updated-costs (reduce (fn [m pos]
                                  (->> (neighbors size pos)
                                       (keep m)
                                       (apply min)
                                       (+ (board pos))
                                       (assoc m pos)))
                                costs
                                to-check)

              to-check-next (->> to-check
                                 (mapcat #(candidates board size updated-costs %) )
                                 set)]
          (recur updated-costs to-check-next))
        (get costs [(dec size) (dec size)])))))

;;741
(defn part1 []
  (min-cost (parse-input)))

;; 2976
(defn part2 []
  (min-cost (expand 5 (parse-input))))


(def test-data
"1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")
