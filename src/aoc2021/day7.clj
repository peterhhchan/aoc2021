(ns aoc2021.day7)

(defn parse-int [s]  (Integer/parseInt s))
(defn str-to-ints [s]
  (->> (re-seq #"\d+" s)
       (map parse-int)))

(defn parse-data []
  (->> (slurp "data/day7.txt")
       (str-to-ints)))

(defn abs [x y]
  (Math/abs (- x y)))

;;
;; Approach #1
;; Brute force approach
;;

(defn cost-1 [start end]
  (abs start end))

(defn cost-2 [start end]
  (let [n (abs start end)]
    (/ (* n (inc n)) 2)))

(defn cost [data cost-fn n]
  (reduce + (map (partial cost-fn n) data)))

(defn cheapest [cost-fn]
  (let [d (parse-data)]
    (->> (range (apply max d))
         (pmap (partial cost d cost-fn))
         (apply min))))

(defn part1 []
  ;;326132
  (cheapest cost-1))

(defn part2 []
  ;;88612508
  (cheapest cost-2))

;;
;; Approach #2
;; Search using binary search
;;


(defn mid [a b]
  (int (+ a (/ (- b a) 2))))

(defn  min-cost [f]
  (let [d (parse-data)]
    (loop [mn   0
           mx   (apply max d)
           best (cost d f (mid mn mx))]
      (if (= mn mx)
        best
        (let [md     (mid mn mx)
              lower  (cost d f (mid mn md))
              higher (cost d f (mid md mx))]
          (if (< lower higher)
            (recur mn md (min lower best))
            (recur (inc md) mx (min higher best))))))))

;; Yields the same answers as above
(def part-1 (min-cost cost-1))
(def part-2 (min-cost cost-2))


;;
;;  Approach #3
;;

(defn part1-a []
  (let [data (sort (parse-data))
        mid  (/ (count data) 2)]
    (cost data cost-1 (nth data mid))))

(defn part2-a []
  (let [data (parse-data)
        mean (quot (reduce + data) (count data))]
    (min (cost data cost-2 mean )
         (cost data cost-2 (inc mean) ))))
