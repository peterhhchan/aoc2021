(ns aoc2021.day5)

(defn parse-int [s]  (Integer/parseInt s))

(defn str-to-ints [s]
  (->> (re-seq #"\d+" s)
       (map parse-int)))

(defn parse-data []
  (->> (slurp "data/day5.txt")
       clojure.string/split-lines
       (map str-to-ints)))

;; Solution 1 - handle diagonals differently, uses a map
(defn range-inclusive [s e]
  (if (< s e)
    (range s (inc e))
    (range s (dec e) -1)))

(defn overlaps [count-diagonals?]
  (->> (parse-data)
       (map (fn [[x1 y1 x2 y2]]
              (let [xs (range-inclusive x1 x2 )
                    ys (range-inclusive y1 y2)]
                (if (or (= x1 x2)
                        (= y1 y2))
                  (->> (for [x xs y ys]
                         {[x y] 1})
                       (into {}))
                  (if count-diagonals?
                    (->> (map (fn [x y] {[x y] 1}) xs ys)
                         (into {}))
                    {})))))
       (apply merge-with + {})
       (filter (fn [[_ v]] (> v 1)))))


(defn part1 [] (count (overlaps false)))
(defn part2 [] (count (overlaps true)))

;;
;; Another solution - handle diagonals the same way, uses frequencies
;;
(defn step [a b]
  (cond
    (= a b) identity
    (> a b) dec
    (< a b) inc))

(defn points-between [[ax ay bx by]]
  (let [fx (step ax bx)
        fy (step ay by)]
    (->> [ax ay]
         (iterate (fn [[x y]]
                    [(fx x) (fy y)]))
         (take-while (fn [[x y]]
                       (not (and (= x (fx bx))
                                 (= y (fy by)))))))))

(defn diagonal? [[ax ay bx by]]
  (not (or (= ax bx)
           (= ay by))))

(defn count-overlaps [pts]
  (->> pts
       (mapcat points-between)
       (frequencies)
       vals
       (filter #(> % 1))
       count))

(defn part1-b []
  (->> (parse-data)
       (remove diagonal?)
       (count-overlaps)))

(defn part2-b []
  (->> (parse-data)
       (count-overlaps)))
