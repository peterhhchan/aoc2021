(ns aoc2021.day5)

(defn parse-int [s]  (Integer/parseInt s))

(defn str-to-ints [s]
  (->> (re-seq #"\d+" s)
       (map parse-int)))

(defn input [] (slurp "data/day5.txt"))

(defn parse-input [input]
  (->> input
       clojure.string/split-lines
       (map str-to-ints)))

;; Solution 1 - handle diagonals differently, uses a map
(defn range-inclusive [s e]
  (if (< s e)
    (range s (inc e))
    (range s (dec e) -1)))

(defn overlaps [input count-diagonals?]
  (->> (parse-input input)
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


(defn part1 [] (count (overlaps (input) false)))
(defn part2 [] (count (overlaps (input) true)))

;;
;; Another solution - handle diagonals the same way, uses frequencies
;;
(defn step [a b]
  (cond
    (= a b) identity
    (> a b) dec
    (< a b) inc))

(defn points-between [[x1 y1 x2 y2]]
  (let [step-x (step x1 x2)
        step-y (step y1 y2)
        x*     (step-x x2)
        y*     (step-y y2)]
    (->> [x1 y1]
         (iterate (fn [[x y]]
                    [(step-x x) (step-y y)]))
         (take-while (fn [[x y]]
                       (not (and (= x x*) (= y y*))))))))

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


(defn part1-b [input]
  (->> (parse-input input)
       (remove diagonal?)
       (count-overlaps)))

(defn part2-b [input]
  (->> (parse-input input)
       (count-overlaps)))
