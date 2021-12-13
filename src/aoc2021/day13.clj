(ns aoc2021.day13
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn data []
  (slurp "data/day13.txt"))

(defn test-data []
"6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(defn parse-long [s]  (Long/parseLong s))

(defn parse-coords [l]
  (let [[x y] (re-seq #"\d+" l)]
    [(parse-long x) (parse-long y)]))

(defn parse-folds [l]
  (let [ins      (last (str/split l #" "))
        [xy pos] (str/split ins #"=")]
    [xy (parse-long pos)]))

(defn parse-input [input]
  (let [[a b]  (str/split input #"\n\n")
        coords (->> (str/split-lines a)
                    (map parse-coords))
        folds  (->> (str/split-lines b)
                    (map parse-folds))]
    {:points coords
     :folds  folds}))

(defn update-points [pts [dir line]]
  (->> pts
       (map (fn [[x y]]
              (if (= dir "x")
                (if (< x line)
                  [x y]
                  [(- (+ line line) x) y])
                (if (< y line)
                  [x y]
                  [x (- (+ line line) y)]))))
       set))

(defn fold-paper [paper folds]
  (reduce (fn [points fold]
              (set (update-points points fold)))
            paper
            folds))

(defn my-print [pts]
  (let [max-y (inc (apply max (map second pts)))
        max-x (inc (apply max (map first pts)))]
    (for [y (range max-y)]
      (->> (for [x (range max-x)]
             (if (get pts [x y]) "8" " "))
           str/join ))))

;; 775
(defn part1 []
  (let [{:keys [points folds]} (parse-input (data))]
    (->> folds
         (take 1)
         (fold-paper points)
         (count))))

(defn part2 []
  (let [{:keys [points folds]} (parse-input (data))]
    (->> folds
         (fold-paper points)
         (my-print))))

(def res
  ["888  8888 8  8 888  8  8 888  8  8 888  "
   "8  8 8    8  8 8  8 8  8 8  8 8 8  8  8 "
   "8  8 888  8  8 8  8 8  8 8  8 88   8  8 "
   "888  8    8  8 888  8  8 888  8 8  888  "
   "8 8  8    8  8 8    8  8 8    8 8  8 8  "
   "8  8 8888  88  8     88  8    8  8 8  8 "])
