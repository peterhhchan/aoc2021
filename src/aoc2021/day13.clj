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

(defn parse-coords [line]
  (->> (re-seq #"\d+" line)
       (mapv parse-long)))

(defn parse-folds [line]
  (let [[xy pos] (-> line
                     (str/split  #" ")
                     last
                     (str/split  #"="))]
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
                  [x (- (+ line line) y)]))))))

(defn my-print [pts]
  (let [max-y (apply max (map second pts))
        max-x (apply max (map first pts))]
    (for [y (range (inc max-y))]
      (str/join (for [x (range (inc max-x))]
                  (if (get pts [x y]) "8" " "))))))


;; 775
(defn part1 [input]
  (let [{:keys [points folds]} (parse-input input)]
    (->> folds
         (take 1)
         (reduce update-points points)
         set
         (count))))

(defn part2 [input]
  (let [{:keys [points folds]} (parse-input input)]
    (->> folds
         (reduce update-points points)
         set
         (my-print))))

(def res
  ["888  8888 8  8 888  8  8 888  8  8 888  "
   "8  8 8    8  8 8  8 8  8 8  8 8 8  8  8 "
   "8  8 888  8  8 8  8 8  8 8  8 88   8  8 "
   "888  8    8  8 888  8  8 888  8 8  888  "
   "8 8  8    8  8 8    8  8 8    8 8  8 8  "
   "8  8 8888  88  8     88  8    8  8 8  8 "])
