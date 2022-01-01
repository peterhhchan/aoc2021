(ns aoc2021.day13
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; https://adventofcode.com/2021/day/13

(defn data [] (slurp "data/day13.txt"))

(defn parse-long [s]  (Long/parseLong s))

(defn parse-coords [line]
  (->> (re-seq #"\d+" line)
       (mapv parse-long)))

(defn parse-folds [line]
  (let [[xy pos] (-> line
                     (str/split  #" ")
                     last
                     (str/split  #"="))]
    [(if (= xy "x") 0 1)
     (parse-long pos)]))

(defn parse-input [input]
  (let [[a b]  (str/split input #"\n\n")
        coords (->> (str/split-lines a)
                    (map parse-coords))
        folds  (->> (str/split-lines b)
                    (map parse-folds))]
    {:points coords
     :folds  folds}))


(defn fold [^long v ^long line]
  (- line (Math/abs (- line v))))

(defn fold-points [pts [axis line]]
  (set (map #(update % axis fold line) pts)))

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
         (reduce fold-points points)
         (count))))

(defn part2 [input]
  (let [{:keys [points folds]} (parse-input input)]
    (->> folds
         (reduce fold-points points)
         (my-print))))

(def res
  ["888  8888 8  8 888  8  8 888  8  8 888  "
   "8  8 8    8  8 8  8 8  8 8  8 8 8  8  8 "
   "8  8 888  8  8 8  8 8  8 8  8 88   8  8 "
   "888  8    8  8 888  8  8 888  8 8  888  "
   "8 8  8    8  8 8    8  8 8    8 8  8 8  "
   "8  8 8888  88  8     88  8    8  8 8  8 "])
