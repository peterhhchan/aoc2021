(ns aoc2021.day20
  (:require [clojure.string :as str]
            [clojure.zip :as zip]))

;; --- Day 20: Trench Map ---

(defn data [] (slurp "data/day20.txt"))

(defn convert [s]
  (-> s
      (str/replace  "." "0")
      (str/replace  "#" "1")
      (str/join)))

(def char->int {\1 1 \0 0})

(defn parse-input []
  (let [[alg image]  (str/split (data) #"\n\n")]
    [(->> (str/split-lines alg)
          (str/join)
          (convert)
          (map char->int)
          vec)
     (str/split-lines (convert image))]))

(defn one-away [[x y]]
  (for [a [-1 0 1]
        b [-1 0 1]]
    [(+ a  x) (+ b y)]))

(defn two-away [[x y]]
  (for [a [-2 -1 0 1 2]
        b [-2 -1 0 1 2]]
    [(+ a x) (+ b y)]))

(defn dark-points [pts]
  (filter #(= 1 (second %)) pts))

(defn flash [steps]
  (let [[decoder image] (parse-input)
        grid            (mapv vec image)]
    ;; Maintain a set of points we have explored, points not within
    ;; this set alternate between 0 and 1.
    (loop [explored (->> (for [x (range (count grid))
                               y (range (count grid))]
                           {[x y] (-> (get-in grid [x y])
                                      (char->int))})
                         (into {}))           n steps]
      (if (zero? n)
        (count (dark-points explored))
        (let [light-or-dark (if (even? n) \0 \1)]
          (recur (->> (dark-points explored)
                      (map (comp set two-away first))
                      (apply clojure.set/union)
                      (into {} (map (fn [p]
                                      {p (->> (one-away p)
                                              (map #(get explored % light-or-dark))
                                              (apply str "2r")
                                              (read-string)
                                              (decoder))}))))
                 (dec n)))))))

;; 4928
(defn part1 [] (flash 2))

;; 16605
(defn part2 [] (flash 50))
