(ns aoc2021.day20
  (:require [clojure.string :as str]
            [clojure.zip :as zip]))

(defn data [] (slurp "data/day20.txt"))
#_(defn data []
"..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###")
(defn convert [s]
  (-> s
      (str/replace  "." "0")
      (str/replace  "#" "1")
      (str/join)))

(defn parse-input []
  (let [[a b]  (str/split (data) #"\n\n")]
    [(vec  (convert (str/join (str/split-lines a))))
     (str/split-lines (convert b))]))


(defn neighbors [[x y]]
  (for [a [-1 0 1]
        b [-1 0 1]]
    [(+ a  x) (+ b y)]))

(defn expand [[x y]]
  (for [a (range -2 3)
        b (range -2 3)]
    [(+ a x) (+ b y)]))

(defn dark-points [pts]
  (->>  (filter #(= 1 (second %)) pts)
        (map first)
        set))

(defn flash [n]
  (let [[decoder b] (parse-input)
        grid  (mapv vec b)]
    (loop [pts (->> (for [x (range (count grid))
                          y (range (count grid))]
                      (let [p (get-in grid [x y])]
                        (if (= p \1)
                          {[x y] 1}
                          {[x y] 0})))
                    (into {}))
           n 50
           dark? true]
      (if (zero? n)
        (count (dark-points pts))
        (recur
         (->> pts
              (filter #(= 1 (second %)))
              (map (comp set expand first))
              (apply clojure.set/union)
              (pmap (fn [p]
                     (let [v (->> (neighbors p)
                                  (map #(get pts % (if dark? \0 \1)) )
                                  (apply str "2r")
                                  (read-string)
                                  (get decoder))]
                       (if (= v \1)
                         {p 1}
                         {p 0}))))
              (into {}))
         (dec n)
         (not dark?))))))

;; 4928
(defn part1 [] (flash 2))

;; 16605
(defn part2 [] (flash 50))
