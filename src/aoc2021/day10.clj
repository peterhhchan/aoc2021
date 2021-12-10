(ns aoc2021.day10
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-data []
  (->> (slurp "data/day10.txt")
       (str/split-lines)))

(def open  "[({<")
(def close "])}>")
(def close->open (zipmap close open))
(def open->close (zipmap open close))

(def scores (zipmap ")]}>" [3 57 1197 25137]))

(def test-input
"[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(defn test-data []
  (->> test-input
       (str/split-lines)))

(defn part1 []
  (let [d (parse-data)]
    (->> d
         (map (fn [l]
                (loop [line l]
                  (let [new (-> line
                                (str/replace #"\[\]" "")
                                (str/replace #"\{\}" "")
                                (str/replace #"\<\>" "")
                                (str/replace #"\(\)" ""))]
                    (if (= (count new) (count line))
                      line
                      (recur new))))))
         (map (fn [l]
                (->>  (filter (set close) l)
                      (map scores)
                      first)))
         (remove nil?)
         (reduce +))))

(def close-scores (zipmap ")]}>" [1 2 3 4]))

(defn part2 []
  (let [d (parse-data)]
    (->> d
         (map (fn [l]
                (loop [line l]
                  (let [new (-> line
                                (str/replace #"\[\]" "")
                                (str/replace #"\{\}" "")
                                (str/replace #"\<\>" "")
                                (str/replace #"\(\)" ""))]
                    (if (= (count new) (count line))
                      line
                      (recur new))))))
         (remove (fn [l]
                (->>  (filter (set close) l)
                      first)))
         (map reverse)
         (map (fn [l]
                (->> (map open->close l)
                     (map close-scores)
                     (reduce (fn [score s]
                               (+ s (* score 5)))
                             0))))
         sort
         (drop 22))))
