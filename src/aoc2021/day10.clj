(ns aoc2021.day10
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-data []
  (->> (slurp "data/day10.txt")
       (str/split-lines)))

(def open  "[({<")
(def close "])}>")
(def open->close (zipmap open close))
(def scores-part1 (zipmap ")]}>" [3 57 1197 25137]))
(def scores-part2 (zipmap ")]}>" [1 2 3 4]))

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

(defn parse [l]
 (loop [line l]
   (let [new (-> line
                 (str/replace #"\[\]" "")
                 (str/replace #"\{\}" "")
                 (str/replace #"\<\>" "")
                 (str/replace #"\(\)" ""))]
     (if (= (count new) (count line))
       line
       (recur new)))))

(defn corrupted? [l]  (some (set close) l))

;;415953
(defn part1 []
  (->> (parse-data)
       (map parse)
       (filter corrupted?)
       (map (fn [l]
              (->>  (keep (set close) l)
                    (map scores-part1)
                    first)))
       (reduce +)))


;;2292863731
(defn part2 []
  (->> (parse-data)
       (map parse)
       (remove corrupted?)
       (map (fn [l]
              (->> (reverse l)
                   (map open->close)
                   (map scores-part2)
                   (reduce (fn [score s]
                             (+ s (* score 5)))
                           0))))
       sort
       (drop 22)
       first))
