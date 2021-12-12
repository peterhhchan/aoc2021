(ns aoc2021.day11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def test-0 "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def test-1 "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(defn data []
  (slurp "data/day12.txt"))

(defn parse-line [s]
  (let [[s e] (re-seq #"\w+" s)]
    {s [e]
     e [s]}))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map parse-line)
       (apply merge-with concat)))

(defn big-cave? [c]
  (= c (str/upper-case c)))

(defn small-caves-once [freqs]
  (->> freqs
       (filter #(not (big-cave? (first %))))
       (every? #(= 1 (second %)))))

(defn my-path [caves hacky-filter p]
  (let [visited (frequencies p)]
    (->> (get caves (first p))
         (mapcat (fn [n]
                   (cond
                     (= n "end")
                     (list (conj p n))
                     (= n "start")
                     nil
                     (or (big-cave? n)
                         (zero? (get visited n 0))
                         (hacky-filter visited))
                     (my-path caves hacky-filter (conj p n)))))
         (seq))))

(defn part1 [input]
  (my-path (parse-input input) (constantly false) (list "start")))

(defn part2 [input]
  (my-path (parse-input input) small-caves-once (list "start")))
