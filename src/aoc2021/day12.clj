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
  (let [[a b] (re-seq #"\w+" s)]
    [a b]))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map parse-line)
       (reduce (fn [m [a b]]
                 (-> m
                     (update a conj b)
                     (update b conj a)))
               {})
       (reduce-kv (fn [m k v]
                    (if (#{"end"} k)
                      m
                      (assoc m k (remove #{"start"} v))))
                  {})))

(defn all-paths [caves visit-again? p]
  (let [visited (frequencies p)]
    (->> (get caves (first p))
         (mapcat (fn [n]
                   (cond
                     (= n "end")
                     (list (conj p n))
                     (or (= n (str/upper-case n))
                         (zero? (get visited n 0)))
                     (all-paths caves visit-again? (conj p n))
                     visit-again?
                     (all-paths caves false (conj p n))))))))

(defn count-paths [caves visit-again? p]
  (let [visited (frequencies p)]
    (->> (get caves (first p))
         (keep (fn [n]
                   (cond
                     (= n "end")
                     1
                     (or (= n (str/upper-case n))
                         (not (visited n)))
                     (count-paths caves visit-again? (conj p n))
                     visit-again?
                     (count-paths caves false (conj p n)))))
         (reduce +))))

;; 5958
(defn part1 [input]
  (count-paths (parse-input input) false (list "start")))

;; 150426
(defn part2 [input]
  (count-paths (parse-input input) true (list "start")))
