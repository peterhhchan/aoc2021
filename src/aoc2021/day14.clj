(ns aoc2021.day14
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 14: Extended Polymerization ---

(defn data [] (slurp "data/day14.txt"))

(defn parse-input [input]
  (let [[h r] (str/split input #"\n\n")
        rules (->> (str/split-lines r)
                   (map #(re-seq #"\w+" %))
                   (map (fn [[[a b] c]]
                          {(str a b) [(str a c) (str c b)]}))
                   (into {}))]
    {:template h
     :rules    rules}))

;; Because we don't need to remember the exact sequence, we can
;; represent the polymer as a frequency o fpairs instead. Each
;; insertion adds two new pairs and removes an existing one.
(defn step [rules polymer]
  (reduce-kv (fn [p k v]
               (let [[a b] (rules k)]
                 (-> p
                     (update k (fnil - 0) v )
                     (update a (fnil + 0) v)
                     (update b (fnil + 0) v))))
             polymer
             polymer))

;; We need to be careful not to dbl count the pairs
(defn count-freqs [template freqs]
  (->> freqs
       (map (fn [[[a b] v]]
              {b v}))
       (apply merge-with + {(first template) 1})))

(defn solve [input n]
  (let [{:keys [template rules]} (parse-input input)]
    (->> (partition 2 1 template)
         (map str/join)
         (frequencies)
         (iterate (partial step rules))
         (drop n)
         first
         (count-freqs template)
         (map second)
         sort
         ((juxt last first))
         (reduce -))))

;; 2874
(defn part1 [input]
  (solve input 10))

;; 5208377027195
(defn part2 [input]
  (solve input 40))
