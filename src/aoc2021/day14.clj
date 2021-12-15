(ns aoc2021.day14
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn data [] (slurp "data/day14.txt"))
(defn test-data []
"NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")


(defn parse-input [input]
  (let [[h r] (str/split input #"\n\n")
        rules (->> (str/split-lines r)
                   (map #(re-seq #"\w+" %))
                   (map (fn [[[a b] c]]
                          {(str a b) [(str a c) (str c b)]}))
                   (into {}))]
    {:template h
     :rules    rules}))

(defn step [rules polymer]
  (reduce-kv (fn [p k v]
               (let [[a b] (rules k)]
                 (-> p
                     (update k (fnil - 0) v )
                     (update a (fnil + 0) v)
                     (update b (fnil + 0) v))))
             polymer
             polymer))

(defn count-freqs [template freqs]
  (->> freqs
       (map (fn [[[a b] v]]
              {b v}))
       (apply merge-with + {(first template) 1})))

(defn solve [input steps]
  (let [{:keys [template rules]} (parse-input input)]
    (->> (partition 2 1 template)
         (map str/join)
         (frequencies)
         (iterate (partial step rules))
         (drop steps)
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
