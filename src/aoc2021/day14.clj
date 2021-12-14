(ns aoc2021.day14
  (:require [clojure.string :as str]
            [clojure.set :as set]))


;; PHOSBSKBBBFSPPPCCCHN - 20
(defn data [] (slurp "data/day14.txt"))
(defn parse-long [s]  (Long/parseLong s))
(defn str-to-ints [s]
  (->> (re-seq #"\d" s)
       (map parse-long)))

(defn grow [s rules]
  (let [combos   (->> (partition  2 1 s)
                      (map (partial apply str))
                      (map (fn [a]
                             (str (first a) (rules a))))
                      (apply str))]
    (str combos (last s))))

(defn part1 []
  (let [d (->> (data))
        [h r]  (str/split d #"\n\n")
        rules     (->> (str/split-lines r)
                       (map #(re-seq #"\w+" %))
                       (map (fn [[a b]]
                              {a b}))
                       (into {}))
        res (loop [n 9
                   s h]
              (if (pos? n)
                (recur (dec n) (grow s rules))
                (grow s rules)))]
    (sort-by second (frequencies res))))


(def freq-ss
  (memoize
   (fn [rules n ss]
     (if (zero? n)
       (frequencies ss)
       (let [sss (rules ss)
             s1  (subs sss 0 2)
             s2  (subs sss 1)]
         (->> [s1 s2]
              (map (partial freq-ss rules (dec n)))
              (apply merge-with + {(last s1) -1})))))))


(defn freqs [ rules n ss]
  (merge-with -
              (->> (partition 2 1 ss)
                   (map (comp  (partial freq-ss rules n)
                               (partial str/join)))
                   (reduce (partial merge-with +)))
              (frequencies ss)
              {(first ss) -1}
              {(last ss) -1}))

(defn part2 []
  (let [d     (->> (data))
        [h r] (str/split d #"\n\n")

        rules (->> (str/split-lines r)
                   (map #(re-seq #"\w+" %))
                   (map (fn [[a b]]
                          {a (str (first a) b (second a))}))
                   (into {}))]
    (->> (freqs rules 40 h)
         (sort-by second))))
