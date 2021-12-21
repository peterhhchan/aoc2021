(ns aoc2021.day21
  (:require [clojure.math.combinatorics :as combo]))

(def input [6 9])

(defn die [n]
  (->> n
       (iterate (fn [n]
                  (if (= n 100)
                    1
                    (inc n))))
       rest
       (take 3)))

(defn next-pos [pos n]
  (let [s (mod (+ pos n) 10)]
    (if (zero? s) 10 s)))

(defn game [a b]
  (loop [scores   [0 0]
         pos      [a b]
         last-die 100
         steps    0
         turn     0]
    (if (or (<= 1000 (scores 0))
            (<= 1000 (scores 1)))
      [scores (* 3 steps (apply min scores))]
      (let [roll    (die last-die)
            cur     (get pos turn)
            next-pos (next-pos cur (reduce + roll))]
        #_(prn "Player " (if (#{1} turn) 1 2) "rolls " roll "and moves to space " new-pos
             " for a total score of " ((update-in scores [turn]  #(+ % new-pos)) turn))
        (recur (update-in scores [turn]  #(+ % next-pos))
               (assoc-in pos [turn] next-pos)
               (last roll)
               (inc steps)
               (if (#{1} turn) 0 1))))))

(def stop 21)
(def freqs (frequencies (map (partial apply +) (combo/selections [1 2 3] 3))))
(def mem-game
  (memoize (fn [scores pos turn]
             (cond (<= 21 (scores 0))
                   {:one 1}
                   (<= 21 (scores 1))
                   {:two 1}
                   :else
                   (let [cur       (get pos turn)
                         next-turn (if (#{1} turn) 0 1)]
                     (->> freqs
                          (map (fn [[k v]]
                                 (let [s (next-pos cur k)]
                                   (->> (mem-game (update-in scores [turn] #(+ % s))
                                                  (assoc-in pos [turn] s)
                                                  next-turn)
                                        (reduce-kv (fn [res a b] (assoc res a (* b v)))
                                                   {})))))
                          (apply merge-with +)))))))
;; 925605
(defn part1 []
  (game 6 9))

(defn part2 [] ;;486638407378784
  (->> (mem-game [0 0] [6 9] 0)
       vals
       (apply max)))
