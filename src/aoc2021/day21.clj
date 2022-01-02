(ns aoc2021.day21
  (:require [clojure.math.combinatorics :as combo]))

;; --- Day 21: Dirac Dice ---

(def input [6 9])

;; We don't actually use this die, shown for correctness
(defn proper-die [n]
  (->> (iterate #(get {100 1} % (inc %)) n)
       rest
       (take 3)))

(defn die-rolls [n]
  (->> (iterate inc n)
       rest
       (take 3)))

(defn next-pos [pos n]
  (let [s (mod (+ pos n) 10)]
    (get {0 10} s s)))

(defn game [a b]
  (loop [scores   [0 0]
         pos      [a b]
         last-die 100
         steps    0]
    (if (>= (scores 1) 1000)
      [scores (* 3 steps (apply min scores))]
      (let [roll     (die-rolls last-die)
            next-pos (next-pos (pos 0) (reduce + roll))]
        (recur (vec (rseq (update scores 0  #(+ % next-pos))))
               (vec (rseq (assoc pos 0 next-pos)))
               (last roll)
               (inc steps))))))

;; 925605
(defn part1 []
  (game 6 9))

;;;
;;; Part 2
;;;

(def winning-score 21)

;; All possible die roll combinations each turn
(def outcomes (->> (combo/selections [1 2 3] 3)
                   (map (partial reduce +))
                   frequencies))
(def mem-game
  (memoize (fn [scores pos turn]
             (cond (>= (scores 0) winning-score) {:player-one 1}
                   (>= (scores 1) winning-score) {:player-two 1}
                   :else
                   (->> outcomes
                        (map (fn [[k v]]
                               (let [next (-> (get pos turn)
                                              (next-pos k))]
                                 (->> (mem-game (update scores turn #(+ % next))
                                                (assoc pos turn next)
                                                ({1 0 0 1} turn) )
                                      (reduce-kv #(assoc %1 %2 (* %3 v)) {})))))
                        (apply merge-with +))))))

;;486638407378784
(defn part2 []
  (->> (mem-game [0 0] [6 9] 0)
       vals
       (apply max)))
