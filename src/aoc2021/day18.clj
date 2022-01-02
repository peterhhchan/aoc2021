(ns aoc2021.day18
  (:require [clojure.string :as str]
            [clojure.zip :as zip]))

;; --- Day 18: Snailfish ---

(defn data [] (slurp "data/day18.txt"))
(defn parse-input []
  (->> (data)
       (str/split-lines)
       (map read-string)))

(defn pair-node? [loc]
  (let [node (zip/node loc)]
    (and (sequential? node)
         (number? (first node))
         (number? (second node)))))

(defn number-node? [loc]
  (number? (zip/node loc)))

(defn height [loc]
  (->> (iterate zip/up loc)
       (take-while seq)
       count
       dec))

(defn update-explode [l]
  (loop [loc l]
    (cond
      (and (= 4 (height loc))
           (pair-node? loc))
      (zip/next (zip/replace loc 0))

      :else
      (recur (zip/next loc)))))

(defn update-left [l v]
  (loop [loc l]
    (cond
      (number-node? loc)
      (zip/next (zip/edit loc + v))

      (nil? (zip/prev loc))
      loc

      :else
      (recur (zip/prev loc)))))

(defn update-right [l v]
  (loop [loc l]
    (cond
      (number-node? loc)
      (zip/root (zip/edit loc + v))

      (zip/end? (zip/next loc))
      (zip/root loc)

      :else
      (recur (zip/next loc)))))

(defn explode [t]
  (let [n (->> (zip/vector-zip t)
               (iterate zip/next )
               (take-while (complement zip/end?))
               (filter #(= 4 (height %)))
               (filter pair-node?)
               first)]
   (if (nil? n)
     t
     (let [[a b] (zip/node n)]
       (-> (update-left (zip/prev n) a)
           (update-explode)
           (update-right b))))))

(defn split [t]
  (let [loc (->> (zip/vector-zip t)
                 (iterate zip/next)
                 (take-while (complement zip/end?))
                 (filter (fn [loc]
                           (let [node (zip/node loc)]
                             (and (number? node)
                                  (> node 9)))))
                 first)]
    (if loc
      (let [v (zip/node loc)]
        (zip/root (zip/replace loc [(int (Math/floor (/ v 2)))
                                    (int (Math/ceil  (/ v 2)))])))
      t)))

(defn add [a b]
  (loop [fish (vector a b)]
    (let [exploded (explode fish)]
      (if (= fish exploded)
        (let [split (split exploded)]
          (if (= split exploded)
            fish
            (recur split)))
        (recur exploded)))))

(defn eval-tree [t]
  (let [vz (zip/vector-zip t)]
    (loop [l (zip/next vz)]
      (cond
        (pair-node? l)
        (let [[a b] (zip/node l)
              n  (zip/replace l (+ (* 3 a) (* 2 b)) )]
          (if (nil? (zip/up n))
            (zip/node n)
            (recur (zip/up n))))

        (zip/next l)
        (recur (zip/next l))

        (zip/up l)
        (recur (zip/up l))

        :else
        (zip/node l)))))

;; 3892
(defn part1 []
  (loop [t (reduce (fn [col item]
                      (add col item))
                    (first (parse-input))
                    (rest  (parse-input)))]
    (eval-tree t)))

;; ~40s - 4909
(defn part2 []
  (let [d (vec (parse-input))]
    (->>  (for [x (range (count d))
                y (range (count d))
                :when (not= x y)]
            (eval-tree (add (d x) (d y))))
          sort
          last)))
