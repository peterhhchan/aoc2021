(ns aoc2021.day19
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.core.matrix :as m]
   [clojure.math.combinatorics :as combo]))

(defn data []
  (slurp "data/day19.txt"))

(defn parse-long [n]
  (Double/parseDouble n))

(defn parse-scanner [scanner]
  (->> (str/split-lines scanner)
       rest
       (map #(str/split % #","))
       (map #(map parse-long %))))

(defn parse-input []
  (->> (str/split (data) #"\n\n")
       (map parse-scanner)))

(defn rx [p]
  (let [r [[1 0 0]
           [0 0 -1]
           [0 1 0]]]
    (m/mmul p r )))

(defn ry [p]
  (let [r [[0 0 -1]
           [0 1 0]
           [1  0 0]]]
    (m/mmul p r )))

(def rots [[identity] [rx] [ry] [rx rx] [rx ry] [ry rx]
           [ry ry] [rx rx rx] [rx rx ry] [rx ry rx] [rx ry ry] [ry rx rx]
           [ry ry rx] [ry ry ry] [rx rx rx ry] [rx rx ry rx] [rx rx ry ry] [rx ry rx rx]
           [rx ry ry ry] [ry rx rx rx] [ry ry ry rx] [rx rx rx ry rx] [rx ry rx rx rx] [rx ry ry ry rx]])

(defn rotate [rots p]  (reduce #(%2 %1) p rots ))
(defn rotations [p]
  (->>  rots
        (map (fn [rot]
               [rot (rotate rot p)]))))


(defn distance [[x y z] [a b c]]
  (+ (* (- x a) (- x a))
     (* (- y b) (- y b))
     (* (- z c) (- z c))))

(defn distance-pairs [scanner]
  (->> (combo/combinations scanner 2)
       (map (fn [[x y]]
              {(distance x y) (list x y)}))
       (into {})))

(defn offset [x y]
  (map - x y))

(defn add [x y]
  (map + x y))

(defn find-rotation [[a b] [c d]]
  (let [offset-1 (offset b a)
        offset-2 (offset d c)]
    (if-let [[rot _] (->> (rotations offset-2)
                          (filter #(= offset-1 (second %)))
                          first)]
      [rot (offset a (rotate rot c))]
      (let [offset-2 (offset c d)
            [rot _] (->> (rotations offset-2)
                         (filter #(= offset-1 (second %)))
                         first)]
        [rot (offset a (rotate rot d))]))))


(defn rotate-scanners [dp-1 dp-2]
  (let [pair       (first (clojure.set/intersection
                           (set (keys dp-1))
                           (set (keys dp-2))))]
    (find-rotation (dp-1 pair) (dp-2 pair))))

(defn manhattan-distances [scanners]
  (->> (combo/combinations (vals scanners) 2)
       (map (fn [[[x y z] [a b c]]]
              (+  (Math/abs (- x a))
                  (Math/abs (- y b))
                  (Math/abs (- z c)))))
       (apply max)))

;; 483, 14804
(defn part1 []
  (let [input        (vec (parse-input))
        beacon-dists (map-indexed (fn [i scanner]
                                    [i (->> (combo/combinations scanner 2)
                                            (map (fn [[x y]]
                                                   (distance x y)))
                                            set)])
                                  input)
        pairs        (->> (combo/combinations beacon-dists 2)
                          (keep (fn [[a b]]
                                  (when (>= (count (clojure.set/intersection (second a) (second b)))
                                            66)
                                    [(first a) (first b)]))))
        [a b]        (first pairs)]
    (loop [beacons  {a (set (input a))}
           scanners {a [0 0 0]}
           scanner-pairs    pairs]
      (if (empty? scanner-pairs)
        {:counts    (count (apply clojure.set/union (vals beacons)))
         :distances (manhattan-distances scanners)}
        (let [[s1 s2]    (->> scanner-pairs
                              (keep (fn [[a b]]
                                      (cond (beacons a) [a b]
                                            (beacons b) [b a])))
                              first)
              [r offset]  (rotate-scanners (distance-pairs (beacons s1))
                                           (distance-pairs (input s2)))
              beacons*   (->> (input s2)
                              (map (partial rotate r))
                              (map (partial add offset))
                              set
                              (assoc beacons s2) )]
          (recur beacons*
                 (merge scanners {s2 offset})
                 (remove (fn [[s1 s2]]  (and (beacons* s1) (beacons* s2))) scanner-pairs)))))))
