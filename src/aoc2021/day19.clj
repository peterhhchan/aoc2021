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

(defn rotations [v]
  (->>  (range 3)
        (mapcat #(rotate v %))))

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

(defn distance-map [scanner]
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

(defn manhattan-distances [scanners]
  (->> (combo/combinations (vals scanners) 2)
       (map (fn [[[x y z] [a b c]]]
              (+  (Math/abs (- x a))
                  (Math/abs (- y b))
                  (Math/abs (- z c)))))
       (apply max)))

(defn part1 []
  (let [input     (vec (parse-input))
        distances (->> input
                       (map-indexed
                        (fn [i scanner]
                          [i (->> (combo/combinations scanner 2)
                                  (map (fn [[x y]]
                                         (distance x y)))
                                  set)])))
        pairs     (->> (combo/combinations distances 2)
                       (keep (fn [[a b]]
                               (when (>= (count (clojure.set/intersection (second a) (second b)))
                                         66)
                                 [(first a) (first b)]))))
        [a b]     (first pairs)]
    (loop [members  {a (set (input a))}
           scanners {a [0 0 0]}
           pairs    pairs]
      (if (empty? pairs)
        {:counts    (count (apply clojure.set/union (vals members)))
         :distances (manhattan-distances scanners)}
        (let [[a b]        (->> pairs
                                (keep #(cond (members (first %))
                                             [(first %) (second %)]
                                             (members (second %))
                                             [(second %) (first %)]))
                                first)
              x            (distance-map (members a))
              y            (distance-map (input b))
              pair         (first (clojure.set/intersection (set (keys x)) (set (keys y))))
              [rot offset] (find-rotation (x pair) (y pair))
              members*      (->> (map (partial rotate rot) (input b))
                                 (map (partial add offset))
                                 set
                                 (assoc members b) )]
          (recur members*
                 (merge scanners {b offset})
                 (->> pairs
                      (remove #(and (members* (first %))
                                    (members* (second %)))))))))))
