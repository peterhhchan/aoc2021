(ns aoc2021.day22
  (:require [clojure.string :as str]))


(defn data [] (slurp "data/day22.txt"))
(defn parse-long [n]   (Long/parseLong n))


(defn parse-row [r]
  (let [on? (str/starts-with? r "on")
        [a b c d e f] (->>  (re-seq #"-*\d+" r)
                            (map parse-long))]

    [on? [[a b] [c d] [e f]]]))

(defn cubes [[[a b] [c d] [e f]]]
  (for [x (range a (inc b))
        y (range c (inc d))
        z (range e (inc f))]
    [x y z]))

;; Simple but slow solution
(defn part1 []
  (let [d (->> (data)
               (str/split-lines)
               (map parse-row)
               (take-while (fn [[_ [[a b]]]]
                             (<= -51 a 51))))]
    (loop [on #{}
           instructions d]
      (if (empty? instructions)
        (count on)
        (let [[on? coords]  (first instructions)
              cs            (cubes coords)]
          (recur (if on?
                   (into on cs)
                   (apply disj on cs))
                 (rest instructions)))))))

(defn overlap [[min-a max-a] [min-b max-b]]
  (if (and (<= min-b max-a)
           (<= min-a max-b))
    (-  (min max-a max-b)
        (max min-a min-b))))

(defn intersect [[a b c] [d e f]]
  (and (overlap a d) (overlap b e) (overlap c f)))


(defn pts [[[a b] [c d] [e f]]]
  (* (inc (- b a))
     (inc (- d c))
     (inc (- f e))))


;; We can either keep the "cube", or split it into up to 3 new ones.
(defn split-cube
  ([c1 c2]
  (->> (split-cube c1 c2 0)
       (mapcat #(split-cube % c2 1))
       (mapcat #(split-cube % c2 2))))

  ([c1 c2 axis]
   (if (not (intersect c1 c2))
     [c1]
     (let [[min-1 max-1] (get c1 axis)
           [min-2 max-2] (get c2 axis)]
       (cond (and (>= max-2 max-1)
                  (<= min-2 min-1))
             [c1]
             (and  (>= max-1 min-2)
                   (>= max-2 max-1))
             [(assoc-in c1 [axis 0] min-2)
              (assoc-in c1 [axis 1] (dec min-2))]
             (and  (>= max-1 max-2)
                   (>= min-2 min-1))
             [(assoc-in c1 [axis 0] (inc max-2))
              (-> c1
                  (assoc-in [axis 1] max-2)
                  (assoc-in [axis 0] min-2))
              (assoc-in c1 [axis 1] (dec min-2))]
             (>= max-2 min-1)
             [(assoc-in c1 [axis 1] max-2)
              (assoc-in c1 [axis 0] (inc max-2))])))))

;; Fast solution by partitioning the cubes Our method is to divide the
;; "on" cubes into smaller cubes based on the new cube.  Remove the
;; pieces that intersect with the cube we are masking.  We can ignore
;; the new cube if its turning cubes off, or add the cube if we are
;; turning on .

;;1162571910364852
(defn part2 []
  (let [commands (->> (data)
                      (str/split-lines)
                      (map parse-row))]
    (loop [cubes         [(second (first commands))]
           [[on? c] & r] (rest commands)]
      (let [cubes* (->> cubes
                        (mapcat (fn [cube]
                                  (if (intersect c cube)
                                    (->> (split-cube cube c)
                                         (remove #(intersect % c)))
                                    [cube])))
                        (concat (when on? [c])))]
        (if r
          (recur cubes* r)
          (->> (map pts cubes*)
               (reduce +)))))))
