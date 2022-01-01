(ns aoc2021.day24
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn data []
  (->>
       (str/split (slurp "data/day24.txt") #"inp")
       rest
       (map (comp rest str/split-lines))))

(def xyzw (zipmap  ["x" "y" "z" "w"] (range)))

(defn solve [ins input]
  (reduce (fn [res l]
            (let [[_ i j] (str/split l #" ")]
              (cond
                (str/starts-with? l "mul")
                (let [a  (get xyzw i)
                      av (res a)
                      b  (if (get xyzw j)
                           (res (get xyzw j))
                           (Integer/parseInt j))]
                  (assoc res a (* av b)))

                (str/starts-with? l "add")
                (let [a  (get xyzw i)
                      av (res a)
                      b  (if (get xyzw j)
                           (res (get xyzw j))
                           (Integer/parseInt j))]
                  (assoc res a (+ av b)))

                (str/starts-with? l "div")
                (let [a  (get xyzw i)
                      av (res a)
                      b  (if (get xyzw j)
                           (res (get xyzw j))
                           (Integer/parseInt j))]
                  (if (zero? b)
                    (reduced nil)
                    (assoc res a (quot av b))))

                (str/starts-with? l "eql")
                (let [a  (get xyzw i)
                      av (res a)
                      b  (if (get xyzw j)
                           (res (get xyzw j))
                           (Integer/parseInt j))]
                  (assoc res a (if (= av b) 1 0)))

                (str/starts-with? l "mod")
                (let [a  (get xyzw i)
                      av (res a)
                      b  (if (get xyzw j)
                           (res (get xyzw j))
                           (Integer/parseInt j))]
                  (if (or (neg? av)
                          (not (pos? b)))
                    (reduced nil)
                    (assoc res a (mod av b)))))))
         input
          ins))

(defn calculate
  ([n]
   (let [instructions (data)]
     (calculate instructions n)))

  ([instructions n]
   (reduce (fn [res [ins digit]]
             (solve ins (assoc res 3 (- (int digit) (int \0)))))
           [0 0 0 0]
           (->> (interleave instructions (seq  (str n)))
                (partition 2)
                (take 14)))))

;; max 99299513899971
;; min 93185111127911
(defn part1 []
)
