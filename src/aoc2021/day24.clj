(ns aoc2021.day24
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn data []
  (->> (slurp "data/day24.txt")
       (str/split-lines)
       (partition-by #(str/starts-with? % "inp"))
       (partition 2)
       (map (comp first rest))))

(defn constraints []
  (->> (data)
       (map (fn [ins]
              (let [push-pop (nth ins 3)
                    x        (nth ins 4)
                    y        (nth ins 14)]
                (->> (list push-pop x y)
                     (map (comp read-string first #(re-seq #"\d+" %)))))))
       (interleave (range))
       (partition 2)
       (reduce (fn [{:keys [stack constraints] :as res} [b [op x y]]]
                 (case op
                   1  (update res :stack #(conj % [b y]))
                   26 (let [[a z] (first stack)]
                         (-> (update res :stack pop)
                             (update :constraints #(conj % [a b (- z x)]))))))
               {:stack       (list)
                :constraints (list)})
       :constraints))

;; max 99299513899971
(defn part1 []
  (->> (constraints)
       (reduce (fn [res [d1 d2 constraint]]
                 (if (neg? constraint)
                   (-> res
                       (assoc d1 9)
                       (assoc d2 (+ 9 constraint)))
                   (-> res
                       (assoc d2 9)
                       (assoc d1 (- 9 constraint)))))
               (vec (repeat 14 0)))
       (str/join)
       read-string))

;; min 93185111127911
(defn part2 []
  (->> (constraints)
       (reduce (fn [res [d1 d2 constraint]]
                 (if (neg? constraint)
                   (-> res
                       (assoc d1 (inc (* constraint -1)))
                       (assoc d2 1))
                   (-> res
                       (assoc d2 (+ 1 constraint))
                       (assoc d1 1))))
               (vec (repeat 14 0)))
       (str/join)
       read-string))


;;
;;  Original Approach
;;

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
