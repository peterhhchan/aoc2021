(ns aoc2021.day4)

(defn str-to-ints [s]
  (->> (re-seq #"\d+" s)
       (map #(Integer/parseInt %))))

(defn input [] (slurp "data/day4.txt"))

(defn parse-input [input]
  (let [[f & lines] (->> input
                         clojure.string/split-lines)]
    {:numbers (str-to-ints f)
     :cards   (->> (map str-to-ints lines)
                   flatten
                   (partition 25)
                   (map #(->> (partition 5 %)
                              ((juxt identity (partial apply map vector)))
                              (apply concat)
                              (map (partial into #{})))))}))

(defn score [card number]
  (* number
     (->> (apply clojure.set/union card)
          (reduce +))))

(defn find-winner [cs [n & nums]]
  (lazy-seq
   (when n
     (let [results (->> cs
                        (map (partial map #(disj % n)))
                        (group-by #(some true? (map empty? %))))
           new-scores (->> (results true)
                           (map #(score % n)))]
       (concat new-scores (find-winner (results nil) nums))))))

(defn winners [input]
  (let [{:keys [numbers cards]} (parse-input input)]
    (find-winner cards numbers )))


;; 41503
(defn part1 [input]
  (first (winners input)))

;; 3178
(defn part2 [input]
  (last (winners input)))
