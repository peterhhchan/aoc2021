(ns aoc2021.day4)

(defn str-to-ints [s]
  (->> (re-seq #"\d+" s)
       (map #(Integer/parseInt %))))

(defn read-data []
  (let [[f & lines] (->> (slurp "data/day4.txt")
                         clojure.string/split-lines)]
    {:numbers (str-to-ints f)
     :cards   (->> (map str-to-ints lines)
                   flatten
                   (partition 25)
                   (map #(let [rows (partition 5 %)]
                           (->> (apply map vector rows)
                                (concat rows )
                                (map (partial into #{}))))))}))

(defn score [card number]
  (* number
     (->> (apply clojure.set/union card)
          (reduce +))))

(defn winners []
  (let [{:keys [numbers cards]} (read-data)]
    (loop [cs           cards
           [num & nums] numbers
           scores       (list)]
      (if (and (seq cs) num)
        (let [results (->> cs
                           (map (partial map #(disj % num)))
                           (group-by #(some true? (map empty? %))))]
            (recur (results nil)
                   nums
                   (->> (results true)
                        (map #(score % num) )
                        (apply conj scores))))
        scores))))


(def part1  (first (winners))) ;; 41503
(def part2  (last  (winners))) ;; 3178
