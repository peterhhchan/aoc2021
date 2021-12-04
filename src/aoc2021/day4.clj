(ns aoc2021.day4)

(defn read-data []
  (let [[n & lines] (->> (slurp "data/day4.txt")
                          clojure.string/split-lines)]
    {:numbers (->> (clojure.string/split n #",")
                   (map #(Integer/parseInt %)))
     :cards   (->> (partition 6 lines)
                   (map rest)
                   (map (fn [cards]
                          (let [c (->> (mapv (fn [card]
                                               (->> (clojure.string/split (clojure.string/trim card) #" +")
                                                    (mapv #(Integer/parseInt %))))
                                             cards))]
                            (concat (map (partial into #{}) c)
                                    (->> (apply map vector c)
                                         (map (partial into #{}))))))))}))

(defn winner? [card]
  (->> (map empty? card)
       (some true?)))

(defn score [card number]
  (* number
     (->> (flatten card)
          (apply clojure.set/union)
          (reduce +))))

(defn winners []
  (let [{:keys [numbers cards]} (read-data)]
    (loop [cs cards
           ns numbers
           scores []]
      (if (seq cs)
        (let [updated (map (partial map #(disj % (first ns)) ) cs)
              winner  (filter winner? updated)]
          (if (seq winner)
            (recur (remove winner? updated)
                   (rest ns)
                   (conj scores (score (first winner) (first ns))))
            (recur updated (rest ns) scores)))
        scores))))

(defn part1 []
  (first (winners)))

(defn part2 []
  (last (winners)))
