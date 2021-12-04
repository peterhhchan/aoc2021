(ns aoc2021.day4)

(defn read-data []
  (let [[n & lines] (->> (slurp "data/day4.txt")
                          clojure.string/split-lines)]
    {:numbers (->> (clojure.string/split n #",")
                   (map #(Integer/parseInt %)))
     :cards   (->> (partition 6 lines)
                   (map rest)
                   (map (fn [cs]
                          (let [c (->> (mapv (fn [card]
                                               (->> (clojure.string/split (clojure.string/trim card) #" +")
                                                    (mapv #(Integer/parseInt %))))
                                             cs))]
                            {:rows (concat (map (partial into #{}) c)
                                           (->> (apply map vector c)
                                                (map (partial into #{}))))}))))}))

(defn winner? [card]
  (->> (:rows card)
       (map empty?)
       (some true?)))

(defn part1 []
  (let [{:keys [numbers cards]} (read-data)]
    (loop [cs cards
           ns numbers]
      (let [updated    (map (fn [card]
                          (update card :rows (fn [rows] (map #(disj % (first ns)) rows ))))
                        cs)

            winner (filter winner? updated)]
        (if (seq winner)
          (* (first ns)
             (->> (flatten (:rows (first winner)))
                  (apply clojure.set/union)
                  (reduce +)))
          (recur updated (rest ns)))))))

(defn part2 []
  (let [{:keys [numbers cards]} (read-data)]
    (loop [cs cards
           ns numbers]
      (let [new    (map (fn [card]
                          (update card :rows (fn [rows] (map #(disj % (first ns)) rows ))))
                        cs)
            winner (filter winner? new)]
        (if (and (seq winner)
                 (= 1 (count cs)))
          (* (first ns)
             (->> (flatten (:rows (first winner)))
                  (apply clojure.set/union)
                  (reduce +)))
          (recur (remove winner? new)
                 (rest ns)))))))
