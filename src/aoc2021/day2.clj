(ns aoc2021.day2)

;; up 7
;; down 3
;; up 3
;; forward 5
;; forward 9
;; down 3
;; down 7
;; down 5
;; forward 7

(defn read-data []
  (->> (slurp "data/day2.txt")
       clojure.string/split-lines
       (map (fn [s]
              (let [[d n]
                    (clojure.string/split s #" " )]
                [d (Integer/parseInt n)])))))


(defn part1 []
  (let [d (read-data)]
    (->> d
         (map (fn [[d n]]
                (case d
                  "forward" {:x n}
                  "down" {:z (- n)}
                  "up" {:z n})))
         (apply merge-with +)
         (vals)
         (reduce *))))


(defn part2 []
  (let [res (->> (read-data)
                 (map (fn [[d n]]
                        (case d
                          "forward" {:x n }
                          "down"    {:aim (- n)}
                          "up"      {:aim n})))
                 (reduce (fn [{:keys [horizontal depth current-aim]} {:keys [x aim]}]
                           (if aim
                             {:current-aim (+ current-aim aim )
                              :horizontal  horizontal
                              :depth       depth}
                             ;; forward
                             {:current-aim current-aim
                              :horizontal  (+ x horizontal)
                              :depth       (+ depth (* x current-aim))}))
                         {:current-aim 0
                          :horizontal  0
                          :depth       0}))]
    (->> (select-keys res [:depth :horizontal])
         vals
         (reduce *))))
