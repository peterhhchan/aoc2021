(ns aoc2021.day23
  (:require  [clojure.string :as str]
             [clojure.data.priority-map :refer [priority-map]]
             [shams.priority-queue :as pq]))

;; --- Day 23: Amphipod ---
;; The solution here isnt great.  Nothing to see here.

(def cave-addition
"  #D#C#B#A#
  #D#B#A#C#")

(defn data []
  (->> (slurp "data/day23.txt")
       (str/split-lines)))

(defn data-part-two []
  (let [d (data)]
    (vec (concat (take 3 d)
                 (str/split-lines cave-addition)
                 (drop 3 d)))))

(defn parse-input [input]
  (let [squares (set ".ABCD")]
    (->> (for [y (range (count input))
               x (range (count (first input)))
               :let [c (get-in input [y x])]
               :when (squares c)]
               {[y x] c})
         (into {}))))

(defn neighbors [[a b]]
  [[(inc a) b] [(dec a) b]
   [a (inc b)] [a (dec b)]])

(defn hallway? [[x y]] (= x 1))
(defn starting-room? [[x y]]  (> x 1))
(defn empty-square? [x]  (= \. x))
(defn blocking-room? [[x y]]
  (and (= x 1) (#{3 5 7 9} y)))

(defn cost [cnx start end]
  (loop [costs {start 0}
         n     0]
    (let [updated (->> (keys costs)
                       (mapcat neighbors)
                       (filter cnx)
                       (remove costs))]
      (if (seq updated)
        (recur (merge costs (zipmap updated (repeat (inc n))))
               (inc n))
        costs))))

(defn path [cnx start end]
  (let [costs (cost cnx start end)]
    (loop [path (list end)
           cost (costs end)]
      (if (zero? cost)
        path
        (recur (conj path
                     (->> (neighbors (first path))
                          (filter cnx)
                          (filter #(= (dec cost) (costs %)))
                          first))
               (dec cost))))))

(def home-columns [ 3 5 7 9] )
(def move-costs (zipmap "ABCD" [1 10 100 1000]))
(def final-pos (zipmap "ABCD" home-columns))
(defn home-column-empty? [state piece-type]
  (->> state
       (filter (fn [[[x y] square]]
                 (and (> x 1)
                      (= y (final-pos piece-type)))))
       (every? (fn [[k v]] (empty-square? v)))))

(defn possible-moves [state paths [pos piece-type]]
  (->> state
       (filter (fn [[d v]]
                 (and (empty-square? v)
                      (not (blocking-room? d))
                      (or (and (starting-room? pos)
                               (not (starting-room? d)))
                          (and (starting-room? d)
                               (= (second d) (final-pos piece-type))
                               (home-column-empty? state piece-type))))))
       (keep (fn [[d v]]
               (let [p (rest (paths [pos d]))]
                 (when (every? #(empty-square? (state %)) p)
                   {:pos         pos
                    :piece       piece-type
                    :dest        d
                    :cost        (* (move-costs piece-type) (count p))
                    :home-square (starting-room? d)}))))))

(defn brute-force [data max-cost]
  (let [cavern         (->  data
                            (parse-input))

        paths          (->> (for [s     (keys cavern)
                                  e     (keys cavern)
                                  :when (not= s e)]
                              {[s e] (path cavern s e)})
                            (into {}))

        _ (def mem-game
            (memoize (fn [state current-cost]
                       (let [moves (->> state
                                         (remove (fn [[k v]]
                                                   (empty-square? v)))
                                         (filter (fn [[k v]]
                                                   (->> (keep state (neighbors k))
                                                        (some empty-square?))))
                                         (mapcat #(possible-moves state paths %)))]
                         (cond
                           (= 1 (count (group-by second state)))
                           current-cost
                           (or (empty? moves)
                               (> current-cost max-cost))
                           max-cost
                           :else
                           (let [home (filter :home-square moves)]
                             (if (seq home)
                               (let [h (->> (sort-by :cost home)
                                            reverse
                                            first)]
                                 (mem-game (-> (dissoc state (:dest h))
                                               (assoc (:pos h) \.))
                                           (+ current-cost (:cost h))))
                               (->> moves
                                    (map (fn [{:keys [pos piece dest cost]}]
                                           (mem-game (-> (assoc state dest piece)
                                                         (assoc pos \.))
                                                     (+ cost current-cost))))
                                    (apply min)))))))))]
    (mem-game cavern 0)))


;;13066 - 33s
(defn part1 []
  (brute-force (data) 15000))

;;47328 - 34s
(defn part2 []
  (brute-force (data-part-two) 50000))
