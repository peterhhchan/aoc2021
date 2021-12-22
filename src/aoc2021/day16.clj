(ns aoc2021.day16
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.zip :as zip]))

(defn data [] (slurp "data/day16.txt"))

(def bits ["0" "1"])
(defn decoder []
  (zipmap "0123456789ABCDEF"
          (for [a bits b bits c bits d bits] (str a b c d))))

(defn parse-input [input]
  (->> input
       (map (decoder))
       (str/join)))

(defn to-num [bits]
  (read-string (apply str "2r" bits)))

(defn my-decode
  ([msg]
   (my-decode msg (count msg)))

  ([msg bits-to-parse]
   (if (or  (every? #{\0} msg)
            (>= 0 bits-to-parse))
     nil
     (let [version (to-num (subs msg 0 3))
           type-id (to-num (subs msg 3 6))
           packet {:type-id type-id
                   :version version}]
       (if (= 4 type-id)
         (let [packets         (->> (subs msg 6)
                                    (partition 5))
               [lead-bits & r] (split-with #(= \1 (first %)) packets)
               total-bits      (inc (count lead-bits))
               packet-length   (+ 6  (* 5 total-bits))
               value           (->> (take total-bits packets)
                                    (map rest)
                                    (map (partial str/join))
                                    (str/join)
                                    (str "2r")
                                    (read-string))
               remaining         (- bits-to-parse packet-length)]
           (if (pos? remaining)
             (concat [(merge packet {:value value})]
                     (my-decode (subs msg packet-length (+ packet-length remaining)) remaining))
             [(merge packet {:value value})]))
         (let [length-type (nth msg 6)
               res (merge packet
                          {:op length-type})]
           (if (= \0 length-type)
             (let [total-length (to-num (subs msg 7 (+ 7 15)) )]
               (concat [(merge res
                               {:children (my-decode (subs msg 22 (+ 22 total-length)) total-length )})]
                       (my-decode (subs msg (+ 22 total-length))
                                  (- bits-to-parse (+ 22  total-length)))))
             (let [sub-packets (to-num (subs msg 7 (+ 7 11)) )
                   next-packets (my-decode (subs msg 18) (- (count msg) 18))]
               (concat [(merge res
                               {:sub-packets sub-packets
                                :children (take sub-packets next-packets)})]
                       (drop sub-packets next-packets))))))))))

(defn ppart1 []
  (->> (data)
       (parse-input)
       my-decode
       (version-sum )))


(defn version-sum [t]
  (if (map? t)
    (reduce +
            (:version t)
            (map version-sum (:children t)))
    (reduce + (map version-sum t))))


(defn solve [t]
  (if (map? t)
    (if (:value t)
      (:value t)
      (let [type-id    (:type-id t)
            my-compare (fn  [op n]
                         (when (or (nil? (first n))
                                 (nil? (second n))))
                         (if (op (solve (first n))
                                 (solve (second n)))
                           1 0))]
        (case type-id
          0 (reduce + (map solve (:children t)))
          1 (reduce * (map solve (:children t)))
          2 (apply min (map solve (:children t)))
          3 (apply max (map solve (:children t)))
          5 (my-compare > (:children t))
          6 (my-compare < (:children t))
          7 (my-compare = (:children t)))))
    (if (nil? t)
      nil
      (solve (first t)))))



(defn part2 []
  (->> (data)
       (parse-input)
       my-decode
       solve)
)

(def tests
  (sort  [["38006F45291200" 9]
          ["8A004A801A8002F478" 16]
          ["620080001611562C8802118E34" 12]
          ["D2FE28" 6]
          ["EE00D40C823060" 14]
          ["C0015000016115A2E0802F182340" 23]
          ["A0016C880162017C3686B18A3D4780" 31]
          ;;  (data)
          ]))

(defn part1 []
  (->> tests
       (map parse-input)
       (map my-decode)
       (map version-sum)
       (zipmap tests)
       ))
