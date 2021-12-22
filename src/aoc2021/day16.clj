(ns aoc2021.day16
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.zip :as zip]))

(defn data [] (slurp "data/day16.txt"))

(def bits ["0" "1"])
(def char-to-bits
  (zipmap "0123456789ABCDEF"
          (for [a bits b bits c bits d bits]
            (str a b c d))))

(defn bits [input]
  (->> (map char-to-bits input)
       (str/join)))

(defn to-num [bits]
  (read-string (apply str "2r" bits)))

(defn decode-bits
  ([msg]
   (decode-bits msg (count msg)))

  ([msg bits-to-parse]
   (if (or  (every? #{\0} msg)
            (>= 0 bits-to-parse))
     nil
     (let [version (to-num (subs msg 0 3))
           type-id (to-num (subs msg 3 6))
           packet  {:type-id type-id
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
                                    (to-num))
               remaining       (- bits-to-parse packet-length)]
           (concat [(merge packet {:value value})]
                   (decode-bits (subs msg packet-length (+ packet-length remaining))
                                remaining)))
         (let [length-type (nth msg 6)
               res         (assoc packet :op length-type)]
           (if (= \0 length-type)
             (let [len (to-num (subs msg 7 (+ 7 15)) )]
               (concat [(assoc res :children (decode-bits (subs msg 22 (+ 22 len)) len))]
                       (decode-bits (subs msg (+ 22 len))
                                    (- bits-to-parse (+ 22 len)))))
             (let [n            (to-num (subs msg 7 (+ 7 11)) )
                   next-packets (decode-bits (subs msg 18)
                                             (- (count msg) 18))]
               (concat [(assoc res
                               :sub-packets n
                               :children    (take n next-packets))]
                       (drop n next-packets))))))))))

(defn part1 []
  (->> (data)
       (bits )
       decode-bits
       (version-sum )))


(defn version-sum [t]
  (if (map? t)
    (reduce +
            (:version t)
            (map version-sum (:children t)))
    (reduce + (map version-sum t))))


(defn reduce-by-type [t]
  (cond (nil? t) nil
        (not (map? t)) (reduce-by-type (first t))
        (:value t) (:value t)
        :else
        (let [compare-children #(get {true 1 false 0}
                                     (% (reduce-by-type (first (:children t)))
                                        (reduce-by-type (second (:children t)))))
              reduce-children #(reduce % (map reduce-by-type (:children t)))]
        (case (:type-id t)
          0 (reduce-children +)
          1 (reduce-children *)
          2 (reduce-children min)
          3 (reduce-children max)
          5 (compare-children > )
          6 (compare-children < )
          7 (compare-children = )))))


;; 12301926782560
(defn part2 []
  (->> (bits (data))
       decode-bits
       reduce-by-type)
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

(defn tests []
  (->> tests
       (map parse-input)
       (map decode-bits)
       (map version-sum)
       (zipmap tests)))
