(ns aoc2021.day18
  (:require [clojure.string :as str]
            [clojure.zip :as zip]))

(defn data [] (slurp "data/day18.txt"))
(defn parse-input []
  (->> (data)
       (str/split-lines)
       (map read-string)))

(defn pair-node? [loc]
  (let [node (zip/node loc)]
    (and (sequential? node)
         (number? (first node))
         (number? (second node)))))

(defn number-node? [loc]
  (number? (zip/node loc)))

(defn height [n loc]
  (->> (iterate zip/up loc)
       (drop n)
       first))

(defn update-explode [l]
  (loop [loc l]
    (cond
      (and (height 4 loc)
           (pair-node? loc))
      (zip/next (zip/replace loc 0))

      :else
      (recur (zip/next loc)))))

(defn update-left [l v]
  (loop [loc l]
    (cond
      (number-node? loc)
      (zip/next (zip/edit loc + v))

      (nil? (zip/prev loc))
      loc

      :else
      (recur (zip/prev loc)))))

(defn update-right [l v]
  (loop [loc l]
    (cond
      (number-node? loc)
      (zip/root (zip/edit loc + v))

      (zip/end? (zip/next loc))
      (zip/root loc)

      :else
      (recur (zip/next loc)))))

(defn explode [t]
  (let [n (->> (zip/vector-zip t)
               (iterate zip/next )
               (take-while (complement zip/end?))
               (filter #(height 4 %))
               (filter pair-node?)
               first)]
   (if (nil? n)
     t
     (let [[a b] (zip/node n)]
       (-> (update-left (zip/prev n) a)
           (update-explode)
           (update-right b))))))

(defn split [t]
  (let [loc (->> (zip/vector-zip t)
                 (iterate zip/next)
                 (take-while (complement zip/end?))
                 (filter (fn [loc]
                           (let [node (zip/node loc)]
                             (and (number? node)
                                  (> node 9)))))
                 first)]
    (if loc
      (let [v (zip/node loc)]
        (zip/root (zip/replace loc [(int (Math/floor (/ v 2)))
                                    (int (Math/ceil  (/ v 2)))])))
      t)))

(defn add [a b]
  (loop [fish (vector a b)]
    (let [exploded (explode fish)]
      (if (= fish exploded)
        (let [split (split exploded)]
          (if (= split exploded)
            fish
            (recur split)))
        (recur exploded)))))

(defn eval-tree [t]
  (let [vz (zip/vector-zip t)]
    (loop [l (zip/next vz)]
      (cond
        (pair-node? l)
        (let [[a b] (zip/node l)
              n  (zip/replace l (+ (* 3 a) (* 2 b)) )]
          (if (nil? (zip/up n))
            (zip/node n)
            (recur (zip/up n))))

        (zip/next l)
        (recur (zip/next l))

        (zip/up l)
        (recur (zip/up l))

        :else
        (zip/node l)))))

;; 3892
(defn part1 []
  (loop [t (reduce (fn [col item]
                      (add col item))
                    (first (parse-input))
                    (rest  (parse-input)))]
    (eval-tree t)))

;; ~40s - 4909
(defn part2 []
  (let [d (vec (parse-input))]
    (->>  (for [x (range (count d))
                y (range (count d))
                :when (not= x y)]
            (eval-tree (add (d x) (d y))))
          sort
          last)))


(defn test-data []
  ["[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

"[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]"


"[1,1]
[2,2]
[3,3]
[4,4]"

"[1,1]
[2,2]
[3,3]
[4,4]
[5,5]"

"[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]"

"[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]
[7,7]"
])



(defn test-1 []
  (->> [["[[[[[9,8],1],2],3],4]" "[[[[0,9],2],3],4]"]
        ["[7,[6,[5,[4,[3,2]]]]]" "[7,[6,[5,[7,0]]]]"]
        ["[[6,[5,[4,[3,2]]]],1]" "[[6,[5,[7,0]]],3]"]
        ["[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"]
        ["[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"]]
       (map (partial map read-string))
       (map (fn [[a b]]
              (=  b (explode a))))))

(defn test-2 []
  (->>  [["[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]" "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"]
         ["[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
          "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
          "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"]
         ["[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
          "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
          "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"]
         ["[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
          "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
          "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"]]
        (map (partial map read-string))
        (map (fn [[a b c]]
               (= c  (add a b))))))
