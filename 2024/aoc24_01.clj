(require '[clojure.string :as str]
         '[clojure.set :as set])

(def data-test "3   4
4   3
2   5
1   3
3   9
3   3")

(defn parse-lists
  [data]
  (->> data
       str/split-lines
       (map (fn [line] (->> (str/split line #"[ ]+")
                            (mapv Integer/parseInt))))
       (apply mapv vector)))

(defn part1
  [data]
  (->> data
       parse-lists
       (mapv sort)
       (apply mapv (comp abs -))
       (reduce +)))

(defn part2
  [data]
  (let [[xs ys] (->> data parse-lists (map frequencies))
        common-nums (set/intersection (set (keys xs))
                                      (set (keys ys)))]
    (reduce (fn [acc k]
              (+ acc (* k (xs k) (ys k))))
            0
            common-nums)))

((juxt part1 part2) (slurp "aoc24_01.txt"))
