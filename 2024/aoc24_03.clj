(def data1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def data2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn part1
  [data]
  (->> data
       (re-seq #"mul[(]([0-9]+),([0-9]+)[)]")
       (map (fn [[_ a b]]
              (apply * (map Integer/parseInt [a b]))))
       (reduce +)))


(defn part2
  [data]
  (->> data
       (re-seq #"(mul|do|don't)[(](([0-9]+),([0-9]+))?[)]")
       (reduce (fn [[acc enabled] [_ op _ a b]]
                 (case op
                   "mul"   [(if enabled (+ acc (apply * (map Integer/parseInt [a b]))) acc) enabled]
                   "do"    [acc true]
                   "don't" [acc false]))
               [0 true])
       first))

((juxt part1 part2) (slurp "aoc24_03.txt"))
