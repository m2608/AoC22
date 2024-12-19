(require '[clojure.string :as str])

(def data "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(defn parse-data
  [data]
  (let [[towels designs] (str/split data #"\n\n")]
    [(map seq (str/split towels #", "))
     (map seq (str/split-lines designs))]))

(defn get-next-towels
  "Возвращает список следующих возможных полотенец для воплощения дизайна."
  [towels design]
  (filter (fn [towel]
            (= towel (take (count towel) design)))
          towels))

(def find-designs
  "Определяет, есть ли комбинация полотенец для воплощения дизайна."
  (memoize
    (fn [towels design]
      (if (empty? design) true
        (when-let [next-towels (get-next-towels towels design)]
          (some (fn [t]
                  (find-designs towels (drop (count t) design)))
                next-towels))))))

(def count-designs
  "Определяет количество способов представления дизайна с помощью набора полотенец."
  (memoize
    (fn
      [towels design]
      (if (empty? design) 1
        (let [next-designs (map (fn [t] (drop (count t) design))
                                (get-next-towels towels design))]
          (reduce + (map (partial count-designs towels) next-designs)))))))

(let [[towels design] (parse-data (slurp "aoc24_19.txt"))]
  [(count (filter (partial find-designs towels) design))
   (reduce + (map (partial count-designs towels) design))])
