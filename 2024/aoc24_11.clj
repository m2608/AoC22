(require '[clojure.string :as str])

(def data "125 17")

(defn evolve-stone
  "Функция выполняет эволюцию одного камня в список камней."
  [stone]
  (let [digits (str stone)
        length (count digits)]
    (cond (= stone 0) [1]
          (even? length) (mapv Integer/parseInt [(subs digits 0 (quot length 2))
                                                 (subs digits (quot length 2))])
          :else [(* stone 2024)])))

(def evolve
  "Выполняет эволюцию камней указанное число раз и считает
  получившееся количество."
  (memoize 
    (fn [stones n]
      (if (zero? n) (count stones)
        (reduce + (mapv (fn [stone]
                          (evolve (evolve-stone stone) (dec n)))
                        stones))))))

(let [stones (mapv Integer/parseInt
                   (str/split (str/trim-newline (slurp "aoc24_11.txt")) #" "))]
  (mapv (partial evolve stones) [25 75]))
