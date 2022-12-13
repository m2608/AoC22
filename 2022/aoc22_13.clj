(ns aoc22_13
  (:require [clojure.string :as s]
            [cheshire.core :as json]))

(def data (slurp "aoc22_13.txt"))

(def sample "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(defn compare-vectors
  "Рекурсивно сравнивает векторы по правилам задания."
  [v1 v2]
  (cond
    ;; Оба аргумента числа, сравниваем как числа.
    (every? number? [v1 v2]) (compare v1 v2)
    ;; Один из аргументов число - преобразуем в вектор.
    (number? v1) (compare-vectors [v1] v2)
    (number? v2) (compare-vectors v1 [v2])
    ;; Оба вектора пустые - значит они равны.
    (every? empty? [v1 v2]) 0
    ;; А если пуст только один, значит больше другой.
    (empty? v1) -1
    (empty? v2) 1
    ;; Сравниваем векторы поэлементно.
    :else (let [cmp (compare-vectors (first v1) (first v2))]
            (if (zero? cmp) (compare-vectors (rest v1) (rest v2))
              cmp))))

(defn find-packet
  "Ищет пакет в пакетах. Возвращает индекс (с 1)."
  [packets packet]
  (->> packets (take-while #(not= % packet)) (count) (inc)))

(->> (s/split data #"\n\n")
     (map #(->> (map json/parse-string (s/split-lines %))))
     (map-indexed (fn [idx [v1 v2]]
                    (if (> 0 (compare-vectors v1 v2)) (inc idx) 0)))
     (reduce +)
     (println))

(->> (s/split-lines data)
     (filter (comp not empty?))
     (map json/parse-string)
     ;; Добавляем разделяющие пакеты.
     (concat [[[2]] [[6]]])
     (sort-by identity compare-vectors)
     ;; Ищем разделяющие пакеты в отсортированном списке.
     (#(vector (find-packet % [[2]])
               (find-packet % [[6]])))
     (reduce *)
     (println))

