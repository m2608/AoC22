(ns aoc23_13
  (:require [clojure.string :as str]))

(def data
"#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(comment
  (def data (slurp "aoc23_13.txt")))

(defn parse-data
  "Парсит набор паттернов, возвращает несколько наборов строк."
  [data]
  (->> (str/split data #"\n\n")
       (map (fn [pattern]
              (->> pattern
                   str/split-lines)))))

(defn count-lines-differences
  "Считает количество отличий в двух строках одной длины."
  [& lines]
  (->> lines
       (apply mapv not=)
       (filter identity)
       count))

(defn check-mirror
  "Проверяет, является ли деление паттерна после строки `idx` зеркальным
  с количеством отличий `diff`."
  [diff pattern idx]
  (let [cnt (min idx (- (count pattern) idx))
        line1 (->> pattern (take idx) (drop (- idx cnt)) str/join)
        line2 (->> pattern (drop idx) (take cnt) reverse str/join)]
    (= diff (count-lines-differences line1 line2))))

(defn find-mirror
  "Ищет линию зеркальной симметрии в паттерне. Возвращает 0, если такой нет."
  [diff pattern]
  (or (first (filter (partial check-mirror diff pattern)
                     (range 1 (count pattern))))
      0))

(defn transpose
  "Транспонирует паттерн."
  [pattern]
  (apply mapv vector pattern))

(defn get-mirrors-score
  "Считает скор для паттерна по условиям задачи. Также принимает на вход
  количество отличий, которое должно присутствовать в отражениях."
  [diff pattern]
  (+ (find-mirror diff (transpose pattern))
     (* 100 (find-mirror diff pattern))))

(let [patterns (parse-data data)]
  (map #(->> patterns
             (map (partial get-mirrors-score %))
             (reduce +))
     [0 1]))
