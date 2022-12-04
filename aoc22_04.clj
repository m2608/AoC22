(ns aoc22_04
  (:require [clojure.string :as str]))

;; Рабочие данные.
(def data (slurp "aoc22_04.txt"))

;; Тестовые данные.
(def data (str/join "\n" ["2-4,6-8"
                          "2-3,4-5"
                          "5-7,7-9"
                          "2-8,3-7"
                          "6-6,4-6"
                          "2-6,4-8"]))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-range
  "Получает числовой диапазон из строк вида \"2-4\".
  Возвращает список из двух чисел - границ диапазона."
  [pair]
  (map parse-int (str/split pair #"-")))

(defn is-within-the-range?
  "Проверяет, находится ли точка внутри дипазона."
  [[left right] point]
  (and (<= left point)
       (>= right point)))

(defn is-second-range-contained?
  "Проверяет входит ли второй диапазон в первый."
  [range1 [left2 right2]]
  (and (is-within-the-range? range1 left2)
       (is-within-the-range? range1 right2)))

(defn is-contains?
  "Проверяет, входит ли один диапазон во в другой."
  [[range1 range2]]
  (or (is-second-range-contained? range1 range2)
      (is-second-range-contained? range2 range1)))

(defn is-overlap?
  "Проверяет, пересекаются ли диапазоны."
  [[range1 range2]]
  ;; Диапазоны пересекаются, если один содержит другой или если
  ;; одна из границ второго диапазона лежит в пределах границ
  ;; первого (или наоборот).
  (or (is-contains? [range1 range2])
      (is-within-the-range? range1 (first range2))
      (is-within-the-range? range1 (second range2))))

(defn count-overlaps
  "Считает количество пересекающихся диапазонов в исходных данных.
  Признаком пересечения служит результат выполнения функции, переданной
  в качестве второго аргумента."
  [data overlap?]
  (->> (str/split-lines data)
     (map (fn [line] (->> (str/split line #",")
                          (map parse-range)
                          (overlap?))))
     (filter identity)
     (count)))

(count-overlaps data is-contains?)

(count-overlaps data is-overlap?)

