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

(defn contain?
  "Проверяет, входит ли один диапазон во в другой."
  [[[a b] [x y]]]
  (or (and (<= a x b) (<= a y b))
      (and (<= x a y) (<= x b y))))
      
(defn overlap?
  "Проверяет, пересекаются ли диапазоны."
  [[[a b] [x y]]]
  (or (<= a x b)
      (<= a y b)
      (<= x a y)
      (<= x b y)))

(defn count-overlaps
  "Считает количество пересекающихся диапазонов в исходных данных.
  Признаком пересечения служит результат выполнения функции, переданной
  в качестве второго аргумента."
  [data overlap-fn]
  (->> (str/split-lines data)
     (map (fn [line] (->> (str/split line #",")
                          (map parse-range)
                          (overlap-fn))))
     (filter identity)
     (count)))

(count-overlaps data contain?)

(count-overlaps data overlap?)

