(ns aoc22_03
  (:require [clojure.string :as str])
  (:use [clojure.set :only [intersection]]))

;; Рабочие данные.
(def data (slurp "aoc22_03.txt"))

;; Тестовые данные.
(def data "vJrwpWtwJgWrhcsFMMfFFhFp
          jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
          PmmdzqPrVvPwwTWBwg
          wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
          ttgJtRGJQctTZtZT
          CrZsJsPPZsGzwwsLwLmpwMDw")

(defn bifurcate
  "Разбивает строку на две части."
  [s]
  (let [half-len (quot (count s) 2)]
    (list (subs s 0 half-len)
          (subs s half-len))))

(defn priority
  "Возвращает «приоритет», порядковый номер буквы."
  [letter]
  (let [letters-down "abcdefghijklmnopqrstuvwxyz"
        ;; Лишний пробел в начале, чтобы индекс был с 1.
        letters-all (str " " letters-down (str/upper-case letters-down))]
    (str/index-of letters-all letter)))

;; Хотел сгенерить алфавит, но просто строчка букв лучше, понятнее.
(->> (range 26)
     (map #(char (+ % (int \a))))
     (str/join))

(defn priorities
  "Возвращает сумму «приоритетов» для всех переданных букв."
  [letters]
  (reduce + (map priority letters)))

(->> data
     (str/split-lines)
     (map #(->> (str/trim %)
                (bifurcate)
                (map set)
                (apply intersection)
                (priorities)))
     (reduce +))

(->> data
     (str/split-lines)
     (map str/trim)
     (partition 3)
     (map #(->> (map set %)
                (apply intersection)
                (priorities)))
     (reduce +))

