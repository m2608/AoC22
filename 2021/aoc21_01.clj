(ns aoc21_01
  (:require [clojure.string :as s]))

(def data (slurp "aoc21_01.txt"))

(def sample "199
200
208
210
200
207
240
269
260
263")

(defn parse-int [s]
  (Integer/parseInt s))

(defn get-changes
  "Получает разницу между соседними элементами списка."
  [numbers]
  (mapv - (rest numbers) (butlast numbers)))

(defn slides
  "Возвращает все последовательные группы по `n` элементов."
  [n numbers]
  (loop [numbers numbers result []]
    ;; Выходим из рекурсии, если осталось меньше n элементов.
    (if (empty? (drop (dec n) numbers)) result
      (recur (rest numbers) (conj result (take n numbers))))))


(->> data
     (s/split-lines)
     (map parse-int)
     (get-changes)
     (filter #(> % 0))
     (count))

(->> data
     (s/split-lines)
     (map parse-int)
     (slides 3)
     (map (partial apply +))
     (get-changes)
     (filter #(> % 0))
     (count))
