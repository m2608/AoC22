(ns aoc22_15
  (:require [clojure.string :as s]))

(def data (slurp "aoc22_15.txt"))

(def sample "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-data
  "Парсит информацию о сенсорах и маячках из исходных данных. Возвращает
  соответствующий им список пар координат."
  [data]
  (->> data
       (s/split-lines)
       (map #(->> %
                  (re-matches #"^Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)$")
                  (rest)
                  (map parse-int)
                  (partition 2)))))

(defn distance
  "Вычисляет «манхэттенское» расстояние между точками."
  [p1 p2]
  (apply + (map #(Math/abs %) (mapv - p1 p2))))

(defn find-range
  "Определяет, какой диапазон в указанной строке перекрывается сигналом от сенсора. В
  этом диапазоне не может находится маячок (кроме уже найденного сенсором, если ордината
  строки совпадает с ординатой маячка)."
  [y [xs ys :as sensor] beacon]
  (let [delta (- (distance sensor beacon) (Math/abs (- ys y)))]
    (if (< delta 0) [] [(- xs delta) (+ xs delta)])))

(defn merge-sorted-ranges
  "Объединяет отсортированные по левой границе диапазоны."
  [[x1 x2 :as rng1] [x3 x4 :as rng2]]
    (cond
      ;; Диапазоны целочисленные, т.к. если первый заканчивается, а второй начинается
      ;; на соседних числах, считаем это одним диапазоном.
      (< x2 (dec x3)) [rng1 rng2]
      (< x4 x2) [rng1]
      :else [[x1 x4]]))

(defn merge-all
  "Объединяет все переданные диапазоны. Возвращает список получившихся диапазонов."
  [ranges]
  (loop [ranges (sort-by first ranges) merged []]
    (cond
      ;; Все диапазоны обработаны.
      (empty? ranges) merged
      ;; На первом шаге просто добавляем первый диапазон в список.
      (empty? merged) (recur (rest ranges) [(first ranges)])
      ;; Последовательно объединяем следующий из списка и последний добавленный в результат диапазоны.
      :else (recur (rest ranges)
                   (reduce conj (pop merged) (merge-sorted-ranges (peek merged) (first ranges)))))))

(defn check-hidden-beacon
  "Проверяет точку на возможность размещения скрытого маячка."
  [point sensors-beacons]
  (every? (fn [[sensor beacon]]
            (> (distance sensor point) (distance sensor beacon)))
          sensors-beacons))

(defn get-ranges-for-line
  "Получает для указанной строки набор диапазонов, которые не могут быть
  заняты маячками."
  [y sensors-beacons]
  (->> sensors-beacons
       (map #(find-range y (first %) (second %)))
       (filter seq)
       (merge-all)))

(let [line-y 2000000]
  (->> data
       (parse-data)
       (get-ranges-for-line line-y)
       (reduce (fn [sum [x1 x2]]
                 (+ sum (- x2 x1))) 0)
       (println)))

;; TODO Решение должно быть рядом с пересечением границ областей сенсором.
;; Но пока это простой перебор.
(let [max-y 4000000
      sensors-beacons (parse-data data)]
  (->> (range (inc max-y) -1 -1) 
       (some (fn [y]
               (let [ranges (get-ranges-for-line y sensors-beacons)]
                 ;; Если в какой-то строке больше одного диапазона, значит
                 ;; между этими диапазонами и расположен ненайденный маячок.
                 (when (> (count ranges) 1)
                   (+ y (-> ranges (first) (second) (inc) (* max-y)))))))
       (println)))

