(ns aoc21_02
  (:require [clojure.string :as s]))

(def data (slurp "aoc21_02.txt"))

(def sample "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn parse-int [s]
  (Integer/parseInt s))

(def step1
  "Изменения позиции субмарины для различных команд."
  {:up [0 -1]
   :down [0 1]
   :forward [1 0]})

(defn parse-command
  "Парсин команду в пару ключевое слово - значение аргумента."
  [line]
  (let [[cmd arg] (s/split line #" ")]
    [(keyword cmd) (parse-int arg)]))

(defn update-pos1
  "Обновляет позицию сабмарины для первой части. Позиция задается
  парой: горизонтальная позиция, глубина."
  [pos [cmd value]]
  (mapv + pos (map (partial * value) (step1 cmd))))

(defn update-pos2
  "Обновляет позицию сабмарины для второй части. Позиция задается
  мапой."
  [pos [cmd value]]
  (case cmd
    :up      (assoc pos :aim (- (:aim pos) value))
    :down    (assoc pos :aim (+ (:aim pos) value))
    :forward (assoc pos
                    :x (+ (:x pos) value)
                    :depth (+ (:depth pos) (* (:aim pos) value)))))

(->> data
     (s/split-lines)
     (map parse-command)
     (reduce update-pos1 [0 0])
     (apply *))

(->> data
     (s/split-lines)
     (map parse-command)
     (reduce update-pos2 {:x 0 :depth 0 :aim 0})
     ((fn [{x :x depth :depth}]
        (* x depth))))
