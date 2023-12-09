(ns aoc23_09
  (:require [clojure.string :as str]))


(def data
"0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(comment
  (def data (slurp "aoc23_09.txt")))

(defn parse-line
  "Разделяем строку по пробелам и парсим числа."
  [line]
  (mapv #(Integer/parseInt %) (str/split line #"\s+")))

(defn next-difference 
  "Считаем разницу между соседними элементами списка."
  [values]
  (mapv - (rest values) (butlast values)))

(defn build-differences
  "Считаем разницы между соседними элементами, пока не получатся
  нули. Возвращаем последовательности левых и правых элементов из
  списков разниц."
  [values]
  (loop [vs values acc-l '() acc-r '()]
    (if (every? zero? vs) [acc-l acc-r]
      (recur (next-difference vs)
             (conj acc-l (first vs))
             (conj acc-r (last vs))))))

(->> data
     str/split-lines
     (map (comp (fn [[left right]]
                  [(reduce + right)
                   (reduce #(- %2 %1) left)])
                build-differences
                parse-line))
     (apply map +))
