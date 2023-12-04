(ns aoc23_03
  (:require [clojure.string :as str]))

(def data
"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(comment 
  (def data (slurp "aoc23_03.txt")))

(defn parse-int [s]
  (Integer/parseInt s))

(def digit?
  "Множество десятичных цифр (в виде символов)."
  (set (map (comp first str) (range 10))))

(defn all-digits? 
  "Проверяет, все ли символы в строке являются цифрами."
  [line]
  (every? digit? line))

(defn parse-line
  "Разбивает строку на цифры и не-цифры."
  [line]
  (->> line
       (partition-by (comp not digit?))
       (map str/join)))

(defn find-numbers
  "Ищет числа в строке. Возвращает список найденных чисел, а также вектор из
  начальной координаты, конечной координаты и текущего номера строки."
  [line-number line]
  (loop [fragments (parse-line line) x 0 numbers []]
    (let [fragment (first fragments)
          fraglen (count fragment)]
      (if (seq fragments)
        ;; Если текущий фрагмент содержит число, добавляем информацию о нем в список.
        ;; Если нет - только увеличиваем текущую координату.
        (recur (rest fragments)
               (+ x fraglen)
               (if (all-digits? fragment)
                 (conj numbers [(parse-int fragment) [x (+ x fraglen -1) line-number]])
                 numbers))
        numbers))))

(defn parse-data
  "Парсит строки и возвращает набор числе с информацией о их местоположении."
  [data]
  (->> data
       (map-indexed find-numbers)
       (filter seq)
       (reduce concat)))

(defn get-adjacents [[x0 x1 y]]
  "Возвращает список координат смежных ячеек для указанной начальной координаты,
  конечной координаты и номера строки."
  (apply concat [[[(dec x0) y] [(inc x1) y]]
                 (map #(vector % (dec y)) (range (dec x0) (+ 2 x1)))
                 (map #(vector % (inc y)) (range (dec x0) (+ 2 x1)))]))

(defn check-coords
  "Проверяет, находится ли в указанной ячейка какой-то непустой символ (не точка)."
  [data [x y]]
  (let [smb (get-in data [y x])]
    (and (not (nil? smb))
         (not= smb \.))))

(defn check-gear
  "Проверяет, находится ли в указанной ячейке звездочка."
  [data [x y]]
  (let [smb (get-in data [y x])]
    (= smb \*)))

(defn check-adjacents
  "Для набора ячеек проверяется, есть ли хотя бы в одной из них непустой символ."
  [data adjacents]
  (some (partial check-coords data) adjacents))

(defn find-gears
  "Возвращает координаты ячеек из списка, в которых находятся звездочки."
  [data adjacents]
  (filter (partial check-gear data) adjacents))

(let [lines (str/split-lines data)]
  (->> ;; Получаем список чисел и их координат.
       (parse-data lines)
       ;; Выбираем только те числа, где в смежных ячейках есть непустые символы.
       (filter (fn [[_ coords]]
                 (check-adjacents lines (get-adjacents coords))))
       ;; Оставляем только список чисел и складываем их.
       (map first)
       (reduce +)))

(let [lines (str/split-lines data)]
  (->> ;; Получаем список чисел и их координат.
       (parse-data lines)
       ;; Для каждого числа получаем список соседних ячеек со звездочками.
       (map (fn [[num coords]]
              (vector num (find-gears lines (get-adjacents coords)))))
       ;; Оставляем только те, где звездочки есть.
       (filter (comp seq second))
       ;; Получаем набор пар, состоящих из координат звездочки и смежного числа.
       (mapcat (fn [[num gears]]
                 (mapv (fn [g]
                         [g num]) gears)))
       ;; Группируем пары по координатам звездочки.
       (group-by first)
       ;; Оставляем только те, где есть два смежных числа.
       (filter (comp (partial = 2) count second))
       ;; Перемножаем эти числа для каждой звездочки и потом складываем их.
       (map (fn [[_ nums]]
              (reduce * (map second nums))))
       (reduce +)))
