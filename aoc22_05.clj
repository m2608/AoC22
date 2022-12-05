(ns aoc22_05
  (:require [clojure.string :as str]))

(def data (slurp "aoc22_05.txt"))

(defn parse-int [s]
  (Integer/parseInt s))

(def data "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parse-crate
  "Парсит стопку контейнеров. Возвращает список элементов в указанной стопке."
  [stacks stack-number]
  (let [pos (inc (* 4 stack-number))]
    (->> stacks
         ;; Берем содержимое всех контейнеров из нужной стопки.
         (map (fn [stack] (subs stack pos (inc pos))))
         ;; Отфильтровываем пустые контейнеры.
         (filter #(not= % " ")))))

(defn parse-move
  "Парсит «ход», т.е. движение крана из переданной строки. Возвращает
  список из трех элементов: количество перемещаемых блоков, откуда они
  перемещаются (номер стопки) и куда."
  [move-line]
  (let [parsed (re-matches #"move\s+(\d+)\s+from\s+(\d+)\s+to\s+(\d+)" move-line)]
    (map parse-int (rest parsed))))

(defn make-move
  "Производит движение крана, перемещая указанное количество контейнеров
  из одной стопки в другую. Последний параметр указывает, может ли
  кран поднимать несколько контейнеров за раз."
  [stacks [quantity from to] & {:keys [multigrip]}]
  (let [;; В исходных данных номера стопок начинаются с 1, а у нас - с 0.
        from (dec from)
        to (dec to)
        ;; Забираем контейнеры из нужной стопки.
        moved-crates ((if multigrip identity reverse)
                      (take quantity (nth stacks from)))]
    (-> stacks
        (assoc from (drop quantity (nth stacks from)))
        (assoc to (concat moved-crates (nth stacks to))))))

(defn make-all-moves
  "Производит последовательно все движения крана. Последний параметр указывает,
  может ли кран поднимать несколько предметов за раз. Возвращает содержимое
  верхних контейнеров в виде строки."
  [stacks moves & {:keys [multigrip]}]
  (->> (reduce #(make-move %1 %2 :multigrip multigrip) stacks moves)
       (map first)
       (str/join "")))

(let [[stacks-and-places moves-lines] (map str/split-lines (str/split data #"\n\n"))
      ;; Считаем количество стопок.
      number-of-stacks (-> (last stacks-and-places)
                           (str/trim)
                           (str/split #"\s+")
                           (count))
      ;; Содержимое стопок. Здесь нужен вектор, а не список, т.к. мы
      ;; будем заменять стопки в дальнейшем при выполнении движений крана.
      stacks (mapv #(parse-crate (butlast stacks-and-places) %)
                  (range number-of-stacks))
      ;; Движения крана.
      moves (map parse-move moves-lines)]
  (println (make-all-moves stacks moves :multigrip false))
  (println (make-all-moves stacks moves :multigrip true)))
