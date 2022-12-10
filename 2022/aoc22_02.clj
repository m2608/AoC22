(ns aoc22_02
  (:require [clojure.string :as str]))

;; Рабочие данные.
(def data (slurp "aoc22_02.txt"))

;; Тестовые данные.
(def data "A Y\nB X\nC Z")

(defn value
  "Значение хода. Обработаны варианты и A-C, и X-Z."
  [ch]
  (or (str/index-of "#ABC" ch)
      (str/index-of "#XYZ" ch)))

(defn score
  "Вычисляем стоимость раунда на основе хода оппонента
  и своего хода."
  [opp-move my-move]
  (let [opp-value (value opp-move)
        my-value (value my-move)]
    (+ my-value
       (cond
         (= opp-value my-value) 3
         (= (mod (- my-value opp-value)
                 3)
            1) 6
         :else 0))))

(->> (str/split-lines data)
     (map #(->> (str/split % #" ")
                (apply score)))
     (reduce +))

(defn move
  "Формируем ход по значению. В т.ч. для удобства можем формировать
  для 0 и 4."
  [value]
  (str (nth "CABCA" value)))

(defn make-moves
  "В зависимости от хода оппонента и желаемого результата
  формируем свой ход."
  [opp-move result]
  (list opp-move
        (cond
          (= "X" result) (move (dec (value opp-move)))
          (= "Y" result) opp-move
          (= "Z" result) (move (inc (value opp-move))))))

(->> (str/split-lines data)
     (map #(->> (str/split % #" ")
                (apply make-moves)
                (apply score)))
     (reduce +))
