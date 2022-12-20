(ns aoc22_20
  (:require [clojure.string :as s]))

(def data (s/trim-newline (slurp "aoc22_20.txt")))

(def sample "1
2
-3
3
-2
0
4")

(defn parse-int [s]
  (Integer/parseInt s))

(defn move-number [numbers n]
  "Двигает элемент в пределах списка. Элементы в списке представлены в виде
  мар вида {:idx <начальный индекс> :val <значение>}."
  (let [[head tail] (split-with (partial not= n) numbers)
        old-pos (count head)
        new-pos (mod (+ old-pos (:val n))
                     (dec (count numbers)))
        removed (concat head (rest tail))]
    (concat (take new-pos removed)
            (list n)
            (drop new-pos removed))))

(defn grove [positions values]
  "Считает сумму значений с указанными порядковыми номерами в циклическом списке,
  если считать с первого нулевого значения."
  (let [pos (count (take-while (comp not zero?) values))]
    (->> positions
         (map #(nth values (mod (+ pos %) (count values))))
         (reduce +))))

(->> data
     s/split-lines
     (map-indexed (fn [idx value]
                    {:idx idx :val (parse-int value)}))
     (repeat 2)
     (apply reduce move-number)
     (map :val)
     (grove [1000 2000 3000])
     println)

(let [dec-key 811589153
      numbers (->> data
                   s/split-lines
                   (map-indexed (fn [idx value]
                                  {:idx idx :val (* dec-key (parse-int value))})))]
  (->> numbers
       ((apply comp (repeat 10 (fn [mixed]
                                 (->> (reduce move-number mixed numbers))))))
       (map :val)
       (grove [1000 2000 3000])
       println))

