(ns aoc23_04b
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data
"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(comment (def data (slurp "aoc23_04.txt")))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-nums
  "Преобразует строку с числами в список чисел."
  [nums-list]
  (->> (str/split nums-list #"\s+")
       (map parse-int)
       set))

(defn parse-line
  "Парсит строку, возвращает два набора чисел."
  [line]
  (->> line
       (re-matches #"Card\s*[0-9]+:\s*([0-9 ]+[0-9])\s*\|\s*([0-9 ]+[0-9])")
       rest
       (map parse-nums)))

(defn count-matches
  "Считает количество совпадений в двух наборах чисел."
  [parsed-nums]
  (->> (map set parsed-nums)
       (apply set/intersection)
       count))

(let [matches (->> data
                   str/split-lines
                   (map (comp count-matches parse-line)))]
  (->> (loop [;; здесь собираем уже обработанные карточки
              pref '()
              ;; это список карточек, подлежащих обработке, по умолчанию каждая из
              ;; них представлена в одном экземпляре
              suff (conj (repeat (count matches) 1))
              ;; количество совпадений для каждой карточки
              matches matches]
         (if (empty? suff) pref
           (let [n-of-matches (first matches)
                 n-of-copies (first suff)]
             (recur (conj pref n-of-copies)
                    ;; к нескольким следующим картам добавляем столько копий, сколько
                    ;; копий текущей карточки существует на данный момент
                    (concat (map (partial + n-of-copies) (take n-of-matches (rest suff)))
                            (drop (inc n-of-matches) suff))
                    (rest matches)))))
       (reduce +)))
