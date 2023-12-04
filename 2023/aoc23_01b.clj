(ns aoc23_01
  (:require [clojure.string :as str]))

(def data
  (str/join
    "\n"
    ["two1nine"
     "eightwothree"
     "abcone2threexyz"
     "xtwone3four"
     "4nineeightseven2"
     "zoneight234"
     "7pqrstsixteen"]))

(comment
  (def data (slurp "aoc23_01.txt")))

(def digits-words
  ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(def digits-numbers
  (mapv str (range 10)))

(defn parse-int [s]
  (Integer/parseInt s))

(defn find-numbers
  "Находит позиции цифр в строке, представленных набором `digits`. Для преобразования
  цифры используется `coerce-fn`."
  [line digits coerce-fn]
  (->> digits
       (mapcat #(vector [(coerce-fn %) (str/index-of line %)]
                        [(coerce-fn %) (str/last-index-of line %)]))
       (filter (comp not nil? second))))

(defn find-first-and-last-digits
  "Находит первую и последнюю цифру в строке и объединяет их в число."
  [line]
  (let [nums (find-numbers line digits-numbers parse-int)
        wrds (find-numbers line digits-words (fn [word]
                                               (inc (.indexOf digits-words word))))]
    (->> (concat nums wrds)
         ((juxt #(apply min-key second %)
                #(apply max-key second %)))
         (map (comp str first))
         str/join
         parse-int)))

(->> data
     str/split-lines
     (map find-first-and-last-digits)
     (reduce +))
