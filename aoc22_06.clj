(ns aoc22_06
  (:require [clojure.string :as str]))

(def data (slurp "aoc22_06.txt"))

(def data-samples
  ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"
   "bvwbjplbgvbhsrlpgdmjqwftvncz"
   "nppdvjthqldpwncqszvftbrmjlhg"
   "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
   "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

(defn find-marker
  "Ищет в строке первую последовательность из `len` различных символов
  (маркер) и возвращает порядковый номер последнего символа маркера
  (длину подстроки от начала исходной строки до конца маркера)."
  [s len]
  (loop [s s n 0]
    (if (= len (count (set (take len s))))
      (+ n len)
      (recur (rest s) (inc n)))))

(for [len [4 14]]
  (println
    "Test data samples with length" (str len ":")
    (str/join " " (map #(find-marker % len)
                       data-samples))
    "\nActual data:"
    (find-marker data len)))
