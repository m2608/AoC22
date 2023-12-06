(ns aoc23_06
  (:require [clojure.string :as str]
            [clojure.math :as m]))

(def data
"Time:      7  15   30
Distance:  9  40  200")

(comment (def data (slurp "aoc23_06.txt")))

(defn parse-int [s]
  (BigInteger. s))

(defn roots
  "Считает минимальный и максимальный целые корни неравенства $$ x^2 - xt + S < 0 $$"
  [[t s]]
  (map (fn [[round-fn add-fn]] (round-fn (* 0.5 (add-fn t (m/sqrt (- (* t t) (* 4 s)))))))
       [[(comp inc int m/floor) -] [(comp dec int m/ceil) +]]))

(defn count-whole-solutions
  [[x1 x2]]
  (- x2 x1 -1))

(->> (str/split-lines data)
     (map (fn [line]
            (map parse-int (-> line (str/split #":\s*") second (str/split #"\s+")))))
     (apply mapv vector)
     (map (comp count-whole-solutions roots))
     (reduce *))

(->> (str/split-lines data)
     (map (fn [line]
            (-> line (str/split #":\s*") second (str/split #"\s+") str/join parse-int)))
     roots
     count-whole-solutions)
