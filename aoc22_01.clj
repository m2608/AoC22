(ns aoc22_01
  (:require [clojure.string :as str]))

(def data (slurp "aoc22_01.txt"))

(defn parse-int [s]
  (Integer/parseInt s))

(->> (str/split data #"\n\n")
     (map (fn [groups] (->> (str/split groups #"\n")
                            (map parse-int)
                            (reduce +))))
     (reduce max)) ; 74711

(->> (str/split data #"\n\n")
     (map (fn [groups] (->> (str/split groups #"\n")
                            (map parse-int)
                            (reduce +))))
     (sort)
     (reverse)
     (take 3)
     (reduce +)) ; 209481
