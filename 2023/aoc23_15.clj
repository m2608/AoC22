(ns aoc23_15
  (:require [clojure.string :as str]))

(def data "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(comment
  (def data (slurp "aoc23_15.txt")))

(defn ascii-hash
  "Считает «хеш» для указанной строки:
  ```
  val ← ((int(ch) + val) × 17) mod 256
  ```"
  [string]
  (reduce (fn [value ch]
            (mod (* 17 (+ value (int ch))) 256))
          0 string))

(defn remove-lense
  "Убирает линзы с указанной меткой из ящика."
  [label box]
  (vec (remove #(= label (first %)) box)))

(defn add-lense
  "Заменяет или добавляет линзу с указанной меткой и фокусом в ящик."
  [label focus box]
  (let [idx (first (keep-indexed #(when (= label (first %2)) %1) box))
        lens (vector label (Integer/parseInt focus))]
    (if idx (update box idx (constantly lens))
      (conj box lens))))

(->> (-> data (str/replace #"\n" "") (str/split #","))
     (map ascii-hash)
     (reduce +))

(->> (-> data (str/replace #"\n" "") (str/split #","))
     (map #(rest (re-matches #"(\w+)([=-])(.*)" %)))
     (reduce (fn [boxes [label command focus]]
               (let [box-n (ascii-hash label)]
                 (case command
                   "-" (update boxes box-n (partial remove-lense label))
                   "=" (update boxes box-n (partial add-lense label focus)))))
             (vec (repeat 256 [])))
     (map-indexed (fn [box-idx box]
                    (map-indexed (fn [lens-idx lens]
                                   (* (inc box-idx) (inc lens-idx) (second lens)))
                                 box)))
     flatten
     (reduce +))

