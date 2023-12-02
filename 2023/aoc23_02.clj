(ns aoc23_02
  (:require [clojure.string :as str]))

(def data
  (str/join
    "\n"
    ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
     "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
     "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
     "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
     "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]))

(comment
  (def data (slurp "aoc23_02.txt")))

(def set-limit
  "Пределы количества кубиков для оценки валидности игры."
  {:red 12
   :green 13
   :blue 14})

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-pair
  "Парсит часть хода с кубиком определенного цвета."
  [line]
  ((juxt (comp keyword second)
         (comp parse-int first))
   (str/split line #"[ ]+")))

(defn parse-set
  "Парсит один набор ходов, возвращает мапу."
  [line]
  (into {} (map parse-pair (str/split line #",[ ]*"))))

(defn parse-sets
  "Парсит наборы ходов, возвращает список мап."
  [line]
  (map parse-set (str/split line #";[ ]*")))

(defn parse-game
  "Парсит строку с игрой, возвращает пару из номер игры и набора ходов."
  [line]
  (->> (re-matches #"Game (\d+): (.*)" line)
       rest
       ((juxt (comp parse-int first)
              (comp parse-sets second)))))

(defn parse-input
  "Парсит входные данные."
  [data]
  (map parse-game (str/split-lines data)))

(defn check-set
  "Проверяет валидность набора."
  [move-set]
  (every? #(<= (get move-set % 0) (% set-limit)) 
          (keys set-limit)))

(defn check-game
  "Проверяет валидность игры."
  [game]
  (every? check-set (second game)))

(defn get-color-limit
  "Определяет минимальный предел валидности игры."
  [move-sets color]
  (->> (map #(get % color 0) move-sets)
       (reduce max)))

(defn get-game-power
  "Считает мощность игры в соответствии с условиями задачи."
  [game]
  (->> (map (partial get-color-limit (second game))
            (keys set-limit))
       (reduce *)))

(->> (parse-input data)
     (filter check-game)
     (map first)
     (reduce +))

(->> (parse-input data)
     (map get-game-power)
     (reduce +))

