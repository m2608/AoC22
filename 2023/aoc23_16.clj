(ns aoc23_16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def pre-data
  "Здесь для удобства отображения слэши заменены на unicode аналоги."
  [".|...⧵...."
   "|.-.⧵....."
   ".....|-..."
   "........|."
   ".........."
   ".........⧵"
   "....∕.⧵⧵.."
   ".-.-∕..|.."
   ".|....-|.⧵"
   "..∕∕.|...."])

(def data
  "Заменяем их обратно."
  (reduce #(str/replace %1 (first %2) (second %2))
          (str/join "\n" pre-data)
          [[\u29f5 \u005c] [\u2215 \u002f]]))

(comment
  (def data (slurp "aoc23_16.txt")))

(defn move-forward
  "Двигает пучок света вперед. Пучок задается текущей позицией и направлением."
  [[position direction]]
  [(mapv + position direction) direction])

(defn move-beam
  "Двигает пучок света на один шаг. Возвращает список (один или два) пучка
  света."
  [cave [position [Δrow Δcol] :as beam]]
  (let [mirror (get-in cave position)]
    (case mirror
      \. (vector (move-forward beam))
      \/ (vector (move-forward [position (mapv - [Δcol Δrow])]))
      \\ (vector (move-forward [position [Δcol Δrow]]))
      \- (if (zero? Δrow) [(move-forward beam)]
           (mapv #(move-forward [position %]) [[0 -1] [0 1]]))
      \| (if (zero? Δcol) [(move-forward beam)]
           (mapv #(move-forward [position %]) [[-1 0] [1 0]])))))

(defn print-energized
  "Вспомогательная функция для печати пещеры с местами, где прошел свет."
  [cave energized]
  (->> (reduce (fn [cave position]
                 (update-in cave position (constantly \#)))
               cave
               energized)
       (map str/join)
       (map println)))

(defn send-beam
  "Посылает свет в пещеру их указанной точки в указанном направлении.
  Возвращает список координат полей, где прошел свет."
  [cave beam]
  (loop [beams [beam] past-beams #{beam}]
    (if (empty? beams) (set (map first past-beams))
      (let [new-beams (->> ;; Двигаем все пучки света на один шаг.
                           (map (partial move-beam cave) beams)
                           ;; Уплощаем список с новыми положениями пучков света.
                           (apply concat)
                           ;; Убираем те, что вышли за пределы пещеры.
                           (filter (fn [[position _]] (get-in cave position)))
                           ;; Убираем те, которые уже были на прошлых итерациях
                           ;; (с тем же направлением и в том же положении).
                           (filter (comp not (partial contains? past-beams))))]
        (recur new-beams
               (set/union past-beams (set new-beams)))))))

(let [cave (mapv vec (str/split-lines data))]
  (count (send-beam cave [[0 0] [0 1]])))

(let [cave (mapv vec (str/split-lines data))
      size (count cave)
      ;; Определяем все возможные стартовые позиции и направления
      ;; пучков света (края пещеры).
      start-beams (->> (range 0 size)
                       (map #(vector [[% 0] [0 1]]
                                     [[0 %] [1 0]]
                                     [[% (dec size)] [0 -1]]
                                     [[(dec size) %] [-1 0]]))
                       (apply concat))]
  (->> (map (comp count (partial send-beam cave)) start-beams)
       (reduce max)))
