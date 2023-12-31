(ns aoc23_10
  (:require [clojure.string :as str]))

(def data1
"7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")

(def data2
"...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")

(def data3
"..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........")

(def data4
".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")

(def data5
"FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

(def data (slurp "aoc23_10.txt"))

(def box-drawing-map
  "Замена исходных символов карты для более красивого отображения
  карты."
  {\- \━
   \| \┃
   \F \┏
   \7 \┓
   \L \┗
   \J \┛})

(defn print-pipes
  "Печатает карту труб, заменяя символы на unicode-аналоги."
  [pipes]
  (print
    (str/join
      "\n"
      (map (comp str/join
                 (partial replace box-drawing-map))
           pipes))))

(def connections
  "Возможные пути для каждого типа трубы в формате
  [Δ-row Δ-col]"
  {\- [[ 0 -1] [ 0  1]]
   \| [[-1  0] [ 1  0]]
   \F [[ 1  0] [ 0  1]]
   \7 [[ 1  0] [ 0 -1]]
   \L [[-1  0] [ 0  1]]
   \J [[-1  0] [ 0 -1]]
   \S [[0 1] [1 0] [0 -1] [-1 0]]})

(defn find-animal
  "Ищет животное среди труб. Животное обозначается символом «S»."
  [pipes]
  (loop [pipes pipes row 0]
    (let [col (str/index-of (first pipes) \S)]
      (if (nil? col)
        (recur (rest pipes) (inc row))
        [row col]))))

(defn get-adjacent-positions
  "Получает смежные позиции в зависимости от текущего типа трубы."
  [pipes position]
  (->> position
       (get-in pipes)
       ;; Получаем дельты по текущему типу трубы. 
       connections
       ;; Считаем потенциальные позиции на основе возможных дельт.
       (map (partial mapv + position))
       ;; Позиция должна быть на поле.
       (filter (partial get-in pipes))))

(defn find-next-position [pipes path position]
  "Находит следующую позиции при продвижении по трубе."
  (->> (get-adjacent-positions pipes position)
       ;; Исключаем последнюю пройденную позицию.
       (filter (partial not= (last path)))
       ;; Исключаем смежных кандидатов, которые не соединены трубами
       ;; с текущей позицией.
       (filter (fn [candidate-position]
                 (seq (filter (partial = position)
                              (get-adjacent-positions pipes candidate-position)))))
       ;; Если несколько возможных путей (такое может быть для стартовой 
       ;; позиции), просто выбираем первый.
       first))

(defn get-full-path
  "Получает полный путь (список позиций)."
  [pipes]
  (let [start-position (find-animal pipes)]
    (loop [position start-position path []]
      (if (and (= position start-position)
               (seq path))
        path
        (recur (find-next-position pipes path position)
               (conj path position))))))

(defn zoom-path
  "Зумит путь в три раза. Это нужно для того, чтобы появилось пространство
  между смежными параллельными трубами."
  [path]
  (->> (partition 3 1 (concat path [(first path) (second path)]))
       (map (fn [[prev current next]]
              (let [zoomed-position (mapv (comp inc (partial * 3)) current)
                    Δ1 (mapv - prev current)
                    Δ2 (mapv - next current)]
                (list zoomed-position
                      (mapv + zoomed-position Δ1)
                      (mapv + zoomed-position Δ2)))))
       (apply concat)))


(defn min-max [path]
  "Получает минимальные и максимальную строку и столбец."
  (for [reduce-fn [min max]
        select-fn [first second]]
    (reduce reduce-fn (map #(select-fn %) path))))

(defn print-map
  "Печатает карту."
  [path]
  (let [[min-row min-col max-row max-col] (min-max path)
        rows (- max-row min-row -1)
        cols (- max-col min-col -1)]
    (->> (map (fn [position]
                (mapv - position [min-row min-col]))
              path)
         (reduce 
           (fn [pipes position]
             (assoc-in pipes position \#))
           (vec (repeat rows (vec (repeat cols \.)))))
         (mapv str/join)
         (str/join "\n")
         (print))))

(defn get-free-points
  "Получает незанятые путем точки в указанном участке."
  [[min-row min-col max-row max-col] path]
  (for [row (range (dec min-row) (+ 2 max-row))
        col (range (dec min-col) (+ 2 max-col))
        :when (not (path [row col]))]
    [row col]))

(defn get-adjacent
  "Получает координаты смежных ячеек."
  [points current]
  (->> [[0 1] [1 0] [0 -1] [-1 0]]
       (mapv #(mapv + current %))
       (filter #(points %))))

(defn exclude-external
  "Исключает из списка ячейки, которые доступны из указанной."
  [start-point points]
  (loop [points (set points)
         next-points #{start-point}]
    (if (empty? next-points) points
        (let [next-point (first next-points)]
          (recur (disj points next-point)
                 (reduce conj (disj next-points next-point)
                         (get-adjacent points next-point)))))))

(->> (str/split-lines data)
     (get-full-path)
     (count)
     (* 1/2))

(let [path (->> data
                str/split-lines
                get-full-path
                zoom-path
                set)
      [min-row min-col _ _ :as borders] (min-max path)]
  (->> path
       (get-free-points borders)
       (exclude-external [(dec min-row) (dec min-col)])
       (filter (fn [[row col]]
                 (and (= 1 (mod row 3))
                      (= 1 (mod col 3)))))
       count))
