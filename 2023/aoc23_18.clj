(ns aoc23_18
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data
"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")

(comment
  (def data (slurp "aoc23_18.txt")))

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn parse-hex
  [s]
  (Integer/parseInt s 16))

(defn parse-data
  "Парсим данные в список четырехэлементных векторов. Первые два элемента
  используются во второй задаче, вторые два - во второй."
  [data]
  (->> (str/split-lines data)
       (map #(->> (re-matches #"([LRUD]) (\d+) [(]#([0-9a-f]{5})([0-3])[)]" %)
                  rest
                  ((juxt (comp keyword first)
                         (comp parse-int second)
                         (comp [:R :D :L :U] parse-int last)
                         (comp parse-hex (fn [m] (nth m 2)))))))))

(def directions
  "Смещения, соответствующие каждому направлению."
  {:R [0 1]
   :L [0 -1]
   :U [-1 0]
   :D [1 0]})

(defn get-path
  "На основе правил обхода получает набор точек, составляющих
  координаты периметра."
  [rules]
  (reduce (fn [tunnels [dir distance]]
            (let [direction (get directions dir)]
              (reduce (fn [tunnels _]
                        (conj tunnels (mapv + (first tunnels) direction)))
                      tunnels
                      (range distance))))
          '([0 0])
          rules))

(defn get-empty-points
  "Получаем все точки на карте, не занятые периметром. Также расширяем
  карту на 1 точку в каждую сторону, чтобы гарантироваться связность
  внешних областей."
  [trenches]
  (let [[[min-row max-row] [min-col max-col]]
        (map #(->> trenches (map %) sort ((juxt first last))) [first second])]
    (for [row (range (dec min-row) (+ 2 max-row))
          col (range (dec min-col) (+ 2 max-col))
          :when (not (contains? trenches [row col]))]
      [row col])))

(defn check-paths
  "Оставляет из списка `trenches` только те точки, которые не связаны
  с точкой `start`."
  [trenches start]
  (let [points (get-empty-points trenches)]
    (loop [points (set points) candidates #{start}]
      (if (empty? candidates)
        points
        (let [current (first candidates)
              reachable (->> (vals directions)
                             (mapv #(mapv + % current))
                             (filter (partial contains? points))
                             set)]
          (recur (disj points current)
                 (set/union (disj candidates current) reachable)))))))

(defn get-vertices
  "Получает список вершин многоугольника, образованного периметром."
  [rules]
  (reduce (fn [vertices [dir distance]]
            (let [direction (get directions dir)]
              (conj vertices (mapv + (first vertices) (mapv (partial * distance)
                                   direction)))))
          '([0 0])
          rules))

(defn square
  "Вычисляет площадь многоугольника, заданного вершинами по формуле Гаусса."
  [vertices]
  (let [rows (mapv first vertices)
        cols (mapv second vertices)]
    (* 1/2 (- (reduce + (mapv * rows (conj (subvec cols 1) (first cols))))
              (reduce + (mapv * cols (conj (subvec rows 1) (first rows))))))))

(defn perimeter
  "Вычисляет периметр."
  [vertices]
  (reduce + (mapv (fn [[r2 c2] [r1 c1]]
                    (abs (+ (- r2 r1) (- c2 c1))))
                  (rest vertices)
                  vertices)))

;; Первую часть решил через поиск несвязанных с внешней областью точек.
;; На самом деле лучше использовать метод из второй части, оставил
;; для истории.
(let [rules (map #(subvec % 0 2) (parse-data data))
      trenches (set (get-path rules))]
  (+ (count trenches) 
     (count (check-paths trenches [0 0]))))

;; Для второй части этот алгоритм не подходит, нужно считать площать по
;; формуле площади произвольного многоугольника.
(let [rules (map #(subvec % 2) (parse-data data))
      vertices (get-vertices rules)]
  ;; Здесь нужно учесть, что толщина периметра ненулевая, поэтому, к примеру,
  ;; правые и нижние ребра не вошли в площадь. Откуда единица - подробно не
  ;; разбирался, скорее всего, начало посчитал дважды.
  (+ (* 1/2 (perimeter vertices))
     (square vertices)
     1))
