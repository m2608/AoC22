(ns aoc22_09
  (:require [clojure.string :as s]))

(def data (slurp "aoc22_09.txt"))

(def sample1 "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def sample2 "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(defn parse-int [s]
  (Integer/parseInt s))

(defn sign
  "Возвращает знак числа."
  [n]
  (if (= n 0) n
    (quot n (abs n))))

(def step
  "Вектор смещения в зависимости от направления, заданного ключевым словом."
  {:L [-1 0]
   :R [1 0]
   :U [0 1]
   :D [0 -1]})

(defn shift
  "Сдвигает «хвост» по направлению к «голове»."
  [head tail]
  (let [[dx dy] (mapv - head tail)]
    (if (and (<= (abs dx) 1)
             (<= (abs dy) 1)) tail
      (mapv + tail [(sign dx) (sign dy)]))))

(defn shift-rope
  "Сдвигает веревку по указанному вектору смещения. Узлы сдвигаются последовательно
  по заданному выше правилу. Возвращает новые координаты узлов, а также добавляет в
  `track` координаты последнего узла для отслеживания его пути."
  ;; Параметры `knots` и `track` упакованы в вектор, чтобы функция принимала и возвращала
  ;; данные в одном формате, т.е. чтобы ее можно было применять последовательно.
  [[knots-list track] step]
  ;; Первый узел просто смещаем по вектору.
  (loop [new-knots (list (mapv + (first knots-list) step))
         knots (rest knots-list)]
    (if (empty? knots) [(reverse new-knots) (conj track (first new-knots))]
      ;; Последующие узлы сдвигаем по заданному выше правилу.
      (recur (conj new-knots (shift (first new-knots) (first knots)))
             (rest knots) ))))

(defn move-rope
  "Сдвигает веревку на определенное смещение в указанном направлении."
  [[knots-list track] [direction distance]]
  ;; Здесь мы последовательно несколько раз применяем функцию `shift-rope`. Для того,
  ;; чтобы это работало, принимать и возвращать данные функция должна в одном формате.
  ;; Поэтому ее параметры `knots` и `track` и упакованы в вектор.
  ((apply comp (repeat distance #(shift-rope % (step direction))))
   [knots-list track]))

(defn make-moves
  "Выполняет перемещения веревки заданной длины. Возвращает конечное положение 
  узлов веревки и путь ее последнего узгла."
  [rope-length moves]
  (reduce move-rope [(repeat rope-length [0 0]) []] moves))

(defn get-track
  "Возвращает трек перемещений веревки заданной длины."
  [rope-length data]
  (->> data
       (s/split-lines)
       (map #(-> % (s/split #" ")
                 ((fn [[direction distance]]
                    [(keyword direction) (parse-int distance)]))))
       (make-moves rope-length)
       (second)))

(->> data (get-track 2) (set) (count))
(->> data (get-track 10) (set) (count))
