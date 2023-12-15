(ns aoc23_14
  (:require [clojure.string :as str]))

(def data
"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(comment
  (def data (slurp "aoc23_14.txt")))

(defn transpose
  "Транспонирует список строк."
  [lines]
  (map str/join (apply mapv vector lines)))

(defn get-stone
  "Получает индексы указанного камня в строке."
  [ch line]
  (keep-indexed #(when (= %2 ch) %1) line))

(defn move
  "Двигает все шарообразные камни на север."
  [stones]
  (map (fn [[cubes spheres]]
         [cubes
          (reduce (fn [result sphere]
                    (->> ;; Препятствием может быть и куб, и шар.
                         (concat cubes result)
                         ;; Ищем последнее препятствие перед текущей сферой.
                         (filter (partial > sphere))
                         (apply max -1)
                         ;; Ставим сферу сразу за препятствием.
                         inc
                         (conj result)))
                  [] spheres)])
       stones))

(defn rotate-stones
  "Вращает поле с камнями одного типа по часовой стрелке."
  [stones]
  (let [size (count stones)]
    (map (fn [new-col]
           (keep-indexed (fn [old-col rows]
                           (when (some #{new-col} rows) old-col))
                         stones))
         (range (dec size) -1 -1))))

(defn rotate
  "Вращает поле со всеми камнями по часовой стрелке."
  [stones]
  (->> ;; Получаем отдельно кубы и сферы.
       (map #(map % stones)
            [first second])
       ;; Вращаем те и другие.
       (map rotate-stones)
       ;; Снова объединяем в общее поле.
       (apply mapv list)))

(defn cycle-platform
  "Сдвигает сферы на платформе последовательно по всем четырем направлениям."
  [stones]
  ((apply comp (repeat 4 (comp rotate move))) stones))

(defn get-load
  "Считает нагрузку платформы по условиям задачи."
  [stones]
  (->> stones  
       (map second)
       (apply concat)
       (group-by identity)
       (map (fn [[k v]]
              (* (count v) (- (count stones) k))))
       (reduce +)))

(defn wait-stable
  "Сдвигает камни на платформе, ждет повторения конфигурации."
  [stones]
  ;; В параметре цикла копим состояния полей платформы.
  (loop [iterations [stones]]
    (let [;; Получаем новую конфигурацию камней.
          new-stones (cycle-platform (peek iterations))
          ;; Проверяем, была ли уже эта конфигурация.
          repetition-index (first (keep-indexed #(when (= new-stones %2) %1)
                                                iterations))]
      ;; Продолжаем, если не нашли повторения.
      (if (nil? repetition-index)
        (recur (conj iterations new-stones))
        ;; Если нашли повторение, возаращаем номер итерации, когда начался цикл,
        ;; и значения нагрузки для всех пройденных итераций.
        [repetition-index (map get-load iterations)]))))

(defn get-stable-load
  [cycles-count [before-rep loads]]
  (let [;; Считаем количество весов до первого повтора.
        loads-count (count loads)
        ;; Считаем количество циклов в цикле повтора.
        rep-cycle-count (- loads-count before-rep)]
    (if (< cycles-count loads-count)
      ;; Если нам нужно получить уже посчитанное значение нагрузки,
      ;; просто возвращаем его.
      (nth loads cycles-count)
      ;; Если нужное значение ещё не вычисляли, определяем на каком
      ;; шаге цикла оно появляется.
      (nth (drop before-rep loads) 
           (mod (- cycles-count before-rep) rep-cycle-count)))))

(let [lines (str/split-lines data)]
  (->> lines
       transpose
       (map (juxt (partial get-stone \#)
                  (partial get-stone \O)))
       move
       get-load))


(let [lines (str/split-lines data)]
  (->> lines
       transpose
       (map (juxt (partial get-stone \#)
                  (partial get-stone \O)))
       wait-stable
       (get-stable-load 1000000000)))
