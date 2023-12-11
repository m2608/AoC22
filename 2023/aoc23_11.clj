(ns aoc23_11
  (:require [clojure.string :as str]))

(def data
"...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(comment
  (def data (slurp "aoc23_11.txt")))

(defn parse-galaxies
  "Парсит поле с галактиками, возвращает список их координат."
  [data]
  (->> data
       (str/split-lines)
       (map-indexed (fn [row line]
                      (mapv (fn [col]
                              [row col])
                            (keep-indexed #(when (= %2 \#) %1) line))))
       (mapcat identity)))

(defn expand-dimension
  "Расширяет пустые элементы в измерении, заданном вектором `div-vec` в
  `multiplier` раз. Например, для расширения строк нужно указать
  вектор `[1 0]` (первое измерение), для столбцов - `[0 1]`."
  [multiplier galaxies dim-vec]
  (let [;; Функция, получающая координату по нужному измерению из вектора.
        get-dimension (fn [g] (apply + (mapv * g dim-vec)))
        ;; Считаем границы заданного измерения.
        [min-val max-val] ((juxt (partial apply min)
                                 (partial apply max))
                           (map get-dimension galaxies))]
    (loop [v min-val shift 0 new-galaxies []]
      (if (> v max-val) new-galaxies
        (let [;; Получаем список галактик с текущей координатой.
              current-galaxies (filter (comp (partial = v)
                                             get-dimension)
                                       galaxies)]
          (recur (inc v)
                 ;; Если на текущей координате нет галактик, увеличиваем сдвиг координаты.
                 ;; Если есть - оставляем, как есть.
                 (if (empty? current-galaxies) (+ shift multiplier -1) shift)
                 ;; Добавляем текущее значение сдвига к координате для галактик в
                 ;; текущей строке.
                 (concat new-galaxies
                         (map (fn [g]
                                (mapv + g (mapv (partial * shift) dim-vec)))
                              current-galaxies))))))))


(defn expand-universe
  "Расширяет оба измерения в `multiplier` раз."
  [multiplier galaxies]
  (reduce (partial expand-dimension multiplier) galaxies [[1 0] [0 1]]))

(defn get-pairs
  "Для заданного списка получает все возможные уникальные пары элементов."
  [galaxies]
  (->> (for [g1 galaxies g2 galaxies
             :when (not= g1 g2)]
         (set [g1 g2]))
       (into #{})
       (map vec)))

(defn distance
  "Считает расстояние между двумя элементами по условиям задачи."
  [pair]
  (->> pair
       (apply mapv -)
       (map abs)
       (reduce +)))

(map #(->> data
           parse-galaxies
           (expand-universe %)
           get-pairs
           (map distance)
           (reduce +))
     [2 1000000])
