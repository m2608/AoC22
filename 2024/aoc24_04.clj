(require '[clojure.string :as str])

(def data "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defn rotate-½π
  "Поворачивает таблицу на π/2, возвращает список строк."
  [lines]
  (apply mapv str (reverse lines)))

(defn rotate-¼π
  "Поворачивает таблицу на π/4, возвращает список строк. Понятно, что при
  повороте на такой угол таблица уже не будет прямоугольной, в первой строке
  будет один символ, во второй - три и т.д."
  [lines]
  (let [w (count (first lines))
        h (count lines)]
    (mapv (fn [row]
            (apply str (for [col (range w)
                             :when (<= 0 col (dec h))]
                         (get (get lines (- row col)) col))))
          (range (+ h w -1)))))

(defn get-x
  "Из указанного списка строк вырезает квадратик 3x3, левый верхний угол
  задается переданными координатами."
  [row col lines]
  (mapv #(apply str (take 3 (drop col %)))
        (take 3 (drop row lines))))

(defn check-x
  "Проверяет, есть ли в квадратике 3x3 на обеих диагоналях строки «MAS»."
  [x]
  (->> 
    ;; С помощью начальной точки и дельт задаем все четыре диагонали.
    [[[0 0] [ 1  1]]
     [[2 2] [-1 -1]]
     [[0 2] [ 1 -1]]
     [[2 0] [-1  1]]]
    (mapv (fn [[start Δ]]
            (apply str (mapv (fn [i]
                               (get-in x (mapv + start (mapv (partial * i) Δ)) ))
                             (range 3)))))
    ;; Считаем, сколько слов "MAS" на диагоналях (может быть 0, 1 или 2.
    (filter (partial = "MAS"))
    count
    (= 2)))

(defn part1
  [lines]
  (let [;; Формируем список функций для получения всех нужных нам вращений (всего получится 8).
        rotations (into (mapv #(apply comp           (repeat % rotate-½π)) (range 0 4))
                        (mapv #(apply comp rotate-¼π (repeat % rotate-½π)) (range 0 4)))]
    (->> lines
         ;; Применяем все вращения.
         ((apply juxt rotations))
         ;; Ищем в получившихся строках строку "XMAS".
         (mapv (fn [wws]
                 (->> wws
                      (mapv (comp count (partial re-seq #"XMAS")))
                      (reduce +))))
         ;; И считаем, сколько получилось всего.
         (reduce +))))

(defn part2 [lines]
  (let [w (count (first lines))
        h (count lines)]
    ;; Обходим все квадратики поля и считаем те, где получаются "MAX" на диагоналях.
    (count
      (for [row (range (- h 2))
            col (range (- w 2))
            :when (check-x (get-x row col lines))]
        1))))

((juxt part1 part2) (str/split-lines (slurp "aoc24_04.txt")))
