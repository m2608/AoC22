(require '[clojure.string :as str])

(def data "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")


(defn find-symbols
  "Находит все вхождения символа в строке, возвращает список индексов."
  [s c]
  (loop [i 0 acc []]
    (if-let [n (str/index-of s c i)]
      (recur (inc n) (conj acc n))
      acc)))

(defn find-symbols-on-map
  "Находит координаты всех указанных символов на карте."
  [lines c]
  (->> lines
       (map-indexed (fn [row line]
                      (mapv (partial vector row) (find-symbols line c))))
       (apply concat)
       set))

(defn find-obstacles
  "Находит координаты всех препятствий на карте, возвращает множество."
  [lines]
  (find-symbols-on-map lines \#))

(defn find-guard
  "Находит координаты охранника на карте и возвращает их."
  [lines]
  (first (find-symbols-on-map lines \^)))

(defn get-size
  "Определяет размеры карты."
  [lines]
  [(count lines) (count (first lines))])

(defn not-in-map
  "Проверяет, выходят ли координаты за пределы карты размера rows × cols."
  [[rows cols] [r c]]
  (or (< r 0) (< c 0) (>= r rows) (>= c cols)))

(defn rotate
  "Поворачивает вектор на движения вправо на ½π."
  [[r c]]
  (mapv (fn [[p q]] (+ (* r p) (* c q))) [[0 1] [-1 0]]))

(defn trace-path
  "Возвращает путь охранника до момента выхода за пределы карты или
  до возникновения цикла и флаг, указывающий на получение цикла."
  [guard obstacles size]
  ;; В качестве пути будем хранить пары [<позиция> <направление>]. 
  ;; Если мы получим пару, которая уже есть в пути, значит нашли цикл.
  (loop [guard guard Δ [-1 0] path #{}]
    (cond (not-in-map size guard) [(set (map first path)) false]
          (path [guard Δ])        [(set (map first path)) true]
          :else (let [new-guard (mapv + guard Δ)]
                  (if (obstacles new-guard)
                    (recur guard (rotate Δ) path)
                    (recur new-guard Δ (conj path [guard Δ])))))))

(defn find-loops
  "Возвращает позиции из списка, приводящии к образованию циклов в пути
  охранника среди препятствий."
  [guard obstacles size positions]
  (loop [positions positions loops []]
    (if (empty? positions) loops
      (let [[_ loop?] (trace-path guard (conj obstacles (first positions)) size)]
        (recur (rest positions)
               (if loop?
                 (conj loops (first positions))
                 loops))))))

(defn show-map
  "Показывает карту: препятствия и путь охранника."
  [[rows cols] obstacles path]
  (->> (range rows)
       (map (fn [row]
              (->> (range cols)
                   (map (fn [col]
                          (cond
                            (obstacles [row col]) \#
                            (path [row col])      \X
                            :else                 \.)))
                   (apply str))))
       (str/join "\n")
       (println)))

(let [[guard obstacles size] ((juxt find-guard find-obstacles get-size)
                              (str/split-lines (slurp "aoc24_06.txt")))
      [path _] (trace-path guard obstacles size)]
  [(count path)
   (count (find-loops guard obstacles size (vec (disj path guard))))])
