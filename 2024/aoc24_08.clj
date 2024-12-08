(require '[clojure.string :as str]
         '[clojure.set :as set])

(def data "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn parse-data
  [lines [w h]]
  (->> (for [r (range h)
             c (range w)
             :let [char (get-in lines [r c])]
             :when (not= char \.)]
         [char [r c]])
       (group-by first)))

(defn get-antinodes-pair
  "Возвращает пару «антинод» для пары антенн. Размер карты задается
  первым аргументом. Функция используется в первой части задачи."
  [[rows cols] p0 p1]
  (let [Δ (mapv - p1 p0)]
    (filter (fn [[r c]]
              (and (<= 0 r (dec rows)) (<= 0 c (dec cols))))
            [(mapv - p0 Δ) (mapv + p1 Δ)])))

(defn get-antinodes-side
  "Возвращает список «антинод» под одну сторону от точки `p` с шагом `Δ`.
  Для получения набора точек используется функция `side-fn` - это либо
  `+`, либо `-`. Вспомогательная функция для получения «антинод» во второй
  части."
  [side-fn [rows cols] p Δ]
  ;; Здесь важно учесть, что сами антенны также являются антинодами, поэтому
  ;; координаты антенны тоже будут добавляться в список.
  (loop [[r c :as p] p acc []]
    (if (and (<= 0 r (dec rows)) (<= 0 c (dec cols)))
      (recur (mapv side-fn p Δ) (conj acc p))
      acc)))

(defn get-antinodes-all
  "Получает список «антинод» для двух точек на карте указанного размера."
  [size p0 p1]
  (let [Δ (mapv - p1 p0)]
    (concat (get-antinodes-side + size p1 Δ)
            (get-antinodes-side - size p0 Δ))))

(defn find-antinodes
  "Находит все «антиноды» для заданного набора точек. Для получения антинод
  для пары точек будет использоваться функция `antinodes-fn`."
  [antinodes-fn size [_ places]]
  (->> (for [i (range (count places))
             j (range i)
             :let [[_ p0] (places i)
                   [_ p1] (places j)]
             a (antinodes-fn size p0 p1)]
         a)
       set))

(defn part1
  [size parsed-data]
  (mapv (partial find-antinodes get-antinodes-pair size) parsed-data))

(defn part2
  [size parsed-data]
  (mapv (partial find-antinodes get-antinodes-all size) parsed-data))

(let [lines (str/split-lines (slurp "aoc24_08.txt"))
      size [(count (first lines)) (count lines)]]
  (->> (parse-data lines size)
       ((juxt (partial part1 size)
              (partial part2 size)))
       (mapv (comp count (partial reduce set/union)))))
