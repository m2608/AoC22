(ns aoc23_05
  (:require [clojure.string :as str]))

(def data
"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(comment (def data (slurp "aoc23_05.txt")))

(defn parse-int [s]
  (BigInteger. s))

(defn parse-ints
  "Парсит строку чисел, разделенных пробелами."
  [line]
  (map parse-int (str/split line #"\s+")))

(defn parse-seeds
  "Парсит информацию о зернах из строки."
  [lines]
  (->> lines
       first
       (re-matches #"seeds:\s+(.*)$")
       rest
       first
       parse-ints))

(defn parse-seed-ranges
  "Парсит информацию о зернах в виде диапазонов."
  [lines]
  (->> lines
       parse-seeds
       (partition 2)))

(defn parse-header
  "Парсит заголовок правил трансляции."
  [lines]
  (->> (first lines)
       (re-matches #"(\w+)-to-(\w+)\s+map:")
       rest
       (mapv keyword)))

(defn parse-ranges
  "Парсит диапазоны правил трансляции."
  [lines]
  (map parse-ints (rest lines)))

(defn parse-map
  "Парсит описание трансляции (заголовок и сами правила)."
  [lines]
  (->> lines
       str/split-lines
       ((juxt parse-header
              parse-ranges))
       ((fn [[[src dst] ranges]]
          [src [dst ranges]]))))

(defn parse-maps
  "Парсит все правила трансляции, возвращает словарь."
  [lines]
  (into {} (map parse-map (rest lines))))

(defn next-seed
  "Транслирует одно зерно по правилу (1 часть)."
  [seed [dst src _]]
  (+ dst (- seed src)))

(defn get-location
  "Определяет следующее положение семени исходя из имени правила трансляции."
  [maps k seed]
  (let [[next-k ranges] (k maps)
        next-seeds (filter (fn [[_ src len]]
                             (<= src seed (+ src len -1)))
                           ranges)]
    [next-k
     (if (seq next-seeds) 
       (next-seed seed (first next-seeds))
       seed)]))

(defn get-last-location
  "Определяет финальное положение семени."
  [maps k seed]
  (loop [[k seed] [k seed]]
    (if (= k :location)
      seed
      (recur (get-location maps k seed)))))

(defn range->interval
  "Преобразует диапазон (range, начало и длина) в интервал (начало и конец)."
  [[s len]]
  [s (+ s len)])

(defn interval->range
  "Преобразует интервал в диапазон (range)."
  [[a b]]
  [a (- b a)])

(defn translate-for-range
  "Принимает на вход диапазон семян и правило трансляции.
  Возвращает список из трех элементов: транслированный по правилу
  диапазон и два диапазона, не попавшие под правило трансляции
  (оказавшиеся слева и справа от него). Результирующие 
  диапазоны могут быть пустыми."
  [seed-range [dst src len]]
  ;; Для удобства преобразуем в интервалы от a до b.
  (let [[a b] (range->interval seed-range)
        [x y] (range->interval [src len])
        Δ (- dst src)]
    (if (or (< b x) (> a y))
      ;; Если интервалы не пересекаются, значит диапазон не был транслирован.
      [[] seed-range []]
      ;; В противном случае первым возвращаем транслированный диапазон. 
      (let [cent [(+ Δ (max a x)) (+ Δ (min b y))]
            left [(min a x) x]
            rght [y (max b y)]]
        (->> [cent left rght]
             (mapv interval->range))))))

(defn translate-for-ranges
  "Принимает на вход список диапазонов семян и список правил трансляции.
  Возвращает список новых диапазонов семян."
  [seed-ranges ranges]
  (->> ranges
       (reduce (fn [[trs srs] r]
                 (let [[more-trs new-srs]
                       ;; Отдельно собираем транслированные и нетранслированные диапазоны.
                       ((juxt #(->> % (map first) (filter seq))
                              #(->> % (map rest) (apply concat) (filter seq)))
                        (map #(translate-for-range % r) srs))]
                   ;; Транслированные добавляем к результату, нетранслированные передаем дальше.
                   [(concat trs more-trs)
                    new-srs]))
               ['() seed-ranges])
       (apply concat)
       ;; Убираем пустые диапазоны.
       (filter (comp not zero? second))))

(defn get-last-location-ranges
  "Определяет финальные положения для всех диапазонов семян."
  [maps k seed-ranges]
  (loop [[k seed-ranges] [k seed-ranges]]
    (if (= k :location)
      seed-ranges
      (let [[next-k ranges] (k maps)]
        (recur [next-k (translate-for-ranges seed-ranges ranges)])))))

(let [[seeds maps] (->> (str/split data #"\n\n")
                        ((juxt parse-seeds parse-maps)))]
  (->> seeds
       (map (partial get-last-location maps :seed))
       (apply min)))


(let [[seeds maps] (->> (str/split data #"\n\n")
                        ((juxt parse-seed-ranges parse-maps)))]
  (->> (get-last-location-ranges maps :seed seeds)
       (map first)
       (apply min)))



