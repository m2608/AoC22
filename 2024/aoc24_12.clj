(require '[clojure.string :as str]
         '[clojure.set :as set])

(def data1 "AAAA
BBCD
BBCC
EEEC")

(def data2 "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(def data3 "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA")

(def directions [[0 1] [1 0] [0 -1] [-1 0]])

(defn get-points
  "Получает точки поля в виде словаря."
  [field]
  (let [[rows cols] [(count field) (count (first field))]]
    (into {} (for [r (range rows) c (range cols) :let [v (get-in field [r c])]]
               [[r c] v]))))

(defn get-field-neighbours
  "Получает всех соседей того же типа для указанной точки поля."
  [field p]
  (let [v (field p)]
    (->> (mapv (partial mapv + p) directions)
         (filter (comp (partial = v) field))
         set)))

(defn get-region-neighbours
  "Получает всех соседей для указанной точки региона."
  [region p]
  (->> (mapv (partial mapv + p) directions)
       (filter region)))

(defn get-region
  "Получает регион, связанный с указанной точкой."
  [field point]
  (loop [points #{point} region #{}]
    (if (empty? points) region
      (let [current-point (first points)
            neighbours (get-field-neighbours field current-point)]
        (recur (-> points (set/union neighbours) (set/difference region) (disj current-point))
               (conj region current-point))))))

(defn get-regions
  "Получает все регионы поля."
  [field]
  (loop [points (into #{} (keys field)) regions []]
    (if (empty? points) regions
      (let [region (get-region field (first points))]
        (recur (set/difference points region)
               (conj regions region))))))

(defn get-perimeter
  "Получает периметр региона."
  [region]
  (->> region
       (mapv (fn [p]
               (- 4 (count (get-region-neighbours region p)))))
       (apply +)))

(defn count-corners
  "Считает количество углов в регионе."
  [region]
  (let [;; Углы определяеются парами возможных направлений.
        corners (partition 2 1 (conj directions (first directions)))]
    (->> region
         (mapv (fn [p]
                 (count
                   (filter (fn [[c c']]
                             (let [;; Координаты точки по диагонали от угла.
                                   pc (mapv + p c c')
                                   ;; Координаты смежных точек.
                                   pl (mapv + p c)
                                   pr (mapv + p c')]
                               (or ;; Внутренний угол.
                                   (and (region pl) (region pr) (not (region pc)))
                                   ;; Внешний угол.
                                   (and (not (region pl)) (not (region pr))))))
                           corners))))
         (reduce +))))

(defn get-price1
  "Получает цену ограды для указанного региона (первая часть задачи)."
  [region]
  (* (get-perimeter region)
     (count region)))

(defn get-price2
  "Получает цену ограды для указанного региона (вторая часть задачи)."
  [region]
  (* (count-corners region)
     (count region)))

(let [field (get-points (str/split-lines (slurp "aoc24_12.txt")))]
  (mapv (fn [f] (reduce + (mapv f (get-regions field)))) [get-price1 get-price2]))

