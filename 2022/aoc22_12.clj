(ns aoc22_12
  (:require [clojure.string :as s]))

(def data (slurp "aoc22_12.txt"))

(def sample "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn parse-map
  "Парсит карту, возвращает ширину, начальную и конечную точки, 
  описание каждой точки карты."
  [data]
  (let [terrain (s/replace data #"\n" "")
        width (count (first (s/split-lines data)))
        [beg end] (mapv #(s/index-of terrain %) "SE")]
    (->> terrain
         ;; Определяем высоту, в т.ч. для начальной и конечной точек.
         (map #(- (int (case % \S \a \E \z %))
                  (int \a)))
         ;; Заполняем начальную информацию о точке на карте.
         (map-indexed #(assoc {} :number %1 :height %2 :visited false :score (count terrain)))
         (vec)
         (vector width beg end))))

(defn get-neighbours
  "Получает доступных для данной точки карты соседей."
  [terrain point width]
  ;; Проверяем границы и добавляем соседей в список
  (->> (cond-> [] 
         (> (rem point width) 0)             (conj (dec point))
         (> (rem (inc point) width) 0)       (conj (inc point))
         (>= point width)                    (conj (- point width))
         (< point (- (count terrain) width)) (conj (+ point width)))
       ;; Проверяем, что они не отмечены, как посещенные.
       (filter #(not (:visited (terrain %))))
       ;; Решать будем в обратном порядке, так что проверяем доступность
       ;; соседних пунктов с точки зрения условия задачи.
       (filter #(<= (:height (terrain point)) (inc (:height (terrain %)))))))

(defn update-neighbour
  "Обновляет информацию о соседях на карте (проставляет скоры)."
  [terrain point neigh]
  (let [point-score (:score (terrain point))
        neigh-score (:score (terrain neigh))]
    (if (< neigh-score (inc point-score)) terrain
      (assoc-in terrain [neigh :score] (inc point-score)))))

(defn visit-point
  "Обновляет данные карты при посещении точки."
  [terrain point width]
  (let [neighbours (get-neighbours terrain point width)]
    ;; Обходим всех соседей и обновляем для них скор.
    (-> (reduce #(update-neighbour %1 point %2)
                terrain neighbours)
        ;; Отмечаем текущую точку как посещенную.
        (assoc-in [point :visited] true))))

(defn find-next-point
  "Ищет непосещенную точку с минимальным скором."
  [terrain]
  (->> terrain
       (filter #(not (:visited %)))
       (sort-by :score)
       (first)
       :number))

(defn find-zero-points
  "Ищем точки с нулевой высотой."
  [terrain]
  (->> terrain
       (filter #(= 0 (:height %)))
       (map :number)))

(defn visit-all
  "Обходит карты и проставляет скоры для всех точек."
  [terrain width]
  (loop [terrain terrain]
    (let [point (find-next-point terrain)]
      (if (nil? point) terrain
        (recur (visit-point terrain point width))))))

(let [[width beg end terrain] (parse-map data)
      terrain (-> terrain (assoc-in [end :score] 0) (visit-all width))]
  (println (:score (nth terrain beg))))


(let [[width _ end terrain] (parse-map data)
      terrain (-> terrain (assoc-in [end :score] 0) (visit-all width))]
  (->> (find-zero-points terrain)
       ;; Определяем скор для всех путей до нулевых высот.
       (map #(get-in terrain [% :score]))
       (reduce min)
       (println)))
