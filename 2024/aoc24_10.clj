(require
  '[clojure.string :as str])

(def data "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defn parse-map
  [data]
  (->> (str/split-lines data)
       (mapv (partial mapv (comp Integer/parseInt str)))))

(defn get-points
  "Возвращает список координат точек указанной высоты."
  [terrain height]
  (for [r (range (count terrain))
        c (range (count (first terrain)))
        :when (= height (get-in terrain [r c]))]
    [r c]))

(def directions [[0 1] [1 0] [0 -1] [-1 0]])

(defn get-reachable
  "Возвращает список доступных точек для указанной."
  [terrain p]
  (let [v (get-in terrain p)]
    (->> directions
         (map (fn [d]
                (let [p' (mapv + p d)]
                  [p' (get-in terrain p')])))
         (filter (fn [[_ v']]
                   (and v' (= (inc v) v'))))
         (mapv first))))
    

(defn get-reachable-tops
  "Возвращает список доступных вершин для указанной."
  [terrain start]
  (reduce (fn [ps _]
            (->> ps
                 (mapv (partial get-reachable terrain))
                 (reduce into)
                 set))
          #{start}
          (range 9)))

(defn get-all-paths
  "Возвращает список путей ко всем доступным вершинам для указанной."
  [terrain start]
  (reduce (fn [paths _]
            (->> paths
                 (mapv (fn [path]
                         (mapv (partial conj path)
                               (get-reachable terrain (peek path)))))
                 (reduce into)
                 set))
          #{[start]}
          (range 9)))

(defn score
  "Возвращает оценку для списка начальных точек карты. Оценка считается
  на основе функции `score-fn`."
  [terrain points score-fn]
  (->> points
       (mapv (comp count (partial score-fn terrain)))
       (reduce +)))

(let [terrain (parse-map (slurp "aoc24_10.txt"))
      points (get-points terrain 0)]
  (mapv (partial score terrain points) [get-reachable-tops get-all-paths]))


