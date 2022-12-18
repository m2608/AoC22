(ns aoc22_16
  (:require [clojure.string :as s]))

(def data (slurp "aoc22_16.txt"))

(def sample "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse
  "Парсит информацию о вентилях."
  [data]
  (->> data
       (s/split-lines)
       (map #(rest (re-matches #"Valve ([A-Z]+) has flow rate=(\d+); tunnel[s]? lead[s]? to valve[s]? (.*)" %)))
       (reduce (fn [graph [valve flow tunnels]]
                 (assoc graph
                        (keyword valve) {:flow (parse-int flow)
                                         :score ##Inf
                                         :visited false
                                         :tunnels (mapv keyword (s/split tunnels #",\s+"))}))
               {})))

(defn distances
  "Считает минимальные расстояния от указанного вентиля до всех остальных.
  Возвращает мапу."
  [valves start]
  (loop [valves (assoc-in valves [start :score] 0)]
    (if (every? :visited (vals valves)) (into {} (mapv (fn [[k v]] [k (:score v)]) valves))
      (let [current (->> valves (filter #(-> % val :visited not)) (sort-by #(-> % second :score)) first key)
            new-score (-> valves current :score inc)
            neighbs (->> valves current :tunnels)]
        (recur (reduce (fn [valves neigh]
                         (if (<= (-> valves neigh :score) new-score) valves
                           (assoc-in valves [neigh :score] new-score)))
                       (assoc-in valves [current :visited] true) neighbs))))))

(defn get-next-time-limit
  "Определяет, сколько останется времени после перехода от одного вентиля к другому."
  [valves time-limit start finish]
  (- time-limit 1
     (get-in valves [start :distances finish])))

(defn filter-next-valves
  "Убирает вентили, до которых все равно не добраться."
  [valves time-limit start finishes]
  (let [next-valves (filter #(>= (get-next-time-limit valves time-limit start %) 0) finishes)]
    (if (empty? next-valves) #{nil}
      next-valves)))

(defn search
  "Ищет путь с наибольшей оценкой."
  [valves next-valves path score time-limit]
  (let [paths-and-scores
        (for [next-valve next-valves
              ;; Для каждой точки определяем, сколько после перехода в нее останется времени
              ;; и каким будет новый скор
              :let [next-time-limit (get-next-time-limit valves time-limit (peek path) next-valve)
                    next-score (+ score (* next-time-limit (get-in valves [next-valve :flow])))]
              ;; Обходим вентили, до которых можем добраться за оставшееся время.
              :when (>= next-time-limit 0)]
          ;; Для каждой новой точки формируем последующие пути.
          (search valves
                  ;; Второй раз этот вентиль открывать не нужно.
                  (disj next-valves next-valve)
                  ;; Сохраняем путь ради интереса.
                  (conj path next-valve)
                  next-score
                  next-time-limit))]
    ;; Если никуда не ходили, возвращаем текущий результат. А если ходили,
    ;; возвращаем лучший путь из найденных.
    (if (empty? paths-and-scores) {:score score :path path}
      (apply max-key :score paths-and-scores))))

(defn search2
  "Ищет путь с наибольшей оценкой для двух существ."
  ;; TODO Не учитывает вариант, когда у двух существ осталась только одна общая точка.
  ;; TODO Привести в порядок код, избавиться от повторов. Возможно, нужен общий вид для
  ;; обоих вариантов задачи.
  [valves next-valves path1 path2 score limit1 limit2]
  (let [next-valves1 (filter-next-valves valves limit1 (peek path1) next-valves)
        next-valves2 (filter-next-valves valves limit2 (peek path2) next-valves)
        next-pairs  (for [nv1 next-valves1
                          nv2 next-valves2
                          :when (not= nv1 nv2)]
                      [nv1 nv2])
        paths-and-scores (for [[nv1 nv2] next-pairs
                               :let [[nl1 nf1] (if (nil? nv1) [limit1 nil]
                                                 [(get-next-time-limit valves limit1 (peek path1) nv1) (get-in valves [nv1 :flow])])
                                     [nl2 nf2] (if (nil? nv2) [limit2 nil]
                                                 [(get-next-time-limit valves limit2 (peek path2) nv2) (get-in valves [nv2 :flow])])]]
                           (search2 valves
                                    (disj next-valves nv1 nv2)
                                    (if (nil? nv1) path1 (conj path1 nv1))
                                    (if (nil? nv2) path2 (conj path2 nv2))
                                    (+ score
                                       (if (nil? nv1) 0 (* nl1 nf1))
                                       (if (nil? nv2) 0 (* nl2 nf2)))
                                    nl1
                                    nl2))]
    (if (empty? paths-and-scores) {:score score :path1 path1 :path2 path2}
      (apply max-key :score paths-and-scores))))

(defn make-dot
  "Вспомогательная функция, формирует представление графа в формате dot."
  [valves]
  (println "graph SAMPLE {")
  (println "node [shape=plaintext]")
  (doseq [[k v] valves]
    (println (str (name k) " [label=\"" (name k) " <" (:flow v) ">\"];"))
    (println (str (name k) " -- {" (s/join ", " (map name (:tunnels v))) "};")))
  (println "}"))

;; Вычисляем лучший результат для одного существа за 30 минут.
(let [time-limit 30
      valves (parse data)
      ;; Вентили с нулевым потоком нам неинтересны.
      next-valves (set (keys (filter #(-> % val :flow zero? not) valves)))
      ;; Считаем расстояния от каждого вентиля до каждого.
      distas (into {} (map #(vector % (distances valves %)) (keys valves)))
      valves (reduce (fn [valves [valve dists]]
                       (assoc-in valves [valve :distances] dists))
                     valves distas)]
  (println (:score (search valves next-valves [:AA] 0 time-limit))))

;; Вычисляем лучший результат для двух существ за 26 минут.
;; Считает сильно долго.
(let [time-limit 26
      valves (parse data)
      ;; Вентили с нулевым потоком нам неинтересны.
      next-valves (set (keys (filter #(-> % val :flow zero? not) valves)))
      ;; Считаем расстояния от каждого вентиля до каждого.
      distas (into {} (map #(vector % (distances valves %)) (keys valves)))
      valves (reduce (fn [valves [valve dists]]
                       (assoc-in valves [valve :distances] dists))
                     valves distas)]
  (println (:score (search2 valves next-valves [:AA] [:AA] 0 time-limit time-limit))))
