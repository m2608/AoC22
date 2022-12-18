(ns aoc22_18
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data (slurp "aoc22_18.txt"))

(defn parse-int [s]
  (Integer/parseInt s))

(def sample "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(defn parse-cubes
  "Парсит входные данные."
  [data]
  (->> data
       str/split-lines
       (map #(->> (str/split % #",")
                  (mapv parse-int)))
       set))

(defn get-cubes
  "Получает список соседних ячеек, которые соотносятся с множеством ячеек
  `cubes` в соответствии с переданной функцией (т.е. может считаться, например,
  пересечение множеств или их разница)."
  [cubes cube check-fn]
  (let [deltas [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]]
    (check-fn (set (map #(mapv + cube %) deltas))
                    cubes)))

(defn get-free-sides
  "Получает список соседних ячеек, которые отсутствуют в списке `cubes`."
  [cubes cube]
  (get-cubes cubes cube set/difference))

(defn get-connected-sides
  "Получает список соседних кубов, которые присутствую в списке `cubes`."
  [cubes cube]
  (get-cubes cubes cube set/intersection))

(defn surface-area 
  "Проверяет количество поверхностей кубов из множества `check`, которые
  определенным образом соотносятся с поверхностями кубов из множества `cubes`.
  Характер этого отношения определяется переданной функцией (т.е. можно получить
  количество сторон, соединенных с соседями или, наоборот, свободных, в 
  зависимости от переданной функции)."
  ([cubes check check-fn]
   (reduce + (map (comp count
                        (partial check-fn cubes))
                  check))))

(defn get-free-cubes-data
  "Определяет незанятые кубами ячейки в указанных границах."
  [cubes [min-x min-y min-z max-x max-y max-z]]
  (for [x (range (- min-x 1) (+ max-x 2))
        y (range (- min-y 1) (+ max-y 2))
        z (range (- min-z 1) (+ max-z 2))
        :when (not (contains? cubes [x y z]))]
    [x y z]))

(defn check-paths
  "Считает пути от одной ячейки до всех остальных. Таким образом мы выбираем из всех
  незанятых ячеек те, которые не связаны с внешним пространством (до них дойти не удастся)."
  [free-cubes external-cube]
  ;; Сначала преобразуем список незанятых ячеек в мапу, где в качестве ключа используются
  ;; координаты ячейки, а в качестве значения - скор и флаг посещения. Также меняем скор
  ;; начальной, заведомо внешней ячейки, на 0.
  (loop [cubes (assoc-in (into {} (mapv #(vector % {:score ##Inf :visited false}) free-cubes))
                         [external-cube :score] 0)]
    (if (every? :visited (vals cubes)) (into {} (mapv (fn [[k v]] [k (:score v)]) cubes))
      (let [current (->> cubes (filter #(-> % val :visited not)) (sort-by #(-> % second :score)) first key)
            new-score (inc (get-in cubes [current :score]))
            neighbs (get-connected-sides cubes current)]
        (recur (reduce (fn [cubes neigh]
                         (if (<= (get-in cubes [neigh :score]) new-score) cubes
                           (assoc-in cubes [neigh :score] new-score)))
                       (assoc-in cubes [current :visited] true) neighbs))))))

;; Определяем внешнюю поверхность кубов.
(let [cubes (parse-cubes data)]
  (-> (surface-area cubes cubes get-free-sides)
      println))

(let [cubes (parse-cubes data)
      ;; Оперделяем границы области, занятой кубами.
      min-max 
      (for [reduce-fn [min max]
            select-fn [first second last]]
        (reduce reduce-fn (map #(select-fn %) cubes)))
      ;; Данные о свободных кубах в пределах занятой кубами области ±1 куб.
      free-cubes-data (get-free-cubes-data cubes min-max)
      ;; Определяем, какие незанятые ячейки заперты внутри массива кубов,
      ;; пытаясь построить путь до них от заведомо внешней ячейки. В качестве таковой
      ;; берем ячейку с минимальными координатами.
      trapped-cubes
      (->> (check-paths free-cubes-data (mapv dec (take 3 min-max)))
           (filter #(-> % val (= ##Inf)))
           (map key))]
  ;; Определяем, сколько поверхностей запертых ячеек граничат с поверхностями кубов
  ;; и вычитаем это число из общего числа свободных поверхностей кубов.
  (-> (surface-area cubes cubes get-free-sides)
      (- (surface-area cubes trapped-cubes get-connected-sides))
      println))

