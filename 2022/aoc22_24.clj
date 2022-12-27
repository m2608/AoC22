(ns aoc22_24
  (:require [clojure.string :as str]))

(def data (slurp "aoc22_24.txt"))

(def sample "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#")

(def directions
  "Вектора движения, соответствующие различным направлениям."
  {\> [ 1  0]
   \< [-1  0]
   \^ [ 0 -1]
   \v [ 0  1]})

(defn parse-map
  "Функция парсит предоставленную карту и возвращает мапу, содержащую:
  - `size`: размеры карты,
  - `start`: координаты начальной точки,
  - `finish`: координаты конечной точки,
  - `blizzs`: список мап, содержащих начальную позицию и направление каждой метели.
  "
  [data']
  (let [data (str/split-lines data')
        size-x (count (first data))
        size-y (count data)
        ;; Для первой и последней строки находим точку, обозначенную точкой. Это
        ;; вход и выход из лабиринта соответственно.
        [start-x finish-x] (map (fn [sel-fn]
                              (count (take-while #(not= % \.) (sel-fn data))))
                            [first last])]
    {:size [size-x size-y] :start [start-x 0] :finish [finish-x (dec size-y)]
     ;; Проходим по всем точкам карты и, если обнаруживаем метель, добавляем ее
     ;; координаты и направление в мапу.
     :blizzs (for [x (range (count (first data)))
                   y (range (count data))
                   :let [value (get-in data [y x])]
                   :when (directions value)]
               {:pos [x y] :dir value})}))

(defn move-blizzs
  "Перемещает метели в соответствии с их направлениями. Возвращает
  новое состояние метелей."
  [[size-x size-y] blizzs]
  (map (fn [{pos :pos dir :dir}]
         (let [[new-x new-y] (mapv + pos (directions dir))]
           {:pos [(inc (mod (dec new-x) (- size-x 2)))
                  (inc (mod (dec new-y) (- size-y 2)))]
            :dir dir}))
       blizzs))

(defn print-blizzs
  "Вспомогательная функция, выводит карту лабиринта."
  [[size-x size-y] start finish blizzs]
  (let [blizzs-map (into {} (map #(vector (:pos %) (:dir %)) blizzs))
        ;; Если в одной точке больше одной метели, нужно будет писать цифру
        ;; вместо направления, поэтому считаем заранее.
        blizzs-cnt (frequencies (map :pos blizzs))]
    (->> (range size-y)
         (map (fn [y]
                (->> (range size-x)
                     (map (fn [x]
                            (cond
                              ;; Начало пути.
                              (= [x y] start) \S
                              ;; Конец пути.
                              (= [x y] finish) \F
                              ;; Стены лабиринта.
                              (or (zero? x) (zero? y) (= x (dec size-x)) (= y (dec size-y))) \#
                              ;; Метели.
                              (blizzs-map [x y]) (if (== (blizzs-cnt [x y]) 1) (blizzs-map [x y])
                                                   (blizzs-cnt [x y] ))
                              ;; Пустые точки.
                              :else \.)))
                     (str/join ""))))
         (str/join "\n")
         println)
    blizzs))

(defn get-empty-for-plane
  "Получает список доступных, незанятых метелями точек для текущего состояния лабиринта. Также 
  в функцию передается `plane-no` - номер текущей плоскости.

  Для удобства изменения во времени можно представить как изменения в пространстве, и вместо одного
  этажа лабиринта с движущимися метелями у нас будет много этажей (плоскостей) с набором недоступных
  точек, занятых неподвижными метелями.
  
  Задача поиска пути таким образом сведется к поиску пути в таком трехмерном лабиринте, где между
  плоскостями можно двигаться только в одном направлении (вниз).
  
  Таким образом данная функция генерирует новый этаж лабиринта, а `plane-no` - номер этого этажа
  (плоскости). И номер этажа будет выступать дополнительной (первой) координатой для незанятых точек."
  [[size-x size-y] start finish plane-no blizzs]
  (let [blizzs-map (set (map :pos blizzs))]
    (->> (conj
           ;; Обходим все точки заданного состояния лабиринта.
           (for [x (range 1 (dec size-x))
                 y (range 1 (dec size-y))
                 :when (not (contains? blizzs-map [x y]))]
             [plane-no x y])
           ;; Добавляем также вход и выход в качестве незанятых точек.
           (into [plane-no] start) (into [plane-no] finish))
         ;; Добавляем флаг посещения и оценку количества шагов для алгоритма
         ;; поиска пути.
         (map #(vector % {:score ##Inf :visited false}))
         (into {}))))

(defn distances
  "Считает расстояния от точки с минимальным скором до всех доступных точек многоэтажного
  лабиринта."
  [planes prev-place-no]
  ;; Двигаться можем только вниз (первая координата), либо точно вниз, либо по одному
  ;; из четырех направлений.
  (let [moves [[1 1 0] [1 -1 0] [1 0 1] [1 0 -1] [1 0 0]]]
    (loop [planes planes]
      ;; Исходными точками могут быть точки всех этажей, кроме самого нижнего, т.к. следующий
      ;; за ним этаж еще не сгенерирован и посчитать оценки для нижнего этажа пока нельзя.
      (let [old-planes (filter #(<= (-> % key first) prev-place-no) planes)]
        ;; Если все точки на предпоследнем этаже посещены, выходим.
        (if (every? #(-> % val :visited) old-planes) planes
          (let [current (->> old-planes
                             (filter #(-> % val :visited not))
                             (sort-by #(-> % val :score))
                             first
                             key)
                new-score (inc (get-in planes [current :score]))
                ;; В качестве соседей выбираем доступные точки на следующем этаже, т.к. двигаться
                ;; мы можем только вниз.
                neighbs (->> (map #(mapv + current %) moves) (filter (partial contains? planes)))]
            (recur (reduce (fn [planes neigh]
                             (if (<= (get-in planes [neigh :score]) new-score) planes
                               (assoc-in planes [neigh :score] new-score)))
                           (assoc-in planes [current :visited] true) neighbs))))))))

;; (defn time-to-reach
;;   "Возвращает время, которое потребуется чтобы по лабиринту `blizzs` добраться
;;   от `start` до `finish` (и совершить обратный путь и т.д., если `turns > 1`)."
;;   [size start finish blizzs turns]
;;   ;; Чтобы не таскать везде эти параметры, определяем новые функции.
;;   (let [move-bliz (partial move-blizzs size)
;;         get-empty (partial get-empty-for-plane size start finish)]
;;     (loop [;; Состояние текущего этажа (координаты и направления метелей).
;;            blizzs blizzs
;;            ;; Набор свободных точек на текущем этаже.
;;            planes (assoc-in (get-empty 0 blizzs) [(into [0] start) :score] 0)
;;            start start
;;            finish finish
;;            paths []
;;            last-plane-no 0]
;;       (let [reached-finish (into [last-plane-no] finish)
;;             new-blizzs (move-bliz blizzs)]
;;         (cond
;;           (< (-> planes (get reached-finish) :score) ##Inf)
;;             (if (= (count paths) (dec turns)) (first reached-finish)
;;               (recur new-blizzs
;;                      (distances (into (get-empty (inc last-plane-no) new-blizzs)
;;                                       (filter #(or (-> % key (= reached-finish))
;;                                                    (-> % key first (> last-plane-no))) planes))
;;                                       ;; {reached-finish (get planes reached-finish)})
;;                                 last-plane-no)
;;                      finish start (conj paths (get planes reached-finish)) (inc last-plane-no)))
;;
;;           :else
;;           (recur new-blizzs
;;                  (distances (into (get-empty (inc last-plane-no) new-blizzs)
;;                                   (filter #(and (-> % key first (= last-plane-no))
;;                                                 (-> % val :score (< ##Inf))) planes))
;;                             last-plane-no)
;;                  start finish paths (inc last-plane-no)))))))
(defn time-to-reach
  "Возвращает время, которое потребуется чтобы по лабиринту `blizzs` добраться
  от `start` до `finish` (и совершить обратный путь и т.д., если `turns > 1`)."
  [size start finish blizzs]
  ;; Чтобы не таскать везде эти параметры, определяем новые функции.
  (let [move-bliz (partial move-blizzs size)
        get-empty (partial get-empty-for-plane size start finish)]
    (loop [;; Состояние текущего этажа (координаты и направления метелей).
           blizzs blizzs
           ;; Набор свободных точек на текущем этаже.
           planes (assoc-in (get-empty 0 blizzs) [(into [0] start) :score] 0)
           last-plane-no 0]
      (let [reached-finish (into [last-plane-no] finish)
            new-blizzs (move-bliz blizzs)]
        (if (< (-> planes (get reached-finish) :score) ##Inf) [blizzs reached-finish]
          (recur new-blizzs
                 (distances (into (get-empty (inc last-plane-no) new-blizzs)
                                  (filter #(and (-> % key first (= last-plane-no))
                                                (-> % val :score (< ##Inf))) planes))
                            last-plane-no)
                 (inc last-plane-no)))))))

(defn time-to-reach-and-turn
  "Возвращает сремя, которое потребуется, чтобы пройти по лабиринту и вернуться
  несколько раз."
  [size start finish blizzs turns]
  (loop [start start finish finish blizzs blizzs turns turns total 0]
    (if (zero? turns) total
      (let [[new-blizzs reached] (time-to-reach size start finish blizzs)]
        ;; Меняем местами начальную и конечную точку и передаем текущее состояние метелей.
        (recur finish start new-blizzs (dec turns) (+ total (first reached)))))))

(let [{size :size start :start finish :finish blizzs :blizzs} (parse-map data)
      [_ reached] (time-to-reach size start finish blizzs)]
  (println (first reached)))

(let [{size :size start :start finish :finish blizzs :blizzs} (parse-map data)]
  (println (time-to-reach-and-turn size start finish blizzs 3)))
