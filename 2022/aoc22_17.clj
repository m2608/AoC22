(ns aoc22_17
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def data (s/trim-newline (slurp "aoc22_17.txt")))

(def sample ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def room [])

(defn text-shape-to-coords
  "Преобразует формы камней из визуального представления
  в набор координат."
  [shape]
  (set
    (for [x (range (count (first shape)))
          y (range (count shape))
          :when (not= \space (get-in shape [y x]))]
      [x (- (count shape) y 1)])))

(def shapes
  "Определяем формы камней."
  (mapv text-shape-to-coords
        [["####"]

         [" # "
          "###"
          " # "]

         ["  #"
          "  #"
          "###"]

         ["#"
          "#"
          "#"
          "#"]

         ["##"
          "##"]]))

(defn find-top
  "Находит координату вершины башни (или координату пола)."
  [room]
  (reduce max -1 (map second room)))

(defn add-shape
  "Добавляет камень в комнату."
  [room shape]
  (let [top-y (find-top room)]
    ;; По условиям задачи новый камень появляется на 4 пункта выше
    ;; самой высокой точки.
    (set (map (fn [[x y]] [(+ x 2) (+ y 4 top-y)])
              shape))))

(defn drop-shape
  "Опускает камень вниз на один пункт, если это возможно. Возвращает новое
  состояние комнаты (набор занятых координат) и новое состояние камня. Если
  опускать камень некуда, делает его частью конфигурации комнаты."
  [room shape]
  (let [moved-shape (set (map (fn [[x y]] [x (dec y)])
                              shape))]
    ;; Проверяем, что при опускании части камня не перекрываются с частями
    ;; камней комнаты. И что мы не достигли пола.
    (if (or (seq (set/intersection room moved-shape))
            (some (fn [[_ y]] (< y 0))
                  moved-shape))
      [(set/union room shape) #{}]
      [room moved-shape])))

(defn move-shape
  "Сдвигаем камень в сторону."
  [room shape direction]
  (let [dx (direction {:< -1 :> 1})
        moved-shape (set (map (fn [[x y]] [(+ x dx) y])
                              shape))]
    ;; Проверяем, что при перемещении части камня не пересекаются с
    ;; частями камней комнаты и что мы не достигли одной из границ.
    (if (or (seq (set/intersection room moved-shape))
            (some (fn [[x _]] (not (<= 0 x 6)))
                  moved-shape))
      [room shape]
      [room moved-shape])))

(defn show-room
  "Вспомогательная функция, выводит визуальное представление текущей
  конфигурации комнаты и падающего камня."
  [room shape]
  (let [top-y (find-top (concat room shape))]
    (->> (for [y (range top-y -1 -1)]
           (->> (map (fn [x]
                       (cond
                         (some #(= % [x y]) shape) "@"
                         (some #(= % [x y]) room) "#"
                         :else "."))
                     (range 7))
                (s/join "")))
         (s/join "\n")
         println)))

(defn tower-height
  "Вычисляет высоту башни их комней. Момент измерения определяется значением
  переданной функции `stop-fn`."
  [winds shapes stop-fn]
  (loop [;; Начальная конфигурация комнаты: камней нет.
         room #{}
         ;; В начальный момент нет и падающего камня.
         shape #{}
         ;; Преобразуем информацию о направлении ветра в ключевые слова для удобства.
         winds winds
         ;; Номер падающего камня.
         shape-no 0
         ;; Нужно ли на данном шаге учитывать ветер.
         sail false
         ;; Номер итерации.
         iter 0]
    (cond
      ;; Функция определяет момент останова на основе номера текущего камня и
      ;; номера итерации.
      (stop-fn shape-no iter)
      [(inc (find-top room)) shape-no iter]

      ;; На данном шаге сдвигаем камень в одну из сторон, в зависимости от ветра.
      sail
      (let [[room shape] (move-shape room shape (first winds))]
        (recur room shape (conj (subvec winds 1) (first winds)) shape-no false iter))

      ;; На данном шаге опускаем камень вниз.
      :else 
      (let [[room shape] (drop-shape room shape)
            new-shape (if (seq shape) shape
                        (add-shape room (nth shapes (rem shape-no (count shapes)))))
            new-shape-no (if (seq shape) shape-no (inc shape-no))]
        (recur room new-shape winds new-shape-no true (inc iter))))))

;; Считаем высоту башни после бросания 2022 камней.
(let [target-shape-no 2023
      winds (mapv (comp keyword str) data)
      [height _ _] (tower-height winds shapes (fn [shape-no _]
                                                (= shape-no target-shape-no)))]
  (println height))

;; Считаем высоту башни после бросания 1000000000000 камней.
(let [target-shape-no 1000000000001
      winds (mapv (comp keyword str) data)
      ;; Сначала находим, когда в первый раз повторится начальная конфигурация.
      [height1 shape-no1 iter1]
      (tower-height winds shapes (fn [shape-no iter]
                                   (and (> iter 0)
                                        (zero? (rem shape-no (count shapes)))
                                        (zero? (rem iter (count winds))))))
      ;; Потом находим, когда она повторится во второй раз. Дело в том, что первое повторение не обязательно
      ;; будет цикличным, т.к. на самое первой итерации построение башни камней идет относительно пола. А вот
      ;; уже после второго повторения высота будет прирастать строго с найденным циклом.
      [height2 shape-no2]
      (tower-height winds shapes (fn [shape-no iter]
                                   (and (> iter iter1)
                                        (zero? (rem shape-no (count shapes)))
                                        (zero? (rem iter (count winds))))))
      ;; Теперь надо определить, сколько полных циклов после первого повторения нам нужно будет сделать,
      ;; чтобы почти достичь нужного количества камней.
      cycles-no (quot (- target-shape-no shape-no1) (- shape-no2 shape-no1))
      ;; Соответствующая высота башни.
      almost-height (+ height1 (* (- height2 height1) cycles-no))
      ;; И количество камней после всех циклов.
      almost-shape-no (+ shape-no1 (* (- shape-no2 shape-no1) cycles-no))
      ;; Сколько камней осталось.
      remaining-shape-no (- target-shape-no almost-shape-no)
      ;; Считаем, какая будет высота, если бросить оставшиеся камни после
      ;; первого повторения.
      [height3]
      (tower-height winds shapes (fn [shape-no _]
                                   (= shape-no (+ remaining-shape-no shape-no1))))]
  (println (+ almost-height (- height3 height1))))
