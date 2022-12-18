(ns aoc22_14
  (:require [clojure.string :as s]))

(def data (slurp "aoc22_14.txt"))

(def sample "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-path
  "Парсит путь, возвращает набор координат."
  [line]
  (->> (s/split line #" -> ")
       (map #(->> (s/split % #",")
                  (map parse-int)))))

(defn find-limits
  "Находит границы поля, возвращает одно из граничных значений в зависимости
  от переданных функций. Функция `select-fn` отвечает за выбор первого или 
  второго элемента, а `reduce-fn` - максимальные или минимальный элемент будет
  возвращен."
  [paths select-fn reduce-fn]
  (->> paths
       (map #(map select-fn %))
       (apply concat)
       (reduce reduce-fn)))

(defn normalize
  "Сдвигает левую границу поля к началу координат."
  [paths min-x]
  (map #(map (fn [point] (mapv - point [min-x 0])) %) paths))

(defn add-path-line
  "Добавляет элемент пути по двум точкам."
  [field [x1 y1] [x2 y2]]
  (if (= y1 y2)
    ;; Горизонтальная линия.
    (reduce (fn [field x] (assoc-in field [y1 x] "#"))
            field (range (min x1 x2) (inc (max x1 x2))))
    ;; Вертикальная линия.
    (reduce (fn [field y] (assoc-in field [y x1] "#"))
            field (range (min y1 y2) (inc (max y1 y2))))))

(defn add-path
  "Добавляет путь по набору координат."
  [field path]
  (reduce (fn [field [start finish]]
            (add-path-line field start finish)) field (zipmap (butlast path) (rest path))))

(defn make-field
  "Создает поле на основе набора путей."
  [paths width height]
  (reduce (fn [field path]
            (add-path field path))
          (vec (repeat height (vec (repeat width "."))))
          paths))

(defn move-stone
  "Передвигает камень из указанных координат. Возвращает новые координаты."
  [field [x y]]
  (cond
    (= "." (get-in field [(inc y) x]       ".")) [x       (inc y)]
    (= "." (get-in field [(inc y) (dec x)] ".")) [(dec x) (inc y)]
    (= "." (get-in field [(inc y) (inc x)] ".")) [(inc x) (inc y)]
    :else [x y]))

(defn occupied? 
  "Проверяет, занято ли указанное поле предметом."
  [field [x y]]
  (not= "." (get-in field [y x])))

(defn place-stone
  "Размещает камень по указанным координатам."
  [field [x y]]
  (assoc-in field [y x] "o"))

(defn drop-stone
  "Бросает камень из указанной точки. Возвращает `nil`, если камень упал в
  бездну или если путь заблокирован."
  [field point]
  ;; Проверяем, свободно ли исходное поле.
  (if (occupied? field point) nil
    (let [width (count (first field))
          height (count field)]
      (loop [point point]
        (let [[new-x new-y :as new-point] (move-stone field point)]
          (cond
            ;; Проверяем выход за границы.
            (or (< new-x 0) (>= new-x width) (>= new-y height)) nil
            ;; Проверяем, остановился ли камень.
            (= point new-point) (place-stone field point)
            ;; Продолжаем падение камня.
            :else (recur new-point)))))))

(defn drop-stones
  "Бросает камни, пока они не начнут падать в бездну, или пока путь не
  окажется заблокированным. Возвращает состояние поля и количество
  камней на поле."
  [field point]
  (loop [field field cnt 0]
    (let [new-field (drop-stone field point)]
      (if (nil? new-field) [field cnt]
        (recur new-field (inc cnt))))))

(defn get-end-field
  "Возвращает конечное состояние поля и количество камней на нем."
  [paths base-point]
  (let [;; Определяем границы поля.
        [min-x max-x _ max-y] (for [select-fn [first second] reduce-fn [min max]]
                                (find-limits paths select-fn reduce-fn))
        ;; Определяем ширину и высоту поля.
        [width height] [(inc (- max-x min-x)) (inc max-y)]
        ;; Определяем начальную точку после сдвига поля к началу координат.
        start (mapv - base-point [min-x 0])]
    (-> paths
        ;; Сдвигаем пути к началу координат.
        (normalize min-x)
        ;; Создаем поле.
        (make-field width height)
        ;; Бросаем камни, пока они не начнут падать в бездну или
        ;; пока не остановятся.
        (drop-stones start))))

(defn print-field
  "Печатает поле."
  [field]
  (println (s/join "\n" (map #(s/join "" %) field))))

(def base-point
  "Точка, откуда падают камни."
  [500 0])

(let [paths (map parse-path (s/split-lines data))
      [_ cnt] (get-end-field paths base-point)]
  (println cnt))

(let [paths (map parse-path (s/split-lines data))
      ;; Определяем границы поля.
      [min-x max-x _ max-y] (for [select-fn [first second] reduce-fn [min max]]
                              (find-limits paths select-fn reduce-fn))
      ;; После добавления поля высота поля увеличится на 2.
      ;; При заполнении поля камнями, в наихушем случае они заполнят поле вправо
      ;; и влево на количество точек, равное высоте. Хотя в исходных данных могут быть
      ;; объекты и за пределами этих границ.
      paths (conj paths [[(min min-x (- (first base-point) (+ max-y 2))) (+ max-y 2)]
                         [(max max-x (+ (first base-point) (+ max-y 2))) (+ max-y 2)]])
      [_ cnt] (get-end-field paths base-point)]
  (println cnt))
