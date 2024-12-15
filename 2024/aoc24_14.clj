(require '[clojure.string :as str]
         '[clojure.java.io :as io])

(def data "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(defn parse-line
  [line]
  (when-let [[x y vx vy] (mapv Integer/parseInt
                               (rest (re-matches #"p=(\d+),(\d+) v=([-]?\d+),([-]?\d+)" line)))]
    {:p [x y] :v [vx vy]}))

(defn move-robot
  "Перемещает робота на указанное количество шагов."
  [size n robot]
  (assoc robot :p (mapv mod (mapv + (robot :p) (mapv (partial * n) (robot :v))) size)))

(defn count-in-quadrant
  "Считает количество роботов в заданном квадранте."
  [robots [[x0 x1] [y0 y1]]]
  (count (filter (fn [robot]
                   (let [[x y] (robot :p)]
                     (and (<= x0 x (dec x1)) (<= y0 y (dec y1)))))
                 robots)))

(defn generate-pbm
  "Создает представление поля в формате pbm."
  [[width height] robots]
  (let [coords (set (map :p robots))]
    (str
      (str/join "\n" ["P1" (str width " " height)])
      "\n"
      (str/join "\n" (mapv (fn [y]
                             (str/join " "
                                    (mapv (fn [x]
                                            (if (coords [x y]) \1 \0))
                                          (range width))))
                           (range height))))))

(defn generate-pbm-files
  "Функция генерирует pbm файлы для каждого шага."
  [size robots name-format]
  (let [cycle (apply * size)]
    (loop [robots robots n 0]
      (spit (format name-format n) (generate-pbm size robots))
      (when (< n cycle)
        (recur (mapv (partial move-robot size 1) robots)
               (inc n))))))

(let [[xn yn :as size] [101 103]
      ;; Функции для получения левой и правой (или верхней и нижней) границ.
      ql #(quot % 2)
      qr (comp inc ql)
      ;; Определения квадрантов.
      quadrants [[[0  (ql xn)] [0  (ql yn)]]
                 [[0  (ql xn)] [(qr yn) yn]]
                 [[(qr xn) xn] [0  (ql yn)]]
                 [[(qr xn) xn] [(qr yn) yn]]]
      robots (mapv parse-line (str/split-lines (slurp "aoc24_14.txt")))]

  ;; Решение первой части.
  (->> quadrants
       (mapv (partial count-in-quadrant (mapv (partial move-robot size 100) robots)))
       (reduce *)
       println)

  ;; Здесь мы генерируем изображения для каждого состояния поля до повторения. Дальше
  ;; есть два варианта. Можно найти ёлочку визуально, например, сгенерировав миниатюры:
  ;;
  ;; ```
  ;; nsxiv -t *
  ;; ```
  ;;
  ;; Другой вариант - сжать файлы gzip'ом и отсортировать по размеру. Файл с ёлочкой
  ;; сожмется лучше всего:
  ;;
  ;; ```
  ;; fd --no-ignore -g '*.pbm' -x gzip
  ;; lsd -la -S
  ;; ```
  (generate-pbm-files size robots "out/%06d.pbm"))
