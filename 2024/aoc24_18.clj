(require '[clojure.string :as str])

(def data "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")

(defn parse-data
  [data]
  (->> (str/split-lines data)
       (map #(mapv Integer/parseInt (str/split % #",")))))

(defn get-map
  "Вспомогательная функция для печати карты."
  [[width height] ram]
  (->> (range height)
       (map (fn [y] (->> (range width)
                         (map (fn [x] (if (ram [x y]) \# \.)))
                         (apply str))))
       (str/join "\n")))

(defn get-good
  "Получает список «хороших» байт на карте."
  [[w h] bad]
  (into {} (for [x (range w)
                 y (range h)
                 :when (not (bad [x y]))]
             [[x y] {:v false :s ##Inf}])))

(defn get-next
  "Получает следующий непосещенный элемент."
  [ram]
  (some->> (filter (comp not :v second) ram)
           seq
           (apply min-key (comp :s second))))

(defn get-reach
  "Получает доступные из указанной точки элементы."
  [ram point]
  (->> [[0 1] [1 0] [-1 0] [0 -1]]
       (map (partial mapv + point))
       (filter ram)))

(defn find-path
  "Оценивает пути до всех элементов из указанного (Дийкстра)."
  [size start bad]
  (loop [ram (assoc-in (get-good size bad) [start :s] 0)]
    (if-let [[point {s :s}] (get-next ram)]
      (if-let [reach (get-reach ram point)]
        (recur (reduce (fn [ram p]
                         (if (<= (get-in ram [p :s]) (inc s)) ram
                           (assoc-in ram [p :s] (inc s))))
                       (assoc-in ram [point :v] true)
                       reach))
        ram)
      ram)))

(defn find-obstacle
  "Ищет количество «плохих» байт, при котором финиш становится
  недостижим (двоичный поиск)."
  [size start finish bytes]
  (loop [min-n 0 max-n (dec (count bytes)) n (quot max-n 2)]
    (if (= min-n n) n
      (let [ram (find-path size start (set (take n bytes)))]
        (if (= (get-in ram [finish :s]) ##Inf)
          (recur min-n n (quot (+ n min-n) 2))
          (recur n max-n (quot (+ n max-n) 2)))))))

(let [size [71 71] start [0 0] finish (mapv dec size)
      bad (set (take 1024 (parse-data (slurp "aoc24_18.txt"))))]
  (get-in (find-path size start bad) [finish :s]))

(let [size [71 71] start [0 0] finish (mapv dec size)
      bytes (parse-data (slurp "aoc24_18.txt"))]
  (nth bytes (find-obstacle size start finish bytes)))
