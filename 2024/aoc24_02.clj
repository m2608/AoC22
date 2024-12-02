(require '[clojure.string :as str])

(def data "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn parse-data
  "Парсит данные задачи."
  [data]
  (->> (str/split-lines data)
       (mapv #(mapv Integer/parseInt (str/split % #" ")))))

(defn remove-at
  "Удаляет указанный элемент из вектора."
  [v n]
  (into (subvec v 0 n) (subvec v (inc n))))

(defn get-dropouts-variants
  "Для указанного вектора возвращает список векторов с одним
  выпавшим элементом."
  [v]
  (mapv (partial remove-at v) (range (count v))))

(defn check-line
  "Проверяет соответствие вектора условиям первой части задачи."
  [v]
  (let [parted (mapv (partial apply -) (partition 2 1 v))]
    (and
      (or (every? pos? parted)
          (every? neg? parted))
      (every? #(<= 1 (abs %) 3) parted))))

(defn check-with-tolerance
  "Проверяет соответствие вектора условиям второй части."
  [v]
  (some check-line (get-dropouts-variants v)))

(->> (parse-data (slurp "aoc24_02.txt"))
     ((juxt (comp count (partial filter check-line))
            (comp count (partial filter check-with-tolerance)))))
