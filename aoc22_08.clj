(ns aoc22_08
  (:require [clojure.string :as s]))

;; Текущие данные.
(def data (slurp "aoc22_08.txt"))

;; Тестовые данные.
(def data "30373
25512
65332
33549
35390")

(defn parse-int [s]
  (Integer/parseInt s))

(defn get-row
  "Возвращает «строку» деревьев в лесу."
  [forest r]
  (nth forest r))

(defn get-col
  "Возвращает «столбец» деревьев в лесу."
  [forest c]
  (mapv #(nth % c) forest))

(defn get-width
  "Ширина леса."
  [forest]
  (count (first forest)))

(defn get-height
  "Высота леса."
  [forest]
  (count forest))

(defn get-higher-trees
  "Возвращает список деревьев, которые не ниже указанного по левую (или верхнюю)
  сторону от него."
  [rc n]
  (let [tree (nth rc n)]
    (->> (take n rc)
         (filter #(>= % tree)))))

(defn get-visible-trees
  "Возвращает список деревьев, которые находятся по левую (или верхнюю) сторону
  от указанного дерева."
  [rc n]
  (let [tree (nth rc n)]
    ;; Считаем все деревья, ниже текущего и добавляем следующее, более высокое дерево.
    (->> (reverse (take n rc))
         (take-while #(< % tree))
         (count)
         ;; Надо добавить дерево, которое выше текущего, но тоже считается видимым.
         (inc)
         ;; Но деревьев не может быть больше, чем всего в списке (такое может случиться,
         ;; если все деревья ниже текущего).
         (min n)
         (#(take % rc)))))

(defn other-side
  "Вызывает переданную функцию для деревьев с другой (правой или нижней) стороны
  указанного дерева."
  [f rc n]
  (f (reverse rc) (- (count rc) n 1)))

(defn get-trees-in-direction
  "Возвращает список видимых деревьев в указанном направлении. Какие деревья
  будут выбраны определяется переданной функцией."
  [get-trees-fn forest r c direction]
  (apply (case direction
           (:left :top)     get-trees-fn
           (:right :bottom) #(other-side get-trees-fn %1 %2))
         (case direction
           (:left :right) [(get-row forest r) c]
           (:top :bottom) [(get-col forest c) r])))

(defn get-score-for-trees-in-all-directions
  "Получает оценку для всех деревьев. В функцию нужно передать две функции:
  для получения списка деревьев по координатам дерева и для аггрегации этого
  списка в оценку."
  [get-trees-fn agg-fn forest]
  (for [r (range (get-height forest))
        c (range (get-width forest))]
    (->> [:left :right :top :bottom]
         (map #(get-trees-in-direction get-trees-fn forest r c %))
         (agg-fn))))

;; Количество деревьев, видимых из-за пределов леса.
(->> data
     (s/split-lines)
     (mapv #(->> (s/split % #"")
                 (mapv parse-int)))
     ((fn [forest]
        (get-score-for-trees-in-all-directions
          ;; Получаем для каждого напраления деревья выше текущего.
          get-higher-trees
          #(->> %
                ;; Считаем количество деревьев выше текущего для каждого
                ;; направления.
                (map count)
                ;; Оставляем элементы, соответствующие направлениям, где
                ;; нет деревьев выше текущего.
                (filter zero?))
          forest)))
     ;; Оставляем только элементы, соответствующие деревьям, которые
     ;; видны хотя бы с одной стороны.
     (filter seq)
     ;; Считаем, сколько таких деревьев.
     (count)
     (println))

;; Максимальная оценка видимости ландшафта для всех деревьев.
(->> data
     (s/split-lines)
     (mapv #(->> (s/split % #"")
                 (mapv parse-int)))
     ((fn [forest]
        (get-score-for-trees-in-all-directions
          ;; Получаем для каждого направления деревья, которые видны
          ;; с текущего.
          get-visible-trees
          ;; Считаем количество деревьев по каждому направлению и перемножаем
          ;; эти числа, получая скор.
          #(->> % (map count) (reduce *))
          forest)))
     ;; Берем максимальный скор из списка.
     (apply max)
     (println))
