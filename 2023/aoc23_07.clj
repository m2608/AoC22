(ns aoc23_07
  (:require [clojure.string :as str]))

(def data
"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(comment
  (def data (slurp "aoc23_07.txt")))

(def cards1 "23456789TJQKA")
(def cards2 "J23456789TQKA")

(defn parse-int [s]
  (Integer/parseInt s))

(defn compare-hands
  "Функция для сравнения рук. Каждая рука представлена тремя элементами:
  * список, содержащий количество элементов в каждой из групп, в порядке убывания;
  * численное значение силы карты;
  * ставка (в данной функции игнорируется)."
  [[hv1 h1 _] [hv2 h2 _]]
  (let [c1 (count hv1) c2 (count hv2)]
    ;; Сначала проверяем количество групп, выигрывает рука, где групп меньше.
    (if (= c1 c2)
      ;; Потом сравниваем сами группы. Выигрывает рука, где в группах больше элементов.
      (if (= hv1 hv2)
        ;; Наконец сравниваем руки.
        (compare h1 h2)
        (compare hv1 hv2))
      (> c1 c2))))

(def get-hands-values1
  (comp vec reverse sort vals frequencies first))

(defn get-hands-values2
  "Функция для оценки рук во второй задаче. Считаем количество джокеров и прибовляем его к
  самой сильной группе."
  [[hand _]]
  (let [[groups j-count] ((juxt #(->> (dissoc % \J) (map second) sort reverse vec)
                                #(get % \J 0))
                          (frequencies hand))]
    (vec (conj (rest groups) (+ (get groups 0 0) j-count)))))

(defn get-rank
  "Считает ранг для списка рук. На вход также передается список карт в порядке возврастания
  силы и функция для оценки рук."
  [hands cards values-fn]
  (let [hands-values
        (->> hands
             str/split-lines
             (map (fn [line] (->> (str/split line #"\s+")
                                  ((juxt values-fn
                                         (comp (partial mapv (partial str/index-of cards)) first)
                                         (comp parse-int second)))))))]
    (->> hands-values
         (sort compare-hands)
         (map last)
         (map vector (map inc (range (count hands-values))))
         (map (partial apply *))
         (reduce +))))

(get-rank data cards1 get-hands-values1)
(get-rank data cards2 get-hands-values2)
