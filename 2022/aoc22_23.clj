(ns aoc22_23
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data (slurp "aoc22_23.txt"))

(def sample "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..")

(defn parse-elves
  "Парсит исходное расположение эльфов, возвращает
  множество координат."
  [data']
  (let [data (str/split-lines data')]
    (into #{}
          (for [x (range (-> data first count))
                y (range (-> data count))
                :when (= (get-in data [y x]) \#)]
            [x y]))))

(def directions
  "Возможные направления для проверки и движения."
  {:N  [ 0 -1]
   :NE [ 1 -1]
   :E  [ 1  0]
   :SE [ 1  1]
   :S  [ 0  1]
   :SW [-1  1]
   :W  [-1  0]
   :NW [-1 -1]
   :XX [ 0  0]})

(def stop-rule
  "Правило, определяющее, когда эльфу можно не перемещаться."
  {:dirs [:N :NE :E :SE :S :SW :W :NW] :move :XX})

(def move-rules
  "Правила, определяющие, куда эльф будет двигаться на данном шаге."
  [{:dirs [:N :NE :NW] :move :N}
   {:dirs [:S :SE :SW] :move :S}
   {:dirs [:W :NW :SW] :move :W}
   {:dirs [:E :NE :SE] :move :E}])

(defn check-rule
  "Функция для проверки правил."
  [elves elf rule]
  (let [check-positions (map #(mapv + elf (directions %))
                             (:dirs rule))]
    (every? (comp not (partial contains? elves)) check-positions)))

(defn propose-position
  "Возвращает предполагаемую следующую позиция для указанного эльфа."
  [rules elves elf]
  (loop [rules (cons stop-rule rules)]
    (cond
      ;; Ни одно правило не подошло, стоим на месте.
      (empty? rules)
      elf
      ;; Одно из правил подошло, возвращаем новую позицию.
      (check-rule elves elf (first rules))
      (mapv + elf (directions (:move (first rules))))
      ;; Продолжаем проверять правила.
      :else
      (recur (rest rules)))))

(defn propose-positions
  "Возвращает предполагаемые следующие позиции для всех эльфов в виде
  мапы, содержащей старую и новую позиции."
  [rules elves]
  (map (fn [elf]
         {:old elf
          :new (propose-position rules elves elf)})
       elves))

(defn resolve-conflicts
  "Определяет, какие эльфы не могут двигаться из-за того, что они хотят
  на данном шаге занять одну и ту же позицию."
  [proposed]
  ;; Смотрим, какие новые позиции встречаются больше одного раза.
  (let [conflicts (->> proposed
                       (map :new)
                       frequencies
                       (filter #(> (val %) 1))
                       keys
                       set)]
    ;; Для конфликтных позиций оставляем старую позицию, для остальных - 
    ;; возвращаем новую.
    (into #{} (map #(if (contains? conflicts (:new %)) (:old %)
                      (:new %))
                   proposed))))

(defn move-elves
  "Перемещает всех эльфов. Возвращает сдвинутый циклически набор
  правил и новые координаты эльфов."
  [[rules elves]]
  [(conj (subvec rules 1) (first rules))
   (resolve-conflicts (propose-positions rules elves))])


(defn get-borders
  "Получает границы занимаемого эльфами поля."
  [elves]
  (for [select-fn [first last]
        reduce-fn [min max]]
    (reduce reduce-fn (map select-fn elves))))

(defn print-map
  "Вспомогательная функция, печатает карту поля."
  [[rules elves]]
  (let [[min-x max-x min-y max-y] (get-borders elves)]
    (->> (range min-y (inc max-y))
         (map (fn [y]
                (->> (range min-x (inc max-x))
                     (map (fn [x]
                            (if (contains? elves [x y]) "#" ".")))
                     (str/join ""))))
         (str/join "\n")
         println)
    (println)
    [rules elves]))

(defn count-empty
  "Считает количество незанятых позиций на поле."
  [[_ elves]]
  (let [[min-x max-x min-y max-y] (get-borders elves)]
    (- (* (- max-x min-x -1) (- max-y min-y -1)) (count elves))))

;; Считаем количество незанятых позиций после 10 раунда.
(let [elves (parse-elves data)]
  (->> [move-rules elves]
       ((apply comp (repeat 10 move-elves)))
       count-empty
       println))

;; Считаем, сколько шагов потребуется для распределения эльфов по полю.
(let [elves (parse-elves data)]
  (loop [[rules elves] [move-rules elves] iter 1]
    (let [[new-rules new-elves] (move-elves [rules elves])]
      ;; Передвигаем эльфов, пока все не остановятся.
      (if (empty? (set/difference elves new-elves)) (print iter)
        (recur [new-rules new-elves] (inc iter))))))
