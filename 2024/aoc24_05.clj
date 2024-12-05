(require '[clojure.string :as str])

(def data "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn split-in-lines-to-int
  [delim lines]
  (mapv #(mapv Integer/parseInt (str/split % delim)) lines))

(defn parse-data
  [data]
  (->> (str/split data #"\n\n")
       (mapv str/split-lines)
       ((juxt (comp (partial split-in-lines-to-int #"[|]") first)
              (comp (partial split-in-lines-to-int #"[,]") second)))))

(defn check-rules
  "Проверяет, что последовательность в `update` удовлетворяет правилам."
  [rules update]
  (every? (fn [[a b]]
            (let [na (.indexOf update a)
                  nb (.indexOf update b)]
              (or (< na 0) (< nb 0) (< na nb))))
          rules))

(defn middle-value
  "Возвращает значение из середины массива."
  [v]
  (nth v (quot (count v) 2)))

(defn part1
  [[rules updates]]
  (->> updates
       ;; Выбираем `updates`, удовлетворящие правилам.
       (filter (partial check-rules rules))
       (mapv middle-value)
       (reduce +)))

(defn comp-fn
  "Возвращает компаратор, который сравнивает два значения по правилам `rules`."
  [rules a b]
  (cond
    (>= (.indexOf rules [a b]) 0) 1
    (>= (.indexOf rules [b a]) 0) -1
    :else 0))

(defn part2
  [[rules updates]] 
  (->> updates
       ;; Выбираем `updates`, нарушающие правила.
       (filter (complement (partial check-rules rules)))
       ;; Сортируем их.
       (mapv (partial sort (partial comp-fn rules)))
       (mapv middle-value)
       (reduce +)))

((juxt part1 part2) (parse-data data))
