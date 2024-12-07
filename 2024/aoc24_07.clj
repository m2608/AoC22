(require '[clojure.string :as str])

(def data "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(def operators [* +])

(defn parse-bigint
  [s]
  (BigInteger. s))

(defn parse-data
  [data]
  (->> (str/split-lines data)
       (mapv (juxt #(-> % (str/split #": ") first parse-bigint)
                   #(-> % (str/split #": ") second (str/split #" ") ((partial mapv parse-bigint)))))))


(defn combinations
  "Возвращает возможные комбинации из набора опций."
  [options]
  (reduce
    (fn [result w]
      (->> result
           (mapv (fn [v]
                   (mapv (partial conj v) w)))
           (reduce into)))
    [[]]
    options))

(defn check-equation
  "Проверяет, приводит ли применение операторов `ops` последовательно к
  аргументам `args` к результату `result`."
  [result args ops]
  (loop [args args ops ops]
    (let [value (apply (first ops) (take 2 args))
          next-args (drop 2 args)]
      (cond
        ;; Если больше аргументов нет, проверяем, получили ли результат.
        (empty? next-args) (= value result)
        ;; Т.к. в задаче все операции увеличивают значение, все аргументы
        ;; положительны, можно сделать небольшую оптимизацию. Если промежуточное
        ;; значение превысило результат, то равенство уже получить не удастся.
        (> value result) false
        ;; Продолжаем вычисления.
        :else (recur (cons value next-args) (rest ops))))))

(defn find-equation
  "Для всех возможных комбинаций из операторов `operators` проверяет, 
  возможно ли получить `result` на аргументах `args`."
  [operators [result args]]
  (some (partial check-equation result args)
        (combinations (repeat (dec (count args)) operators))))

(defn part1
  [data]
  (->> data
       (filter (partial find-equation [* +]))
       (map first)
       (reduce +)))

(defn part2
  [data]
  (->> data
       (filter (partial find-equation [* + (comp parse-bigint str)]))
       (map first)
       (reduce +)))

((juxt part1 part2) (parse-data (slurp "aoc24_07.txt")))
