(ns aoc22_10
  (:require [clojure.string :as s]))

(def sample1 "noop
addx 3
addx -5")

(def sample2 (slurp "aoc22_10_sample2.txt"))

(def data (slurp "aoc22_10.txt"))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-data
  "Парсит входные данные, возвращает список команд, каждая команда представлена
  парой [:команда аргумент]."
  [data]
  (->> data
       (s/split-lines)
       (map (fn [line]
              (let [[cmd arg] (s/split line #" ")]
                [(keyword cmd) (and arg (parse-int arg))])))))

(defn run
  "Выполняет переданные на вход команды. Возвращает список значений регистра
  для каждого цикла."
  [commands]
  (loop [commands commands queue (clojure.lang.PersistentQueue/EMPTY) x 1 values '()]
    ;; Выходим из рекурсии только если команды во входных данных
    ;; закончились и все команды выполнены.
    (if (and (empty? commands) (empty? queue))
      (reverse (conj values x))
      (recur (rest commands)
             ;; Добавляем функцию, соответствующую текущей команде, в очередь.
             (apply conj (pop queue)
                    (let [[cmd arg] (first commands)]
                      (case cmd
                        ;; Команда `noop` не меняет значения регистра.
                        :noop (list identity)
                        ;; Команда `addx` в первом цикле не меняет значения регистра,
                        ;; а во втором - прибавляет к нему свой аргумент.
                        :addx (list identity #(+ % arg))
                        ;; Во всех остальных случаях (если команды на входе закончились),
                        ;; не добавляем команд в очередь.
                        '())))
             ;; Применяем функцию из очереди.
             ((if (empty? queue) identity (peek queue)) x)
             (conj values x)))))

(defn signal-strengths
  "Определяем «силу сигнала» для каждого `period` цикла, начиная с `start`."
  [start period values]
  (->> (zipmap (range start (count values) period)
               (take-nth period (drop start values)))
       (map (partial apply *))))

(defn visualize
  "Визуализирует значения для сетки шириной в указанное количество колонок. 
  Возвращает текст."
  [columns values]
  (->> values
       ;; В задании исходное значение не учитывается.
       (rest)
       (map-indexed (fn [n value]
                      (if (<= (Math/abs (- (mod n columns) value)) 1) "#" ".")))
       (partition columns)
       (map s/join)
       (s/join "\n")))

(->> data
     (parse-data)
     (run)
     (signal-strengths 20 40)
     (reduce +)
     (println))

(->> data
     (parse-data)
     (run)
     (visualize 40)
     (println))
