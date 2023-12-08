(ns aoc23_08
  (:require [clojure.string :as str]))

(def data1
"RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(def data2
"LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(def data3
"LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(def data (slurp "aoc23_08.txt"))

(defn parse-node
  "Парсим ноду, возвращает вектор из двух элементов: имени ноды и 
  списка из двух возможных ходов."
  {:test (fn [] (assert (= (parse-node "AAA = (BBB, CCC)")
                           [:AAA [:BBB :CCC]])))}
  [line]
  (let [[node left right]
        (->> line
             (re-matches #"^(\w+)[ ]+=[ ]+[(](\w+),[ ]+(\w+)[)]$")
             rest
             (map keyword))]
    [node [left right]]))

(defn parse-data
  "Парсит входные данные. Возвращает набор функций, соответствующих ходам
  и набор нод в виде мапы."
  [data]
  (->> (str/split data #"\n\n")
       ((juxt 
          first
          (fn [nodes-data]
            (->> (second nodes-data)
                 str/split-lines
                 (map parse-node)
                 (into {})))))))

(defn get-next-node
  "Получает слудующую ноду из списка нод с использованием указанной инструкции."
  [nodes instruction node]
  (({\L first \R second} instruction)
   (get nodes node)))

(defn count-steps
  "Считет количество шагов, необходимое для прохождения пути от начальной ноды
  (задается ясно) до конечной (задается функцией)."
  [instructions nodes start-node finish-fn]
  (loop [instructions (cycle instructions) node start-node count 0]
    (if (finish-fn node)
      count
      (recur (rest instructions)
             (get-next-node nodes (first instructions) node)
             (inc count)))))

(defn get-starting-nodes
  "Получает список стартовых нод для второй задачи."
  [nodes]
  (filterv #(str/ends-with? % "A") (keys nodes)))

(defn gcd
  "Вычисляет НОД."
  [a b]
  (if (zero? b) a
    (recur b (mod a b))))

(defn lcm
  "Вычисляет НОК."
  [a b]
  (abs (/ (* a b) (gcd a b))))

(let [[instructions nodes] (parse-data data)]
  (count-steps instructions nodes :AAA #(= % :ZZZ)))

(let [[instructions nodes] (parse-data data)
      starting-nodes (get-starting-nodes nodes)]
  (->>
    (mapv (fn [starting-node]
            (count-steps instructions nodes starting-node #(str/ends-with? % "Z")))
          starting-nodes)
    (reduce lcm)))
