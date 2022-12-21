(ns aoc21_01
  (:require [clojure.string :as str]))

(def data (slurp "aoc22_21.txt"))

(defn parse-int [s]
  (Integer/parseInt s))

(defn sign [n]
  (cond
    (< n 0) -1
    (> n 0) 1
    :else 0))

(def sample "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32") 

(def operations
  "Операции, которые может выполнять обезьяна."
  {"*" *
   "+" +
   "/" /
   "-" -})

(defn parse-line
  "Парсит информацию об одной обезьяне. Возвращает список из двух элементов, первый элемент будет
  не nil для обезьян, сообщающих числа, второй - для производящих операции. Например:

  ```
  [{:lfqf 4} nil]
  [nil {:drzm [* :hmdt :zczc]}]
  ```"
  [line]
  (let [numb (zipmap [:full :name :value]
                     (re-matches #"^(\w+): (\d+)$" line))
        oper (zipmap [:full :name :arg1 :oper :arg2]
                     (re-matches #"^(\w+): (\w+) ([-+/*]) (\w+)$" line))]
    [(when (seq numb)
       ;; Сообщает число.
       {(keyword (:name numb)) (parse-int (:value numb))})
     (when (seq oper)
       ;; Выполняет операцию. Операцию заменяем на соответствующую функцию.
       {(keyword (:name oper)) [(operations (:oper oper)) (keyword (:arg1 oper)) (keyword (:arg2 oper))]})]))

(defn parse-monkeys
  "Парсит информацию об обезьянах. Возвращает два списка: в первом обезьяны,
  которые сообщают числа, во втором - которые выполняют операции."
  [lines]
  (loop [lines lines numbs {} opers {}]
    (if (empty? lines) [numbs opers]
      (let [[numb oper] (parse-line (first lines))]
        (cond
          numb (recur (rest lines) (into numbs numb) opers)
          oper (recur (rest lines) numbs (into opers oper)))))))

(defn calculate-oper
  "Вычисляет результат операции для обезьяны, если аргументы уже известны. Если нет,
  возвращает nil."
  [numbs oper]
  (let [[func arg1 arg2] (val oper)]
    (and (numbs arg1) (numbs arg2)
         (vector (key oper) (func (numbs arg1) (numbs arg2))))))

(defn calculate
  "Вычисляет итог для всех обезян."
  ([[numbs opers]]
   ;; В первой задаче не подменяем последнюю операцию и не заменяем число humn.
   (calculate [numbs opers] (first (:root opers)) (:humn numbs)))
  ([[numbs opers] root-fn humn-value]
   ;; Однако для выполнения второй задачи нужна возможность подменять операции и
   ;; заменять число.
   (loop [numbs (assoc numbs :humn humn-value)
          opers (assoc-in opers [:root 0] root-fn)]
     (if (:root numbs) (:root numbs)
       ;; На каждой итерации вычисляем те выражения, которые можно вычислить
       ;; и переквалифицируем соответствующих обезьян в "числовые".
       (let [new-numbs
             (->> opers
                  (map (partial calculate-oper numbs))
                  (filter identity)
                  (into {}))]
         (recur (into numbs new-numbs) (apply dissoc opers (keys new-numbs))))))))

(defn search-equality
  "Ищет значение humn, при котором два аргумента финального выражения будут равны.
  Использует метод деления пополам."
  [[numbs opers]]
  ;; Вспомогательные функции для определения знака результата вычислений.
  (letfn [(get-sign [value] (sign (calculate [numbs opers] - value)))
          (get-signs [left right] (map get-sign [left right]))]
    ;; Все числа в исходных данных положительные, предположим, что и ответ будет
    ;; положительным, поэтому в качестве нижней границы указываем 0.
    (loop [left 0 right 1
           [ls rs] (get-signs left right)]
      (cond
        (zero? ls) left
        (zero? rs) right
        ;; Если знак одинаков для обеих границ, расширяем эти границы.
        (= ls rs) (let [left (quot left 2) right (* right 2)]
                    (recur left right (get-signs left right)))
        ;; Если же знак разный, вычисляем знак в средней точке. После
        ;; чего продолжаем поиск между границ с разными знаками.
        :else (let [center (quot (+ left right) 2)
                    cs (get-sign center)]
                (if (= ls cs)
                  (recur center right [cs rs])
                  (recur left center [ls cs])))))))

(->> data
     str/split-lines
     parse-monkeys
     calculate
     println)

(->> data
     str/split-lines
     parse-monkeys
     search-equality
     println)
