(ns aoc22_11
  (:require [clojure.string :as s]))

(def data (slurp "aoc22_11.txt"))

(def sample (slurp "aoc22_11_sample.txt"))

(defn parse-int [s]
  (Integer/parseInt s))

(defn split [re s]
  (s/split s re))

(defn parse-operation
  "Возвращает функцию, соответствующую операции, записанной в строке."
  [line]
  (let [[arg1 op arg2] (s/split line #" ")]
    (fn [x]
      ((case op
         "*" *
         "+" +)
       (if (= arg1 "old") x (parse-int arg1))
       (if (= arg2 "old") x (parse-int arg2))))))

(def parses
  "Парсинг полей для одной обезьяны."
  [{:key :monkey
    :fn #(some->> % (re-matches #"^Monkey (\d+):")
                  (second) (parse-int))}
   {:key :items
    :fn #(some->> % (re-matches #"^\s+Starting items:\s+((?:(?:\d+)(?:,\s+)?)+)$")
                  (second) (split #",\s+") (mapv parse-int))}
   {:key :operation
    :fn #(some->> % (re-matches #"^\s+Operation:\s+new\s+=\s+(.*)$")
                  (second) (parse-operation))}
   {:key :divider
    :fn #(some->> % (re-matches #"^\s+Test:\s+divisible by\s+(\d+)$")
                  (second) (parse-int))}
   {:key :true
    :fn #(some->> % (re-matches #"^\s+If true: throw to monkey\s+(\d+)$")
                  (second) (parse-int))}
   {:key :false
    :fn #(some->> % (re-matches #"^\s+If false: throw to monkey\s+(\d+)$")
                  (second) (parse-int))}])

(defn parse-monkey
  "Получает информацию об обезьяне из исходных данных. Возвращает мапу."
  [data]
  ;; Используем свой парсер для каждой строки.
  (->> (map (fn [[line parse]] [(:key parse) ((:fn parse) line)])
            (zipmap (s/split-lines data) parses))
       ;; Количество раз, которое обезьяна проверяла предметы. В начале - 0.
       (into {:inspected 0})
       ((fn [monkey]
          ;; Формируем функцию, которая, исходя из условия данной обезьяны будет
          ;; возвращать номер обезьяны, которой будет переброшен предмет.
          (assoc monkey :test (fn [x] (if (zero? (mod x (:divider monkey))) (:true monkey)
                                        (:false monkey))))))))

(defn monkey-run
  "Для данной обезьяны возвращает какой обезьяне и какие предметы нужно перекинуть.
  В качестве первого параметра принимает функцию «уменьшения беспокойства»."
  [dump-fn monkey]
  (map (fn [item]
         (let [level (dump-fn ((:operation monkey) item))]
           [((:test monkey) level) level]))
       (:items monkey)))

(defn monkeys-update
  "Обновляет информацию об обезьянах в соответствии со списком перекидываемых предметов."
  [monkeys items-update]
  (reduce (fn [monkeys [no item]]
            (assoc-in monkeys [no :items]
                      (conj (get-in monkeys [no :items]) item)))
          monkeys items-update))

(defn round
  "Вычисляет состояние обезьян после одного раунда."
  [dump-fn monkeys]
  (reduce (fn [monkeys no]
            (let [current (get monkeys no)
                  ;; Получаем список перекидываемых предметов для данной обезьяны.
                  items-update (monkey-run dump-fn current)]
              (-> monkeys
                  ;; Она перебросит все свои предметы, у нее ничего не останется.
                  (assoc-in [no :items] [])
                  ;; Увеличиваем счетчик, показывающий, сколько раз обезьяна
                  ;; рассматривала предметы.
                  (assoc-in [no :inspected]
                            (+ (:inspected current) (count items-update)))
                  ;; Обновляем информацию об обезьянах, добавляя им предметы, перекинутые
                  ;; текущей обезьяной.
                  (monkeys-update items-update))))
          monkeys
          (range (count monkeys))))

(defn monkey-business
  "Вычисляет уровень «занятости» обезьян за указанное количество раундов.
  Третий параметр - функция, которая возвращает функцию подавления беспокойства на основе
  информации об обезьянах."
  [data rounds-no make-dump-fn]
  (->> (s/split data #"\n\n")
       (mapv parse-monkey)
       ((fn [monkeys]
          (let [dump-fn (make-dump-fn monkeys)]
            (reduce (fn [monkeys _] (round dump-fn monkeys)) monkeys (range rounds-no)))))
       (sort-by :inspected)
       (map :inspected)
       (reverse)
       (take 2)
       (apply *)))

(println (monkey-business data 20
                          (fn [_]
                            ;; Функция подавления беспокойства в первом задании никак не зависит от обезьян.
                            (fn [x] (quot x 3)))))

(println (monkey-business data 10000
                          (fn [monkeys]
                            ;; Во втором задании функция подавления беспокойства не должна влиять на результаты
                            ;; проверки делимости обезьянами. Поэтому берем остаток от деления на произведение
                            ;; всех возможных делителей.
                            (fn [x] (rem x (apply * (map :divider monkeys)))))))
