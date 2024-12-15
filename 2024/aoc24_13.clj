(require '[clojure.string :as str])

(def data "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(def prices {:a 3 :b 1})

(defn parse-button
  [line]
  (when-let [[_ name x y] (re-matches #"^Button (A|B): X[+](\d+), Y[+](\d+)$" line)]
    {(keyword (str/lower-case name)) (mapv Integer/parseInt [x y])}))

(defn parse-prize
  [line]
  (when-let [[_ x y] (re-matches #"^Prize: X=(\d+), Y=(\d+)$" line)]
    {:prize (mapv Integer/parseInt [x y])}))

(defn parse-data
  [data]
  (mapv (fn [machine-data]
          (let [[button-a button-b prize] (str/split-lines machine-data)]
            (into {} [(parse-button button-a) (parse-button button-b) (parse-prize prize)])))
        (str/split data #"\n\n")))

(defn get-claw
  "Получаем коэффициенты для позиционирования клешни. Два уравнение, две неизвестных,
  просто решаем систему."
  [machine]
  (let [{[p q] :prize [x1 y1] :a [x2 y2] :b} machine
        δ (- (* x2 y1) (* x1 y2))]
    (when (not (zero? δ))
      (let [[a a'] ((juxt quot mod) (- (* q x2) (* p y2)) δ)
            [b b'] ((juxt quot mod) (- (* p y1) (* q x1)) δ)]
        (when (= 0 a' b')
          [a b])))))

(defn get-price
  "Возвращает оценку стоимости для указанных коэффициентов."
  [a b]
  (+ (* a (prices :a)) (* b (prices :b))))

(let [machines1 (parse-data (slurp "aoc24_13.txt"))
      machines2 (mapv #(update % :prize (partial mapv (partial + 10000000000000N))) machines1)]
  (mapv (fn [machines]
          (->> machines
               (map get-claw)
               (filter seq)
               (map (partial apply get-price))
               (reduce +)))
        [machines1 machines2]))
