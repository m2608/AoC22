(ns aoc22_07
  (:require [clojure.string :as s]))

(defn parse-int [s]
  (Integer/parseInt s))

;; Рабочие данные.
(def data (slurp "aoc22_07.txt"))

;; Тестовые данные.
(def data "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn parse-command
  "Парсит команду из строки. Возвращает nil, если это не удалось. Если
  удалось, возвращает команду и ее аргумент, если таковой имеется."
  [line]
  (let [parsed (re-matches #"[$][ ]+(cd|ls)(?:[ ]+(.*))?" line)]
    (rest parsed)))

(defn update-tree
  "Добавляет в дерево файлы по указанному пути. Возвращает обновленный
  вариант дерева."
  [tree path files-and-dirs]
  (reduce (fn [tree file-or-dir]
            (let [[file-size-or-dir file-name] file-or-dir]
              ;; Каталоги игнорируются, в дерево они будут добавлять только
              ;; при переходе в них с помощью команды "cd".
              (if (= file-size-or-dir "dir") tree
                (assoc-in tree (conj path file-name) (parse-int file-size-or-dir)))))
          tree files-and-dirs))

(defn parse-file-and-dirs
  "Получает список каталогов и файлов до следующей команды."
  [lines]
  (->> lines
       ;; Берем все строки, пока не встретим команду.
       (take-while #(empty? (parse-command %)))
       (map #(s/split % #" "))))

(defn parse-tree
  "Восстанавливает дерево каталог по истории команд. Возвращает дерево (в 
  виде мапы) и список каталогов (в виде списков элементов пути)."
  [data]
  (loop [lines (s/split-lines data) path [] tree {} paths [[]]]
    (if (empty? lines) [tree paths]
      (let [[command dir] (parse-command (first lines))]
        (cond
          ;; Для команды "ls" добавляем файлы из текущего каталога
          ;; в дерево и переходим к следующей команде.
          (= command "ls")
          (let [files-and-dirs (parse-file-and-dirs (rest lines))]
            (recur (drop (inc (count files-and-dirs)) lines) path (update-tree tree path files-and-dirs) paths))
          (= command "cd")
          (cond
            ;; Переход на уровень выше, убираем последний элемент из текущего пути.
            (= dir "..")
            (recur (rest lines) (pop path) tree paths)
            ;; Переход в корень, очищаем текущий путь.
            (= dir "/")
            (recur (rest lines) [] tree paths)
            ;; Переход куда-то еще. Добавляем каталог к текущему пути, добавляем каталог
            ;; в дерево, добавляем путь к каталогу в общий список путей.
            :else
            (recur (rest lines) (conj path dir) (assoc-in tree (conj path dir) {}) (conj paths (conj path dir)))))))))

(defn tree-element-type
  "Определяет тип элемента дерева."
  [[_ folder-or-file-size]]
  (if (map? folder-or-file-size) :folders :files))

(defn get-size
  "Получает общий размер переданного дерева."
  [tree]
  (loop [stack [tree] size 0]
    (if (empty? stack) size
      ;; Группируем по типу и выделяем файлы и каталоги отдельно.
      (let [[files folders] (map (group-by tree-element-type (first stack))
                                 [:files :folders])]
        ;; Добавляем каталоги в стек, а размер файлов к общему размеру.
        (recur (concat (rest stack) (map second folders))
               (reduce + size (map second files)))))))

(defn get-all-sizes
  "Получает размеры всех переданных каталогов. Каждый каталог передается как
  список элементов пути."
  [tree paths]
  (map (fn [path]
         {:path path :size (get-size (get-in tree path))}) paths))

(let [size-limit 100000]
  (->> (parse-tree data)
       (apply get-all-sizes)
       (filter #(<= (:size %) size-limit))
       (map :size)
       (reduce +)))

(let [total-size 70000000
      target-free 30000000
      all-sizes (apply get-all-sizes (parse-tree data))
      to-free (- target-free (- total-size (:size (first all-sizes))))]
  (->> all-sizes
       (filter #(> (:size %) to-free))
       (map :size)
       (apply min)))
