(require '[babashka.pods :as pods]
         '[clojure.string :as str]
         '[babashka.cli :as cli]
         '[babashka.http-client :as http])

(pods/load-pod 'retrogradeorbit/bootleg "0.1.9")

(require '[pod.retrogradeorbit.bootleg.utils :refer [convert-to]]
         '[pod.retrogradeorbit.hickory.select :as s])

(def cookie-file "session.txt")

(def url-years "https://adventofcode.com/events")
(def url-days "https://adventofcode.com/%d")
(def url-input "https://adventofcode.com/%d/day/%d/input")

(defn request-years
  "Возвращает доступные на данный момент года в виде множества."
  []
  (let [years-page (convert-to (:body (http/get url-years)) :hickory)]
    (->> years-page
         (s/select (s/and (s/tag :div) (s/class :eventlist-event)))
         (mapv #(let [year-label (->> (s/select (s/tag :a) %)
                                      first :content first)]
                  (Integer/parseInt (str/replace year-label #"\[(\d+)\]" "$1"))))
         set)))

(defn request-days
  "Возвращает список доступных заданий (дней) для указанного года в виде множества."
  [year]
  (let [days (convert-to (:body (http/get (format url-days year))) :hickory)]
    (->> days
         (s/select (s/tag :main))
         first
         (s/select (s/tag :a))
         (mapv #(let [day-label (->> (s/select (s/and (s/tag :span) (s/class :calendar-day)) %)
                                     first :content first)]
                  (Integer/parseInt (str/replace day-label #" " ""))))
         set)))

(def spec {:spec {:year   {:desc "year for the task"}
                  :day    {:desc "day for the task"}
                  :output {:desc "output file"}}
           :require []})

(defn get-options
  [data]
  (assoc data :options (cli/parse-opts *command-line-args* spec)))

(defn get-session
  [data]
  (assoc data :session (str/trim-newline (slurp cookie-file))))

(defn get-year
  "Проверяет правильность указания года. Если год не указан, выбирает последний
  доступный на сайте."
  [data]
  (let [years (request-years)
        y (get-in data [:options :year])]
    (cond
      (and y (years y)) (assoc data :year y)
      (and y (not (years y))) (do (println "Could not found the year" y "on the AoC site.")
                                  (System/exit 1))
      :else (let [y (apply max years)]
              (println "Last year:" y)
              (assoc data :year y)))))

(defn get-day
  "Проверяет правильность указания дня. Если день не указан, выбирает последний доступный
  на сайте."
  [data]
  (let [y (:year data)
        d (get-in data [:options :day])
        days (request-days y)]
    (cond
      (and d (days d)) (assoc data :day d)
      (and d (not (days d))) (do (println "Could not found day" d "in the year" y "on the AoC site.")
                                 (System/exit 1))
      :else (let [d (apply max days)]
              (println "Last day:" d)
              (assoc data :day d)))))

(defn get-output
  "Формирует имя выходного файла, если оно не указано."
  [data]
  (let [y (:year data)
        d (:day data)
        output-file (get-in data [:options :output])]
    (if output-file
      (assoc data :output output-file)
      (let [output-file (format "%04d/aoc%02d_%02d.txt" y (mod y 100) d)]
        (println "Output file:" output-file)
        (assoc data :output output-file)))))

(let [data (-> {} get-options get-session get-year get-day get-output)
      input (:body (http/get (format url-input (:year data) (:day data))
                             {:headers {"Cookie" (str "session=" (:session data))}}))]
  (spit (:output data) input))
