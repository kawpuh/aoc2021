(ns aoc2021.aoc2021
  (:require [clojure.string :as s])
  (:gen-class))

(defn day1-part1 []
  (let [corpus (slurp "day1.txt")
        depths (map read-string (s/split-lines corpus))
        ans (->> (map < (butlast depths) (rest depths))
                 (filter identity)
                 (count))]
    (println ans)))

(defn day1-part2 []
  (let [corpus (slurp "day1.txt")
        depths (map read-string (s/split-lines corpus))
        windows (map + depths (next depths) (nnext depths))
        ans (->> (map < (butlast windows) (rest windows))
                 (filter identity)
                 (count))]
    (println ans)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (day1-part1)
  (day1-part2))
