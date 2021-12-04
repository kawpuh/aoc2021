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

(defn day2-part1 []
  (let [input (slurp "day2.txt")
        cmds (->> input s/split-lines
                  (map #(s/split % #"\s"))
                  (map #(vector (keyword (first %))
                                (Integer/parseInt (second %)))))
        [final-horiz final-depth]
        (reduce
          (fn [[horiz depth] [instr n]]
            (case instr
              :forward [(+ n horiz) depth]
              :down [horiz (+ depth n)]
              :up [horiz (- depth n)]))
          [0 0]
          cmds)]
    (* final-horiz final-depth)))

(defn day2-part2 []
  (let [input (slurp "day2.txt")
        cmds (->> input s/split-lines
                  (map #(s/split % #"\s"))
                  (map #(vector (keyword (first %))
                                (Integer/parseInt (second %)))))
        [final-horiz final-depth _final-aim]
        (reduce
          (fn [[horiz depth aim] [instr n]]
            (case instr
              :forward [(+ n horiz) (+ depth (* n aim)) aim]
              :down [horiz depth (+ aim n)]
              :up [horiz depth (- aim n)]))
          [0 0 0]
          cmds)]
    (* final-horiz final-depth)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ; (day1-part1)
  ; (day1-part2)
  (println (day2-part1))
  (println (day2-part2))
  )
