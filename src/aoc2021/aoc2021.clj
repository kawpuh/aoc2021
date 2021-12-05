(ns aoc2021.aoc2021
  (:require [clojure.string :as s])
  (:gen-class))

(defn day1-part1 []
  (let [corpus (slurp "day1.txt")
        depths (map read-string (s/split-lines corpus))
        ans (->> (map < (butlast depths) (rest depths))
                 (filter identity)
                 (count))]
    ans))

(defn day1-part2 []
  (let [corpus (slurp "day1.txt")
        depths (map read-string (s/split-lines corpus))
        windows (map + depths (next depths) (nnext depths))
        ans (->> (map < (butlast windows) (rest windows))
                 (filter identity)
                 (count))]
    ans))

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

(defn day3-part1 []
  (let [input (slurp "day3.txt")
        report (s/split-lines input)
        gamma-rate (->> (apply map vector report)
                        (map frequencies)
                        (map #(first (apply max-key val %)))
                        (apply str)
                        (str "2r")
                       read-string)
        epsilon-rate (->> (apply map vector report)
                          (map frequencies)
                          (map #(first (apply min-key val %)))
                          (apply str)
                          (str "2r")
                          read-string)]
  (* epsilon-rate gamma-rate)))


(defn day3-part2 []
  (let [input (slurp "day3.txt")
        report (s/split-lines input)
        gen-rating (loop [rem-options (map str report)
                          n (count (first report))
                          acc ""]
                     (if (= n 0)
                       acc
                       (let [bit-counts (->> rem-options
                                             (apply map vector)
                                             (map frequencies)
                                             first)
                             bit-criteria (if (> (get bit-counts \0 0)
                                                 (get bit-counts \1 0)) \0 \1)]
                         (recur (->> rem-options
                                     (filter #(= bit-criteria (first %)))
                                     (map rest))
                                (dec n)
                                (str acc bit-criteria)))))
        scrub-rating (loop [rem-options (map str report)
                            n (count (first report))
                            acc ""]
                       (if (= n 0)
                         acc
                         (let [bit-counts (->> rem-options
                                               (apply map vector)
                                               (map frequencies)
                                               first)
                               bit-criteria (cond
                                              (= 0 (get bit-counts \0 0)) \1
                                              (= 0 (get bit-counts \1 0)) \0
                                              (< (get bit-counts \1 0)
                                                 (get bit-counts \0 0)) \1
                                              :else
                                              \0)]
                           (recur (->> rem-options
                                       (filter #(= bit-criteria (first %)))
                                       (map rest))
                                  (dec n)
                                  (str acc bit-criteria)))))
        gen-rating (read-string (str "2r" gen-rating))
        scrub-rating (read-string (str "2r" scrub-rating))]
    (* gen-rating scrub-rating)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (day3-part1))
  (println (day3-part2)))
