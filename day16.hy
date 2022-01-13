(import functools)

(defn hex_to_bin [hex_s]
  (cut (str (bin (int hex_s 16))) 2 None))

(defn parse_literal [bin_str]
  (setv cursor 6)
  (setv literal "")
  (for [i (range 6 (len bin_str) 5)]
    (setv literal (+ literal (cut bin_str (+ 1 i) (+ 5 i))))
    (when (= "0" (get bin_str i)) (break)))
  (, (+ i 5) (int literal 2)))

(defn parse_op [bin_str]
  (setv length_type (get bin_str 6))
  (if (= length_type "0")
    (do
      (setv length (int (cut bin_str 7 22) 2))
      (setv cursor 22)
      (setv end (+ length cursor))
      (setv packets [])
      (while (> end cursor)
        (setv [add_cursor packet]
              (parse_packet (cut bin_str cursor None)))
        (setv cursor (+ cursor add_cursor))
        (.append packets packet)))
    (do
      (setv length (int (cut bin_str 7 18) 2))
      (setv cursor 18)
      (setv packets [])
      (while (> length (len packets))
        (setv [add_cursor packet]
              (parse_packet (cut bin_str cursor None)))
        (setv cursor (+ cursor add_cursor))
        (.append packets packet))))
  (, cursor packets))

(setv ans_part1 0)

(defn parse_packet [bin_str]
  (setv ver (cut bin_str 0 3))
  (global ans_part1)
  (setv ans_part1 (+ ans_part1 (int ver 2)))
  (setv type_id (int (cut bin_str 3 6) 2))
  (if (= type_id 4)
    (parse_literal bin_str)
    (do
      (setv [cursor args] (parse_op bin_str))
      (, cursor
         (cond
           [(= type_id 0) (sum args)]
           [(= type_id 1) (functools.reduce (fn [a b] (* a b)) args)]
           [(= type_id 2) (min args)]
           [(= type_id 3) (max args)]
           [(= type_id 5) (int (> (get args 0) (get args 1)))]
           [(= type_id 6) (int (< (get args 0) (get args 1)))]
           [(= type_id 7) (int (= (get args 0) (get args 1)))])))))

(setv input (with [f (open "day16.txt")]
              (.read f)))
(setv bin_string (hex_to_bin input))

(parse_packet (hex_to_bin input))
(print "part 1: " ans_part1)

(print "part 2: " (get (parse_packet (hex_to_bin input)) 1))
