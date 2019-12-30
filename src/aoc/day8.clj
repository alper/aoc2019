(ns aoc.day8
  (:require [clojure.string :as s]
            [clojure.set]))

; Part 1

(def sample-data "123456789012")

(def width 25)
(def height 6)

(defn number-of
  [c s]
  (count (filter #(= % c) s)))

(defn result-mult
  [s]
  (* 
   (number-of \1 s) 
   (number-of \2 s)))

(defn do-part1
  []
  (->> (slurp "8-input.txt")
       (partition (* width height))
       (sort #(compare (number-of \0 %1) (number-of \0 %2)))
       (first)
       (result-mult)))

; => 1072

; part 2