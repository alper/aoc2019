(ns aoc.day6
  (:require [clojure.string :as s])
  )

; Part 1

(def sample "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

(def sample-answer 42)

(defn adjacency-list
  [s]
  (->> s
       (s/split-lines)
       (map #(s/split % #"\)")))
  )

(defn tree-descendants
  [adj-list val]
  (->> adj-list
       (filter #(= val (first %)))
       (map second)))

(defn ->leaf
  [val]
  [val])

(defn ->branch
  [val children]
  [val children])

(defn ->tree
  [adj-list val]
  (let [->tree' (partial ->tree adj-list)
        kids (tree-descendants adj-list val)]
    (if-not (empty? kids)
      (->branch val (map ->tree' kids))
      (->leaf val))))

; (defn number-of-orbits
;   ([tree] (number-of-orbits tree 0))
;   ([tree orbits] (let [[head & rest :as tree] tree, orbits orbits]
;                    (cond 
;                      (empty? tree) orbits
;                      (empty? rest) orbits
;                      (seq? rest) (reduce + (map number-of-orbits rest (inc orbits)))))))

; (defn sum-of-depths
;   [tree cur-depth] (let [[head & rest] tree]
;                      (if (empty? rest)
;                        cur-depth
;                        (reduce + cur-depth (map (partial ) rest)))))


(defn sum-of-depths
  [branches]
  (loop [branches branches
         cur-depth 0
         total-depth 0]
    (cond
      (empty? branches) total-depth
      :else (recur
             (mapcat (fn [node] (second node)) branches)
             (inc cur-depth)
             (+ total-depth (* (count branches) cur-depth))))))