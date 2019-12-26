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

(defn number-of-orbits
  ([tree & branches] (reduce + 0 (map number-of-orbits branches)))
  ([leaf] 1)
  ([] 0))