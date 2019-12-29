(ns aoc.day6
  (:require [clojure.string :as s]))

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
       (map #(s/split % #"\)"))))

; (defn adjacency-map
;   [s]
;   (->> s
;        (s/split-lines)
;        (map #(s/split % #"\)"))
;        (reduce (fn [coll, kv] (update coll (first kv) #(concat %1 (second kv)))) {})))

(defn tree-root
  [adj-list]
  (let [orbiter-set (->> adj-list
                         (map second)
                         (set))
        orbitee-set (->> adj-list
                         (map first)
                         (set))
        root-orbitee (->> orbitee-set
                          (filter #(not (contains? orbiter-set %1))))]
    (first root-orbitee)))

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

(defn do-part1
  []
  (let [adjacencies (adjacency-list (slurp "6-input.txt"))
        root (tree-root adjacencies)
        tree (->tree adjacencies root)]
    (sum-of-depths [tree])))
