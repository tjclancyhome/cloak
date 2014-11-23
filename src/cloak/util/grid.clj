(ns cloak.util.grid
  (:require [taoensso.timbre :as timbre
             :refer (log  trace  debug  info  warn  error  fatal)]))

(defn create-grid
  ([[w h]]
   (create-grid w h \space))
  ([[w h] init-val]
   (vec (repeat h (vec (repeat w init-val))))))

(defn grid?
  [grid]
  (and (vector? grid) (vector? (first grid))))

(defn grid-height
  [grid]
  (count grid))

(defn grid-width
  [grid]
  (count (first grid)))

(defn grid-dimensions
  "Return as [width height]."
  [grid]
  [(grid-width grid) (grid-height grid)])

(defn grid-cell
  ([grid loc]
   (grid-cell grid (first loc) (second loc)))
  ([grid x y]
  (let [c (get grid y)]
    (get c x))))

(defn grid-row
  [grid r]
  (get grid r))

(defn grid-column
  [grid c]
  (map (fn [row]
         (get row c)) grid))

(defn grid-center
  [grid]
  (let [[w h] (grid-dimensions grid)]
    [(quot w 2) (quot h 2)]))

(defn set-grid-cell
  [grid x y v]
  (assoc-in grid [y x] v))

(defn set-grid-row
  [grid row coll]
  (if (= (count coll) (grid-width grid))
    (assoc-in grid [row] coll)))

(defn set-grid-column
  [grid column coll]
  (if (= (count coll) (grid-height grid))
    (let [n (count coll)]
      (loop [i 0
             grid grid]
        (if (< i n)
          (recur (inc i) (set-grid-cell grid column i (get coll i)))
          grid)))))

(defn grid->str-vec
  [grid]
  (vec (for [row grid]
         (apply str row))))

(defn grid-print
  [grid]
  (doseq [r grid]
    (println (apply str r))))

;;
;; some example code
;;
(comment
  (def grid (create-grid [5 5] \.))
  (def coll [\a \b \c \d \e])
  (grid-print (set-grid-row grid 1 coll))
  (println)
  (grid-print (set-grid-column grid 1 coll))
  (println)
  (println (grid? grid))
  (println)
  (println (time (grid->str-vec grid)))
)
