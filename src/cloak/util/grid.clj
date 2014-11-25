(ns cloak.util.grid
  (:require [taoensso.timbre :as timbre
             :refer (log  trace  debug  info  warn  error  fatal)]))
;;
;; To me this is really elegant.  I would have been looping
;; and doing all sort of bad stuff.
;;
(defn slice
  "Borrowed this from Caves."
  [grid start width]
  (->> grid
       (drop start)
       (take width)))

;;
;; In caves there is a utility function called shear.
;; Here I just call it a sub-grid of a grid.
;;
(defn sub-grid
  "Get a subview of a grid."
  [grid start-x start-y width height]
  (map #(slice % start-x width)
        (slice grid start-y height)))

;;
;; Copied this also from Caves. Original function name is map2d.
;;
(defn map-grid
  "Map a function over a grid (a 2d sequence)."
  [f grid]
  (map (partial map f) grid))

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
  "Returns the value of a cell at [x y]."
  ([grid loc]
   (grid-cell grid (first loc) (second loc)))
  ([grid x y]
  (let [c (get grid y)]
    (get c x))))

(defn grid-row
  "Returns row r from the grid."
  [grid r]
  (get grid r))

(defn grid-column
  "Returns the values in a column as a vector."
  [grid c]
  (vec (map (fn [row]
         (get row c)) grid)))

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

(defn grid-update-row
  [grid x y coll]
  (let [n (count coll)]
    (loop [i 0
           grid grid]  ;; todo: find a better way than using the loop form
      (if (< i n)
        (recur (inc i) (set-grid-cell grid (+ x i) y (get coll i)))
        grid))))

(defn grid-place-grid
  [grid other-grid x y]
  (debug "x:" x "y:" y)
  (let [rows (grid-height other-grid)]
    (loop [i 0
           grid grid]
      (if (< i rows)
        (recur (inc i) (grid-update-row grid x (+ i y) (grid-row other-grid i)))
        grid))))

(defn grid->str-vec
  "This takes the entire grid and converts each cell to
  a simple string and returns a vector of strings where
  each string is a compact form of the contents of a row."
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
  (def g (create-grid [20 20] \.))
  (println g)
  (println (sub-grid g 5 5 5 5))
  (def grid (create-grid [5 5] \.))
  (def coll [\a \b \c \d \e])
  (grid-print (set-grid-row grid 1 coll))
  (println)
  (grid-print (set-grid-column grid 1 coll))
  (println)
  (println (grid? grid))
  (println)
  (println (time (grid->str-vec grid)))
  (println)
  (println (time (grid-column grid 1)))
  (println)
  (println (time (grid-row grid 1)))
)
