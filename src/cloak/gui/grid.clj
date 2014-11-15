(ns cloak.gui.grid)

(defn create-grid
  ([dimension fill-char]
   (create-grid (first dimension) (second dimension) fill-char))
  ([w h fill-char]
  (vec (repeat h (vec (repeat w fill-char))))))

(defn grid-cell
  [grid x y]
  (let [c (nth grid y)]
    (nth c x)))

(defn grid-row
  [grid r]
  (nth grid r))

(defn grid-center
  [grid]
  (let [w (count (first grid))
        h (count grid)]
    [(/ w 2) (/ h 2)]))

(defn update-grid-cell
  [grid x y v]
  (assoc-in grid [y x] v))

(defn set-grid-cell
  [grid x y v]
  (assoc-in grid [y x] v))

(defn grid-height
  [grid]
  (count grid))

(defn grid-width
  [grid]
  (count (first grid)))

(defn grid-dimensions
  [grid]
  [(count (first grid)) (count grid)])
