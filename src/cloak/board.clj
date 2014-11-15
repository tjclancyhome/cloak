(ns cloak.board)

(defn create-grid
  ([size c]
   (create-grid (first size) (second size) c))
  ([w h c]
  (vec (repeat h (vec (repeat w c))))))

(defn grid-at
  [grid x y]
  (let [c (nth grid x)]
    (nth c y)))

(defn grid-row
  [grid r]
  (nth grid r))
