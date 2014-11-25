(ns cloak.old-dungeon
  (:require [cloak.util.grid :refer :all]
            [cloak.gui.core :refer :all]
            [taoensso.timbre :as timbre
             :refer (log  trace  debug  info  warn  error  fatal)]))

(set! *warn-on-reflection* false)

(def empty-space \.)

(defn can-move?
  [grid loc]
  (= (grid-cell grid loc) empty-space))

(defn update-grid-row
  [grid x y v]
  (let [n (count v)]
    (loop [i 0
           grid grid]  ;; todo: find a better way than using the loop form
      (if (< i n)
        (recur (inc i) (set-grid-cell grid (+ x i) y (nth v i)))
        grid))))

(defn add-random-door
  [room]
  (let [wall (inc (rand-int 4))
        width (grid-width room)
        height (grid-height room)
        wall-center (if (odd? wall) (quot height 2) (quot width 2))]
    (debug "wall:" wall "width:" width "height:" height "wall-center:" wall-center)
    (cond
     (= wall 1) (set-grid-cell room 0 wall-center empty-space)
     (= wall 2) (set-grid-cell room wall-center 0 empty-space)
     (= wall 3) (set-grid-cell room (dec width) wall-center empty-space)
     (= wall 4) (set-grid-cell room wall-center (dec height) empty-space))))

(defn create-room
  "This creates a room via the gui box method."
  [width height]
  (let [width (+ 2 width)
        height (+ 2 height)]
    (add-random-door (create-box width height single-line-frame))))

(defn generate-dungeon

  "This is hard-coded for now because I simply have no idea how to randomly generate a
  dungeon.

  This just creates a single room of size 32x10 and places it in the center of the
  game board grid."

  [width height]
  (let [game-grid (create-grid [width height] \.)
        [game-width game-height] (grid-center game-grid)
        room (create-room 12 6)
        room-width  (grid-width room)
        room-height (grid-height room)
        room-loc-x (- game-width  (quot room-width 2))
        room-loc-y (- game-height (quot room-height 2))]
    (loop [i 0
           game-grid game-grid]
      (if (< i room-height)
        (recur (inc i) (update-grid-row game-grid room-loc-x (+ i room-loc-y) (grid-row room i)))
        game-grid))))


;(println (generate-dungeon 100 49))
