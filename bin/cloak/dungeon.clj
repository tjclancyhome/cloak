(ns cloak.dungeon
  (:require [cloak.util.grid :refer :all]
            [cloak.gui.core :refer :all]
            [taoensso.timbre :as timbre
             :refer (log  trace  debug  info  warn  error  fatal)]))

(set! *warn-on-reflection* true)

(defn open-space?
  [grid x y]
  (= (grid-cell grid x y) \.))

(defn update-grid-row
  [grid x y v]
  (loop [i 0
         n (count v)
         grid grid]
    (if (< i n)
      (recur (inc i)
             n
             (-> grid
                 (set-grid-cell (+ x i) y (nth v i))))
      grid)))

(defn create-room
  "This creates a room via the gui box method."
  [width height]
  (let [width (+ 2 width)
        height (+ 2 height)]
    (create-box width height double-line-frame)))

(defn add-random-door
  [room]
  (let [wall (inc (rand-int 4))
        width (grid-width room)
        height (grid-height room)
        wall-center (if (odd? wall)
                      (/ height 2)
                      (/ width 2))]
    (debug "wall:" wall "width:" width "height:" height "wall-center:" wall-center)
    (cond
     (= wall 1) (set-grid-cell room 0 wall-center \.)
     (= wall 2) (set-grid-cell room wall-center 0 \.)
     (= wall 3) (set-grid-cell room (dec width) wall-center \.)
     (= wall 4) (set-grid-cell room wall-center (dec height) \.))))

(defn generate-dungeon

  "This is hard-coded for now because I simply have no idea how to randomly generate a
  dungeon.

  This just creates a single room of size 32x10 and places it in the center of the
  game board grid."

  [width height]
  (let [game-grid (create-grid width height \.)
        game-grid-center (grid-center game-grid)
        room (add-random-door (create-room 32 10))
        room-width  (grid-width room)
        room-height (grid-height room)
        room-loc-x (- (first game-grid-center) (/ room-width 2))
        room-loc-y (- (second game-grid-center) (/ room-height 2))]
    (loop [i 0
           n (grid-height room)
           game-grid game-grid]
      (if (< i n)
        (recur (inc i)
             n
             (-> game-grid
                 (update-grid-row room-loc-x (+ i room-loc-y) (grid-row room i))))
        game-grid))))


