(ns cloak.dungeon
  (:require [cloak.gui.grid :refer :all]
            [cloak.gui.core :refer :all]
            [taoensso.timbre :as timbre
             :refer (log  trace  debug  info  warn  error  fatal)]))

(defn space-not-blocked?
  [grid x y]
  (= (grid-cell grid x y) \.))

(defn set-grid-row
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
  (create-box width height))

(defn generate-dungeon

  "This is hard-coded for now because I simply have no idea how to randomly generate a
  dungeon.

  This just creates a single room of size 32x10 and places it in the center of the
  game board grid."

  [width height]
  (let [game-grid (create-grid width height \space)
        game-grid-center (grid-center game-grid)
        room (create-room 32 10)
        room-loc-x (- (first game-grid-center) (/ 32 2))
        room-loc-y (- (second game-grid-center) (/ 10 2))]
    (loop [i 0
           n (grid-height room)
           game-grid game-grid]
      (if (< i n)
        (recur (inc i)
             n
             (-> game-grid
                 (set-grid-row room-loc-x (+ i room-loc-y) (grid-row room i))))
        game-grid))))

;(println (generate-dungeon 120 50))
