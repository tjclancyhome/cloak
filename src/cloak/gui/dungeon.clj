(ns cloak.gui.dungeon
  (:require [cloak.util.grid :refer :all]
            [cloak.symbols :refer :all]
            [lanterna.screen :as s]
            [taoensso.timbre :as timbre
             :refer (log  trace  debug  info  warn  error  fatal)]))

;;
;; Redoing the dungeon module and placing it in cloak.gui namespace
;; because it will define the dungeon drawing and generation code.
;;

;;
;; Creating a tile structure similar to Caves but with more attributes
;; that I will describe soon.
;;

(defrecord Tile [kind fgcolor bgcolor symb seen])

(defn create-tile
  "All tiles when initially created are not seen hence
   no funtion parameter for the seen key."
  ([kind fgcolor symb]
   (create-tile kind fgcolor :black symb))
  ([kind fgcolor bgcolor symb]
   (Tile. kind fgcolor bgcolor symb false)))

(def tiles
  {:floor  (create-tile :floor :white  \.)
   :up     (create-tile :up    :yellow \<)
   :down   (create-tile :down  :yellow \>)
   :path   (create-tile :path  :gray   (:block-sparse symbol-map))
   :wall   (create-tile :wall  :gray   \#)
   :void   (create-tile :void  :black  \space)})

(defn tile->vec
  "This method will output create a vector that contains
   the character symbol of the tile and a map of options
   that describe the folloing attributes of the tile:

   :fg   - foreground color
   :bg   - background color
   :seen - whether the tile has been seen by our intrepid
           adventurer.

   This vector will be used for drawing each of the tiles
   of the game board (dungeon).
  "
  [tile]
  (vector (:symb tile) {:fg (:fgcolor tile) :bg (:bgcolor tile) :seen (:seen tile)}))

(defn screen-center
  "Returns the center coordinates of the screen as
   a vector: [x y]"
  [screen]
  (let [[cols rows] (s/get-size screen)]
    [(quot cols 2) (quot rows 2)]))

(defn create-dungeon
  "Create an empty dungeon using the :void tile, which is just
   a space."
  [w h]
  (create-grid [w h] (:path tiles)))

(defn create-room
  [w h]
  (create-grid [w h] (:floor tiles)))

(defn add-room
  [dungeon room x y]
  (grid-place-grid dungeon room x y))

(defn render-dungeon
  [grid]
  (map-grid tile->vec grid))

(defn center-viewport
  [dungeon screen]
  (let [[x y] (grid-center dungeon)
        [cols rows] (s/get-size screen)
        start-x (Math/abs (- x (quot cols 2)))
        start-y (Math/abs (- y (quot rows 2)))]
    (debug "x:" x "y:" y "start-x:" start-x "start-y:" start-y)
    (sub-grid dungeon start-x start-y cols rows)))

;;
;; Some sample code.
;;

(defn create-small-dungeon
  [w h]
  (let [dungeon (create-dungeon w h)
        [cx cy] (grid-center dungeon)
        x (- cx 6)
        y (- cy 3)]
    (debug "w:" w "h:" h "cx:" cx "cy:" cy "x:" x "y:" y)
    (set-grid-cell (set-grid-cell (add-room dungeon (create-room 12 6) x y) x y (:up tiles)) 30 25 (:void tiles))))

(def small-dungeon (create-small-dungeon 90 50))

;(println (map-grid (fn [cell] (:symb cell)) small-dungeon))

(defn show-dungeon
  [d]
  (let [screen (s/get-screen :swing {:cols 80 :rows 40})]
    (s/start screen)
    (center-viewport d screen)
    (s/put-sheet screen 0 0 (render-dungeon d))
    (s/redraw screen)))

(show-dungeon small-dungeon)
