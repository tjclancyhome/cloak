(ns cloak.core
  (:require [cloak.symbols :refer :all]
            [cloak.util.grid :refer :all]
            [cloak.gui.core :refer :all]
            [cloak.dungeon :refer [generate-dungeon can-move?]]
            [clojure.string :refer [join]]
            [lanterna.screen :as s]
            [taoensso.timbre :as timbre
             :refer (log  trace  debug  info  warn  error  fatal)])
  (:gen-class))

(timbre/set-level! :debug)

(set! *warn-on-reflection* false)

;;
;; These can be used to diff against the heroe's current
;; position to get the new coords.
;;
(def direction {:n  [ 0 -1]
                :s  [ 0  1]
                :e  [ 1  0]
                :w  [-1  0]
                :nw [-1 -1]
                :ne [ 1 -1]
                :sw [-1  1]
                :se [ 1  1]})

;(defn draw-world
;  [game screen]
;  (let [grid (-> game :world :grid)
;        vs (grid->str-vec grid)
;        nrows (count grid)]
;    (dotimes[i nrows]
;      (s/put-string screen 0 i (get vs i)))))

;(defn draw-world2
;  [game screen]
;  (let [grid (-> game :world :grid)]
;    (s/put-sheet screen 0 0 grid)))

;;
;; after timing all three functions, this one turns out to be
;; the fastest.
;;
(defn draw-world
  [game screen]
  (let [grid (-> game :world :grid)
        nrows (count grid)]
    (dotimes[i nrows]
      (s/put-string screen 0 i (apply str (get grid i))))))

(defn get-input [game screen]
  (assoc game :input (s/get-key-blocking screen)))

(defn center-of [screen]
  (let [[cols rows] (s/get-size screen)]
    [(quot cols 2) (quot rows 2)]))

(defn update-location [game loc]
  (assoc-in game [:player :location] loc))

(defn move-player
  [game dir]
  (let [loc    (-> game :player :location)
        grid   (-> game :world :grid)
        newloc (map + loc dir)]
    (if (can-move? grid newloc)
      (update-location game newloc)
      game)))

(defn process-input [game]
  (let [input (:input game)]
    (dissoc game :input)
    (case input
      (\8 \k :up)     (move-player game (:n  direction))
      (\2 \j :down)   (move-player game (:s  direction))
      (\4 \h :left)   (move-player game (:w  direction))
      (\6 \l :right)  (move-player game (:e  direction))
      (\7 \y)         (move-player game (:nw direction))
      (\9 \u)         (move-player game (:ne direction))
      (\1 \b)         (move-player game (:sw direction))
      (\3 \n)         (move-player game (:se direction))
      :escape         (assoc-in game [:end-game] true)
      game)))

(defn draw-status-line [game screen]
  (let [hp (-> game :player :hp)
        max-hp (-> game :player :max-hp)
        level (-> game :player :level)
        [x y] (-> game :player :location)
        status (str "level: " level " hp: " hp "/" max-hp " location: (" x "," y ")")
        [_ rows] (s/get-size screen)]
    (s/put-string screen 0 (dec rows) status)))

(defn draw-player [location icon screen]
  (let [[x y] location]
    (trace "x:" x "y:" y)
    (s/put-string screen x y (str icon))
    (s/move-cursor screen x y)))

(defn- player-location [game]
  (-> game :player :location))

(defn- player-avatar [game]
  (-> game :player :avatar))

(defn update-gui [game screen]
  (trace "update-gui:" game)
  (draw-world game screen)
  (let [location (player-location game)
        icon     (player-avatar game)]
    (trace "location:" location "icon:" icon)
    (draw-player location icon screen)
    (draw-status-line game screen)
    (s/redraw screen))
  game)

(defn initialize-screen [screen]
  (s/start screen)
  (s/redraw screen))

(defn game-ended? [game]
  (get-in game [:end-game]))

(defn run [game screen]
  (update-gui game screen)
  (loop [game game]
    (when-not (game-ended? game)
      (recur
        (update-gui
          (process-input
            (get-input game screen)) screen)))))

(defn -main
  [& args]
  (let [screen (s/get-screen :swing {:cols 120 :rows 50})]
    (initialize-screen screen)
    (let [size (s/get-size screen)
          dungeon (generate-dungeon (first size) (second size))
          game {:world {:size size
                        :grid dungeon}
                :input nil
                :player {:location (center-of screen)
                         :avatar \@
                         :level 1
                         :max-hp 10
                         :hp 10}}]
      (s/in-screen screen
        (run game screen)))))

;(-main)
