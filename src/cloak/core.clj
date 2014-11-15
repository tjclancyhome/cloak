(ns cloak.core
  (:require [cloak.symbols :refer :all]
            [cloak.board :refer :all]
            [cloak.gui.core :refer :all]
            [clojure.string :refer [join]]
            [lanterna.screen :as s]
            [taoensso.timbre :as timbre
             :refer (log  trace  debug  info  warn  error  fatal)])
  (:gen-class))

(timbre/set-level! :debug)

(defn world [game]
  (:world game))

(defn draw-room
  [room screen]
  (let [box (nth room 2)
        y (nth room 1)
        x (nth room 0)
        rows (count box)]
    (dotimes [i rows]
      (s/put-string screen x (+ i y) (get-as-str box i)))))

(defn draw-world
  [game screen]
  (let [grid (-> game :world :grid)
        rows (count grid)]
    (dotimes[i rows]
      (s/put-string screen 0 i (apply str (nth grid i))))
    (let [rooms (-> game :world :rooms)
          n (count rooms)]
      (debug (str "rooms: " n))
      (dotimes [i n]
        (draw-room (nth rooms i) screen)))))

(defn get-input [game screen]
  (assoc game :input (s/get-key-blocking screen)))

(defn center-of [screen]
  (let [[cols rows] (s/get-size screen)]
    [(/ cols 2) (/ rows 2)]))

(defn update-location [game x y]
  (assoc-in game [:player :location] [x y]))

(defn move-player [game dir]
  (let [[x y] (-> game :player :location)]
    (case dir
      :up    (update-location game x (dec y))
      :down  (update-location game x (inc y))
      :left  (update-location game (dec x) y)
      :right (update-location game (inc x) y))))

(defn process-input [game]
  (let [input (:input game)]
    (dissoc game :input)
    (case input
      :up     (move-player game :up)
      :down   (move-player game :down)
      :left   (move-player game :left)
      :right  (move-player game :right)
      \q      (assoc-in game [:end-game] true)
      game)))

(defn draw-player [location icon screen]
  (let [[x y] location]
    (debug "x:" x "y:" y)
    (s/put-string screen x y (str icon))
    (s/move-cursor screen x y)))

(defn- player-location [game]
  (-> game :player :location))

(defn- player-avatar [game]
  (-> game :player :avatar))

(defn update-gui [game screen]
  (debug "update-gui:" game)
  ;(s/clear screen)
  (draw-world game screen)
  (let [location (player-location game)
        icon     (player-avatar game)]
    (debug "location:" location "icon:" icon)
    (draw-player location icon screen)
    (s/redraw screen))
  game)

(defn initialize-screen [screen]
  (s/start screen)
  (s/redraw screen))

(defn game-ended? [game]
  (get-in game [:end-game]))

(defn run [game screen]
  (update-gui game screen)
  (debug game)
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
          game {:world {:size size
                        :grid (create-grid size \.)
                        :rooms [[5 5 (create-box 32 10)]]}
                :input nil
                :player {:location (center-of screen)
                         :avatar \@
                         :level 1
                         :hp 10}}]
      (s/in-screen screen
        (run game screen)))))

(-main)
