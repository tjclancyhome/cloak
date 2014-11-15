(ns cloak.core
  (:require [cloak.symbols :refer :all]
            [clojure.string :refer [join]]
            [lanterna.screen :as s]
            [taoensso.timbre :as timbre
             :refer (log  trace  debug  info  warn  error  fatal  report
                   logf tracef debugf infof warnf errorf fatalf reportf
                   spy logged-future with-log-level with-logging-config
                   sometimes)])
  (:gen-class))

(use 'clojure.pprint)

(defn world [game]
  (:world game))

(defn draw-world
  "This is very bad. I create a string called row by simply
   repeating and joining a string with the value of a period."
  [game screen]
  (let [[w h] (s/get-size screen)
        row (join "" (take w (repeat ".")))]
    (loop [i 0]
      (if-not (= i h)
        (do
          (s/put-string screen 0 i row)
          (recur (inc i)))))))

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
    (trace "x:" x "y:" y)
    (s/put-string screen x y (str icon))
    (trace "attempted to put icon onto the screen")))

(defn- player-location [game]
  (-> game :player :location))

(defn- player-icon [game]
  (-> game :player :player-icon))

(defn update-gui [game screen]
  (trace "update-gui:" game)
  ;(s/clear screen)
  (draw-world game screen)
  (let [location (player-location game)
        icon     (player-icon game)]
    (trace "location:" location "icon:" icon)
    (draw-player location icon screen)
    (s/redraw screen)
    ;; this has to happen after the screen is redrawn.
    (s/hide-cursor screen))
  game)

(defn initialize-screen [screen]
  (s/start screen)
  (s/redraw screen)
  (s/hide-cursor screen))

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
  (let [screen (s/get-screen :swing {:cols 100 :rows 40})]
    (initialize-screen screen)
    (let [[x y] (center-of screen)
          game {:world {:size (s/get-size screen)}
                :input nil
                :player {:location [x y]
                         :player-icon \@}}]
      (run game screen))
    (s/stop screen)))

