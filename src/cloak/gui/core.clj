(ns cloak.gui.core
  (:require [cloak.symbols :refer :all]
            [cloak.gui.grid :refer :all]
            [clojure.string :as str]))

(use 'clojure.pprint)

;;
;; Not sure about these yet.
;;
(defrecord Dimension [width height])
(defrecord Point [x y])


(defn box-line
  [len left middle right]
  (let [v []]
    (-> v
        (conj left)
        (into (repeat len middle))
        (conj right))))

(defn box-top
  [len]
  (box-line len double-line-top-left-corner double-line-horizontal double-line-top-right-corner))

(defn box-bottom
  [len]
  (box-line len double-line-bottom-left-corner double-line-horizontal double-line-bottom-right-corner))

(defn fill-body
  [box w h start-y]
  (if (< start-y h)
    (recur (-> box
               (assoc-in [start-y 0] double-line-vertical)
               (assoc-in [start-y (dec w)] double-line-vertical))
           w h (inc start-y))
    box))

(defn create-box
  [w h]
  (let [box (create-grid w h \.)]
    (-> box
        (assoc 0 (box-top (- w 2)))
        (fill-body w (dec h) 1)
        (assoc (dec h) (box-bottom (- w 2))))))

(defn get-as-str
  [box y]
  (apply str (get box y)))

(defn box-to-str-vec
  [box]
  (let [h (count box)]
    (vec (for [y (range h)]
           (get-as-str box y)))))

;(def b (create-box 32 10))
;(println b)
;(pprint (box-to-str-vec b))
