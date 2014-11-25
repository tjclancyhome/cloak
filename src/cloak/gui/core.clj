(ns cloak.gui.core
  (:require [cloak.symbols :refer :all]
            [cloak.util.grid :refer :all]
            [clojure.string :as str]))

(use 'clojure.pprint)

(set! *warn-on-reflection* false)

;;
;; Not sure about these yet.
;;
(defrecord Dimension [width height])
(defrecord Point [x y])
(defrecord Coord [x y])

;;
;; Note: The GameTiles idea won't work with boxes that
;; contain different characters to construct it (see
;; the frame definitions below).
;;
(defrecord GameTile [kind fgcolor bgcolor symb])
(def game-tiles
  {:floor        (GameTile. :floor :white :black \.)
   :stairs-up    (GameTile. :stairs-up :yellow :black \<)
   :stairs-down  (GameTile. :stairs-down :yellow :black \>)
   :wall         (GameTile. :wall :gray :black \#)}) ;

(defn make-tile
  ([kind fgcolor symb]
   (make-tile kind fgcolor :black symb))
  ([kind fgcolor bgcolor symb]
   (GameTile. kind fgcolor bgcolor symb)))

(defn tile->vec
  [tile]
  (vector (:symb tile) {:fg (:fgcolor tile) :bg (:bgcolor tile)}))

(def double-line-frame
  {:veritcal-edge double-line-vertical
   :horizontal-edge double-line-horizontal
   :top-left double-line-top-left-corner
   :top-right double-line-top-right-corner
   :bottom-left double-line-bottom-left-corner
   :bottom-right double-line-bottom-right-corner})

(def single-line-frame
  {:veritcal-edge single-line-vertical
   :horizontal-edge single-line-horizontal
   :top-left single-line-top-left-corner
   :top-right single-line-top-right-corner
   :bottom-left single-line-bottom-left-corner
   :bottom-right single-line-bottom-right-corner})

(def block-middle-frame
  {:veritcal-edge block-middle
   :horizontal-edge block-middle
   :top-left block-middle
   :top-right block-middle
   :bottom-left block-middle
   :bottom-right block-middle})

(def block-dense-frame
  {:veritcal-edge block-dense
   :horizontal-edge block-dense
   :top-left block-dense
   :top-right block-dense
   :bottom-left block-dense
   :bottom-right block-dense})

(def hash-frame
  {:veritcal-edge \#
   :horizontal-edge \#
   :top-left \#
   :top-right \#
   :bottom-left \#
   :bottom-right \#})

(defn box-line
  [len left middle right]
  (let [v []]
    (-> v
        (conj left)
        (into (repeat len middle))
        (conj right))))

(defn box-top
  [len frame-style]
  (box-line len
            (:top-left frame-style)
            (:horizontal-edge frame-style)
            (:top-right frame-style)))

(defn box-bottom
  [len frame-style]
  (box-line len
            (:bottom-left frame-style)
            (:horizontal-edge frame-style)
            (:bottom-right frame-style)))

(defn fill-body
  [box w h start-y frame-style]
  (if (< start-y h)
    (recur (-> box
               (assoc-in [start-y 0] (:veritcal-edge frame-style))
               (assoc-in [start-y (dec w)] (:veritcal-edge frame-style)))
           w h (inc start-y) frame-style)
    box))

(defn create-box
  ([w h]
   (create-box [w h single-line-frame]))
  ([w h frame-style]
  (let [box (create-grid [w h] \.)]
    (-> box
        (assoc 0 (box-top (- w 2) frame-style))
        (fill-body w (dec h) 1 frame-style)
        (assoc (dec h) (box-bottom (- w 2) frame-style))))))

(defn get-as-str
  [box y]
  (apply str (get box y)))

(defn box-to-str-vec
  [box]
  (let [h (count box)]
    (vec (for [y (range h)]
           (get-as-str box y)))))

;(def b (create-box 32 10 double-line-frame))
;(println b)
;(pprint (box-to-str-vec b))

(def tile (make-tile :floor :white \.))
(println (tile->vec tile))
(println (tile->vec (:wall game-tiles)))
