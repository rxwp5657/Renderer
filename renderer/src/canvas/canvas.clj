(ns canvas.canvas
  (:use [canvas.color]))

(defn make-canvas
  "Make a new canvas"
  [width height]
  {:width  width
   :height height
   :canvas (into-array (for [w (range width) h (range height)] (make-color 0 0 0)))})

(defn- get-coordinate
  "Map 2D index to 1D index"
  [w r c]
  (+ (* w r) c))

(defn get-pixel
  "Get the pixel at the r row and c column"
  [cv r c]
  (aget (:canvas cv) (get-coordinate (:width cv) r c)))

(defn write-pixel
  "Set pixel color"
  [cv r c color]
  (aset (:canvas cv) (get-coordinate (:width cv) r c) color))
