(ns render.core
  (:gen-class)
  (:use [render.ray-tracer]
        [canvas.canvas]
        [canvas.color]
        [la-math.vector]
        [la-math.matrix]))

(def ray-origin (make-point 0 0 -5))
(def wall-z 10)
(def wall-size 7)
(def canvas-pixels 100)
(def pixel-size (/ wall-size canvas-pixels))
(def half (/ wall-size 2))

(def color  (make-color 1 0 0))
(def shape  (make-sphere))

(defn get-circle-coordinates
  []
  (for [y (range (- canvas-pixels 1))
        x (range (- canvas-pixels 1))
        :let [world-y  (- half (* pixel-size y))
              world-x  (+ (* -1 half) (* pixel-size x))
              position (make-point world-x world-y wall-z)
              r  (make-ray ray-origin (norm (v- position ray-origin)))
              xs (intersect shape r)]
        :when (not (nil? (hit xs)))]
    {:x x :y y}))

(defn -main
  [& args]
  (let [coordinates (get-circle-coordinates)]
    (loop [coordinate (first coordinates)
           rest-coordinates (rest coordinates)
           cv (make-canvas canvas-pixels canvas-pixels)]
      (if (empty? rest-coordinates)
        (save-canvas cv "circle")
        (do
          (write-pixel cv (:x coordinate) (:y coordinate) color)
          (recur (first rest-coordinates) (rest rest-coordinates) cv))))))
