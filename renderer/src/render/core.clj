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
(def canvas-pixels 500)
(def pixel-size (/ wall-size canvas-pixels))
(def half (/ wall-size 2))

(def color  (make-color 1 0 0))
(def shape  (set-material (make-sphere) (set-color (make-material) (make-color 0 1 0))))

(def light-position (make-point -10 10 -10))
(def light-color (make-color 1 1 1))
(def light (make-light-point light-position light-color))

(defn get-circle-coordinates
  []
  (for [y (range (- canvas-pixels 1))
        x (range (- canvas-pixels 1))
        :let [world-y  (- half (* pixel-size y))
              world-x  (+ (* -1 half) (* pixel-size x))
              pos (make-point world-x world-y wall-z)
              r  (make-ray ray-origin (norm (v- pos ray-origin)))
              xs (intersect shape r)]
        :when (not (nil? (hit xs)))]
    {:x x :y y :color (lighting (:material (:object (hit xs)))
                                light
                                (position r (:t (hit xs)))
                                (neg (:direction r))
                                (normal-at (:object (hit xs)) (position r (:t (hit xs)))))}))


(defn -main
  [& args]
  (let [coordinates (get-circle-coordinates)]
    (loop [coordinate (first coordinates)
           rest-coordinates (rest coordinates)
           cv (make-canvas canvas-pixels canvas-pixels)]
      (if (empty? rest-coordinates)
        (save-canvas cv "circle")
        (do
          (write-pixel cv (:x coordinate) (:y coordinate) (:color coordinate))
          (recur (first rest-coordinates) (rest rest-coordinates) cv))))))
