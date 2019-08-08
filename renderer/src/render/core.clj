(ns render.core
  (:gen-class)
  (:use [render.alg.ray-tracer]
        [render.alg.scene]
        [render.comp.camera]
        [render.comp.world]
        [render.comp.data-structures]
        [render.primitives.shape]
        [render.primitives.sphere]
        [render.primitives.cube]
        [render.primitives.cylinder]
        [render.primitives.plane]
        [render.primitives.hexagon]
        [render.patterns.stripe]
        [render.patterns.checker]
        [render.patterns.gradient]
        [render.patterns.ring]
        [render.patterns.pattern]
        [canvas.canvas]
        [canvas.color]
        [la-math.vector]
        [la-math.matrix]
        [parser.parser]))

;; "/Users/charles/Desktop"

(def parser (parse-obj-file "/Users/charles/Desktop/tea.txt"))
(def tea (obj-to-group parser))
(def world   (make-world (make-light-point (make-point 0 0 0) (make-color 1 1 1)) (set-transform tea (translation -0.5 1 0.5))))
(def camera  (set-camera-transform (make-camera 50 50 (/ Math/PI 3)) (make-view-transform (make-point 0 1.5 -65) (make-point 0 1 0) (make-vector 0 1 0))))

;; (make-point 0 5 -5) (make-point 0 -5 0)
;; translation 0 2 -1.5


(defn -main
  [& args]
  (save-canvas (render camera world) "tea-pod"))
