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
        [render.patterns.stripe]
        [render.patterns.checker]
        [render.patterns.gradient]
        [render.patterns.ring]
        [render.patterns.pattern]
        [canvas.canvas]
        [canvas.color]
        [la-math.vector]
        [la-math.matrix]))

(def material (make-material   (make-color 0.0274 0.85 0.85) 0.1 0.9 0.1 300 nil 10 0 0.01))
(def material-w  (make-material (make-color 1 0.9 0.9) 0.1 0.9 0.0 200 nil 0 0 1.0))
(def material-p  (make-material (make-color 1 0.9 0.9) 0.1 0.9 1 300 nil 0.0 0.0 1.0))
(def s-material  (make-material (make-color 0 0 0) 0.1 0.9 0.9 300 nil 1 1 1.52))
(def c-material  (make-material (make-color 0 1 0) 0.1 0.9 0.9 200 nil 0.0 0.0 1.0))

(def right-wall (set-transform (set-material (make-sphere) material-w) (m*m (translation 0 0 5)
                                                                         (m*m (rotation-y (/ Math/PI 4))
                                                                              (m*m (rotation-x (/ Math/PI 2))
                                                                                   (scaling 10 0.01 10))))))

(def left-wall (set-transform (set-material (make-sphere) material-w) (m*m (translation 0 0 5)
                                                                         (m*m (rotation-y (/ (* -1 Math/PI) 4))
                                                                              (m*m (rotation-x (/ Math/PI 2))
                                                                                   (scaling 10 0.01 10))))))

(def floor  (set-transform (set-material (make-sphere) material-w) (scaling 10 0.01 10)))
(def middle (set-transform (set-material (make-cylinder) c-material) (m*m (translation -0.5 1 0.5 ) (rotation-y (/ Math/PI 3)))))


(def world (make-world (make-light-point (make-point 0 15 -15) (make-color 1 1 1)) floor middle right-wall left-wall))
(def camera (set-camera-transform (make-camera 100 100 (/ Math/PI 3)) (make-view-transform (make-point 0 1.5 -5) (make-point 0 1 0) (make-vector 0 1 0))))

;; (make-point 0 5 -5) (make-point 0 -5 0)
;; translation 0 2 -1.5


(defn -main
  [& args]
  (save-canvas (render camera world) "scene-cube"))
