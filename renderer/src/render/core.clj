(ns render.core
  (:gen-class)
  (:use [render.alg.ray-tracer]
        [render.alg.scene]
        [render.comp.camera]
        [render.comp.world]
        [render.comp.data-structures]
        [render.primitives.shape]
        [render.primitives.sphere]
        [canvas.canvas]
        [canvas.color]
        [la-math.vector]
        [la-math.matrix]))

(def material (make-material (make-color 1 0.9 0.9) 0.1 0.9 0 200))
(def floor (set-transform (set-material (make-sphere) material) (scaling 10 0.01 10)))
(def left-wall (set-transform (set-material (make-sphere) material) (m*m (translation 0 0 5)
                                                                         (m*m (rotation-y (/ (* -1 Math/PI) 4))
                                                                              (m*m (rotation-x (/ Math/PI 2))
                                                                                   (scaling 10 0.01 10))))))

(def right-wall (set-transform (set-material (make-sphere) material) (m*m (translation 0 0 5)
                                                                         (m*m (rotation-y (/ Math/PI 4))
                                                                              (m*m (rotation-x (/ Math/PI 2))
                                                                                   (scaling 10 0.01 10))))))

(def middle (set-transform (set-material (make-sphere)
                                         (make-material (make-color 0.1 1 0.5) 0.1 0.7 0.3 200))
                           (translation -0.5 1 0.5)))

(def right (set-transform (set-material (make-sphere)
                                        (make-material (make-color 0.5 1 0.2) 0.1 0.7 0.3 200))
                          (m*m (translation 1.5 0.5 -0.5) (scaling 0.5 0.5 0.5))))

(def left (set-transform (set-material (make-sphere)
                                       (make-material (make-color 1 0.8 0.1) 0.1 0.7 0.3 200))
                         (m*m (translation -1.5 0.33 -0.75) (scaling 0.33 0.33 0.33))))

(def world (make-world (make-light-point (make-point -10 10 -10) (make-color 1 1 1)) floor left-wall right-wall middle right left))
(def camera (set-camera-transform (make-camera 500 500 (/ Math/PI 3)) (make-view-transform (make-point 0 1.5 -5) (make-point 0 1 0) (make-vector 0 1 0))))

(defn -main
  [& args]
  (save-canvas (render camera world) "scene-shadow-3o"))
