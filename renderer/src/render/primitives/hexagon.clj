(ns render.primitives.hexagon
  (:use [la-math.matrix]
        [render.primitives.shape]
        [render.primitives.group]
        [render.primitives.sphere]
        [render.primitives.cylinder]))

(defn hexagon-corner
  "Make a corner of the hexagon"
  []
  (set-transform (make-sphere) (m*m (translation 0 0 -1) (scaling 0.25 0.25 0.25))))

(defn hexagon-edge
  "Make an edge of the hexagon"
  []
  (set-transform (make-cylinder 0 1 false) (m*m (translation 0 0 -1)
                                                (m*m (rotation-y (* -1 (/ Math/PI 6)))
                                                     (m*m (rotation-z (* -1 (/ Math/PI 2))) (scaling 0.25 1 0.25))))))
(defn hexagon-side
  "Make a side of the hexagon"
  []
  (let [side (make-group)]
    (do
      (add-child side (hexagon-corner))
      (add-child side (hexagon-edge))
      side)))

(defn make-hexagon
  "Make a hexagon"
  []
  (let [hex (make-group)]
    (loop [n 0]
      (if (= n 5) hex
        (do (add-child hex (set-transform (hexagon-side) (rotation-y (* n (/ Math/PI 3)))))
          (recur (inc n)))))))
