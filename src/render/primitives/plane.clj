(ns render.primitives.plane
  (:use [render.primitives.shape]
        [render.comp.data-structures]
        [la-math.vector]))

(declare plane-normal-at intersect-plane)
(defrecord Plane [shape local-normal-at local-intersect])

(defn make-plane
 "Make a plane object"
 []
 (Plane. (make-shape) plane-normal-at intersect-plane))

(defn intersect-plane
  "Plane - ray intersect"
  [plane ray]
  (let [t (/ (y (neg (:origin ray))) (y (:direction ray)))]
    (if (< (Math/abs (* -1 (y (:direction ray)))) 0.00001)
      []
      (make-intersections (make-intersection t plane)))))

(defn plane-normal-at
  "Return normal vector of a plane"
  [plane point]
  (make-vector 0 1 0))
