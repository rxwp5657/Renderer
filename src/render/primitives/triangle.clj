(ns render.primitives.triangle
  (:use [la-math.vector]
        [la-math.matrix]
        [render.alg.ray-tracer]
        [render.comp.data-structures]
        [render.primitives.shape]))

(declare triangle-normal-at triangle-intersect)

(defrecord Triangle [shape p1 p2 p3 e1 e2 normal local-normal-at local-intersect])

(defn make-triangle
  "Make a triangle"
  [p1 p2 p3]
  (let [e1 (v- p2 p1)
        e2 (v- p3 p1)]
    (Triangle. (make-shape) p1 p2 p3 e1 e2 (norm (cross e2 e1)) triangle-normal-at triangle-intersect)))

(defn triangle-normal-at
  "Return the normal of a triangle"
  [triangle point]
  (:normal triangle))

(defn triangle-intersect
  "Triangle - ray intersection"
  [triangle ray]
  (let [dir-cross-e2 (cross (:direction ray) (:e2 triangle))
        det (dot (:e1 triangle) dir-cross-e2)
        f (/ 1.0 det)
        p1-to-origin (v- (:origin ray) (:p1 triangle))
        u (* f (dot p1-to-origin dir-cross-e2))
        origin-cross-e1 (cross p1-to-origin (:e1 triangle))
        v (* f (dot (:direction ray) origin-cross-e1))
        t (* f (dot (:e2 triangle) origin-cross-e1))]
    (cond
      (< (Math/abs det) 0.00001) []
      (or (< u 0) (> u 1)) []
      (or (< v 0) (> (+ u v) 1)) []
      :else (make-intersections (make-intersection t triangle)))))
