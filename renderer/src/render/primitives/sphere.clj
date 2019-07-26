(ns render.primitives.sphere
  (:use [render.primitives.shape]
        [render.data-structures]
        [la-math.vector]
        [la-math.matrix]
        [canvas.color]))

(declare intersect-sphere sphere-normal-at)

(defrecord Sphere [id shape center radii local-intersect local-normal-at])

(defn make-sphere
  "make a sphere"
  []
  (Sphere. "Something" (make-shape) (make-point 0 0 0) 1 intersect-sphere sphere-normal-at))

(defn s=
 "Compare two spheres"
  [s1 s2]
  (and (v=  (:center s1) (:center s2))
       (=   (:radii s1)  (:radii s2))
       (m=  (:transform (:shape s1)) (:transform (:shape s2)))
       (mt= (:material  (:shape s1)) (:material  (:shape s2)))))

(defn intersect-sphere
  "Check if ray intersects with a sphere"
  [sphere ray]
  (let [sphere-ray (v- (:origin ray) (make-point 0 0 0))
        a (dot (:direction ray) (:direction ray))
        b (* (dot (:direction ray) sphere-ray) 2)
        c (- (dot sphere-ray sphere-ray) 1)
        discriminant (- (Math/pow b 2) (* 4 a c))]
    (if (< discriminant 0)
      (make-intersections)
      (make-intersections (make-intersection (/ (- (* -1 b) (Math/sqrt discriminant)) (* 2 a)) sphere)
                          (make-intersection (/ (+ (* -1 b) (Math/sqrt discriminant)) (* 2 a)) sphere)))))

(defn sphere-normal-at
  "Return a normal vector from a point in a sphere"
  ^doubles
  [sphere point]
  (v- point (make-point  0 0 0)))
