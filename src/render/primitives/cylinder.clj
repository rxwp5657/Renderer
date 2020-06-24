(ns render.primitives.cylinder
  (:use [render.primitives.shape]
        [render.comp.data-structures]
        [la-math.vector]))


(declare intersect-cylinder cylinder-normal-at)

(defrecord Cylinder [shape minimum maximum capped local-intersect local-normal-at])

(defn make-cylinder
  "make a cylinder"
  ([]
   (Cylinder. (make-shape) Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY false intersect-cylinder cylinder-normal-at))
  ([minimum maximum capped]
   (Cylinder. (make-shape) minimum maximum capped intersect-cylinder cylinder-normal-at)))

(defn set-cylinder-minimum
  "Set the minimum height of the cylinder"
  [cylinder minimum]
  (assoc cylinder :minimum minimum))

(defn set-cylinder-maximum
  "Set the maximum height of the cylinder"
  [cylinder maximum]
  (assoc cylinder :maximum maximum))

(defn set-cylinder-capped
  "Make the cylinder capped or not"
  [cylinder capped]
  (assoc cylinder :capped capped))

(defn capped?
  "Is cylinder capped?"
  [cylinder]
  (:capped cylinder))

(defn- check-cap
  "Checks to see if the intersection at t is within a radius of 1"
  [ray t]
  (let [x (+ (x (:origin ray)) (* t (x (:direction ray))))
        z (+ (z (:origin ray)) (* t (z (:direction ray))))]
    (<= (+ (Math/pow x 2) (Math/pow z 2)) 1)))

(defn- intersect-caps
  "Intersect the ray on the caps if cylinder is capped"
  [cylinder ray xs]
  (if (not (capped? cylinder))
    xs
    (let [t1  (/ (- (:minimum cylinder) (y (:origin ray))) (y (:direction ray)))
          t2  (/ (- (:maximum cylinder) (y (:origin ray))) (y (:direction ray)))
          xs1 (if (check-cap ray t1) (conj xs (make-intersection t1 cylinder)) xs)
          xs2 (if (check-cap ray t2) (conj xs1 (make-intersection t2 cylinder)) xs1)]
      xs2)))

(defn- get-ts
  "Calculalte intersection t of a cylinder"
  [a b disc]
  (let [t0 (/ (- (* -1 b) (Math/sqrt disc)) (* 2 a))
        t1 (/ (+ (* -1 b) (Math/sqrt disc)) (* 2 a))]
    (if (> t0 t1)
      {:t0 t1 :t1 t0}
      {:t0 t0 :t1 t1})))

(defn- check-y
  "Check if intersection is in cylinder bounds"
  [cylinder ray y t xs]
  (if (and (< (:minimum cylinder) y)
           (< y (:maximum cylinder)))
    (conj xs (make-intersection t cylinder))
    xs))

(defn intersect-cylinder
  "Check if ray intersects with a cylinder"
  [cylinder ray]
  (let [a (+ (Math/pow (x (:direction ray)) 2) (Math/pow (z (:direction ray)) 2))
        b (+ (* 2 (x (:origin ray)) (x (:direction ray)))
             (* 2 (z (:origin ray)) (z (:direction ray))))
        c (- (+ (Math/pow (x (:origin ray)) 2) (Math/pow (z (:origin ray)) 2)) 1)
        disc (- (Math/pow b 2) (* 4 a c))
        ts (get-ts a b disc)
        y0 (+ (y (:origin ray)) (* (:t0 ts) (y (:direction ray))))
        y1 (+ (y (:origin ray)) (* (:t1 ts) (y (:direction ray))))
        xs (check-y cylinder ray y1 (:t1 ts) (check-y cylinder ray y0 (:t0 ts) (make-intersections)))]
    (cond
      (< disc 0) []
      :else (intersect-caps cylinder ray xs))))

(defn cylinder-normal-at
  "Return a normal vector from a point in a cylinder"
  ^doubles
  [cylinder point]
  (let [dist (+ (Math/pow (x point) 2) (Math/pow (z point) 2))]
    (cond
      (and (< dist 1) (>= (y point) (- (:maximum cylinder) 0.00001))) (make-vector 0 1 0)
      (and (< dist 1) (<= (y point) (+ (:minimum cylinder) 0.00001))) (make-vector 0 -1 0)
      :else (make-vector (x point) 0 (z point)))))
