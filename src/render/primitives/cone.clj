(ns render.primitives.cone
  (:use [render.primitives.shape]
        [render.comp.data-structures]
        [render.primitives.cylinder]
        [la-math.vector]))


(declare intersect-cone cone-normal-at)

(defrecord Cone [shape minimum maximum capped local-intersect local-normal-at])

(defn make-cone
  "make a cone"
  ([]
   (Cone. (make-shape) Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY false intersect-cone cone-normal-at))
  ([minimum maximum capped]
   (Cone. (make-shape) minimum maximum capped intersect-cone cone-normal-at)))

(defn set-cone-minimum
  "Set the minimum height of the cone"
  [cone minimum]
  (assoc cone :minimum minimum))

(defn set-cone-maximum
  "Set the maximum height of the cone"
  [cone maximum]
  (assoc cone :maximum maximum))

(defn set-cone-capped
  "Make the cone capped or not"
  [cone capped]
  (assoc cone :capped capped))

(defn- check-cap
  "Checks to see if the intersection at t is within a radius of 1"
  [ray t plane]
  (let [x (+ (x (:origin ray)) (* t (x (:direction ray))))
        z (+ (z (:origin ray)) (* t (z (:direction ray))))]
    (<= (+ (Math/pow x 2) (Math/pow z 2)) (Math/abs plane))))

(defn- intersect-caps
  "Intersect the ray on the caps if cone is capped"
  [cone ray xs]
  (if (not (capped? cone))
    xs
    (let [t1  (/ (- (:minimum cone) (y (:origin ray))) (y (:direction ray)))
          t2  (/ (- (:maximum cone) (y (:origin ray))) (y (:direction ray)))
          xs1 (if (check-cap ray t1 (:minimum cone)) (conj xs (make-intersection t1 cone)) xs)
          xs2 (if (check-cap ray t2 (:maximum cone)) (conj xs1 (make-intersection t2 cone)) xs1)]
      xs2)))

(defn- get-ts
  "Calculalte intersection t of a cone"
  [a b disc]
  (let [t0 (/ (- (* -1 b) (Math/sqrt disc)) (* 2 a))
        t1 (/ (+ (* -1 b) (Math/sqrt disc)) (* 2 a))]
    (if (> t0 t1)
      {:t0 t1 :t1 t0}
      {:t0 t0 :t1 t1})))

(defn- check-y
  "Check if intersection is in cone bounds"
  [cone ray y t xs]
  (if (and (< (:minimum cone) y)
           (< y (:maximum cone)))
    (conj xs (make-intersection t cone))
    xs))

(defn intersect-cone
  "Check if ray intersects with a cone"
  [cone ray]
  (let [a (+ (- (Math/pow (x (:direction ray)) 2)
                (Math/pow (y (:direction ray)) 2))
             (Math/pow (z (:direction ray)) 2))
        b (+ (- (* 2 (x (:origin ray)) (x (:direction ray)))
                (* 2 (y (:origin ray)) (y (:direction ray))))
             (* 2 (z (:origin ray)) (z (:direction ray))))
        c (+ (- (Math/pow (x (:origin ray)) 2)
                (Math/pow (y (:origin ray)) 2))
             (Math/pow (z (:origin ray)) 2))
        disc (- (Math/pow b 2) (* 4 a c))
        ts (get-ts a b disc)
        y0 (+ (y (:origin ray)) (* (:t0 ts) (y (:direction ray))))
        y1 (+ (y (:origin ray)) (* (:t1 ts) (y (:direction ray))))
        xs (check-y cone ray y1 (:t1 ts) (check-y cone ray y0 (:t0 ts) (make-intersections)))]
    (cond
      (and (= a 0.0) (not (= b 0.0))) (intersect-caps cone ray (make-intersections (make-intersection (/ (* -1 c) (* 2 b)) cone)))
      (< disc 0) []
      :else (intersect-caps cone ray xs))))

(defn cone-normal-at
  "Return a normal vector from a point in a cone"
  ^doubles
  [cone point]
  (let [dist (+ (Math/pow (x point) 2) (Math/pow (z point) 2))
        yt  (Math/sqrt (+ (Math/pow (x point) 2) (Math/pow (z point) 2)))
        yr (if (> (y point) 0) (* -1 yt) yt)]
    (cond
      (and (< dist 1) (>= (y point) (- (:maximum cone) 0.00001))) (make-vector 0 1 0)
      (and (< dist 1) (<= (y point) (+ (:minimum cone) 0.00001))) (make-vector 0 -1 0)
      :else (make-vector (x point) yr (z point)))))
