(ns render.ray-tracer
  (:use [la-math.vector]
        [la-math.matrix]))

(defn make-ray
  "Create a ray given its position point and direction vector"
  [^doubles o ^doubles p]
  {:origin o :direction p})

(defn position
  "Return new ray position over time t"
  ^doubles
  [ray ^double t]
  (v+ (:origin ray) (v* (:direction ray) t)))

(defn make-intersection
  "Make a intersection data structure"
  [t obj]
  {:t t :object obj})

(defn make-intersections
  "Make an aggregate collection of intersections"
  [& intersections]
  (vec (sort-by :t intersections)))

(defn ray-transform
  "Apply the transformation to the ray"
  [ray transform]
  (make-ray (m*v transform (:origin ray))
            (m*v transform (:direction ray))))

(defn intersect
  "Check if ray intersects with a sphere"
  [sphere ray]
  (let [ray2 (ray-transform ray (inverse (:transform sphere)))
        sphere-ray (v- (:origin ray2) (make-point 0 0 0))
        a (dot (:direction ray2) (:direction ray2))
        b (* (dot (:direction ray2) sphere-ray) 2)
        c (- (dot sphere-ray sphere-ray) 1)
        discriminant (- (Math/pow b 2) (* 4 a c))]
    (if (< discriminant 0)
      (make-intersections)
      (make-intersections (make-intersection (/ (- (* -1 b) (Math/sqrt discriminant)) (* 2 a)) sphere)
                          (make-intersection (/ (+ (* -1 b) (Math/sqrt discriminant)) (* 2 a)) sphere)))))

(defn hit
  "Return the smallest non negative intersection"
  [intersections]
  (loop [val  (first intersections)
         r    (rest intersections)]
    (cond
      (and (not (nil? val)) (> (:t val) 0)) val
      (not (nil? val)) (recur (first r) (rest r))
      :else nil)))

(defn s=
  "Compare two spheres"
  [s1 s2]
  (= (:id s1) (:id s2)))

(defn make-sphere
  "make a sphere"
  []
  {:id "Something" :center (make-point 0 0 0) :radii 1 :transform (identity-m)})

(defn set-transform
  [obj transform]
  {:id (:id obj) :center (:center obj) :radii (:radii obj) :transform transform})
