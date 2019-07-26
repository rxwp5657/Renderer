(ns render.ray-tracer
  (:use [render.data-structures]
        [la-math.vector]
        [la-math.matrix]
        [canvas.color]))

(defn position
  "Return new ray position over time t"
  ^doubles
  [ray ^double t]
  (v+ (:origin ray) (v* (:direction ray) t)))

(defn ray-transform
  "Apply the transformation to the ray"
  [ray transform]
  (make-ray (m*v transform (:origin ray))
            (m*v transform (:direction ray))))

(defn intersect
  "Check if ray intersects with shape"
  [shape ray]
  (let [local-ray (ray-transform ray (inverse (:transform (:shape shape))))]
    ((:local-intersect shape) shape local-ray)))

(defn hit
  "Return the smallest non negative intersection"
  [intersections]
  (loop [val  (first intersections)
         r    (rest intersections)]
    (cond
      (and (not (nil? val)) (> (:t val) 0)) val
      (not (nil? val)) (recur (first r) (rest r))
      :else nil)))

(defn normal-at
  "Return a normal vector from a point in a shape"
  ^doubles
  [shape point]
  (let [local-point  (m*v (inverse (:transform (:shape shape))) point)
        local-normal ((:local-normal-at shape) shape local-point)
        world-normal (m*v (transpose (inverse (:transform (:shape shape)))) local-normal)
        wn (make-vector (x world-normal) (y world-normal) (z world-normal))]
    (norm wn)))

(defn reflect
  "Reflect a vector over another"
  ^doubles
  [v n]
  (v- v (v* n (* 2 (dot v n)))))

(defn lighting
  "Phong Reflection model"
  [material light point eyev normalv in-shadow?]
  (let [effective-color (c*c (:color material) (:intensity light))
        lightv  (norm (v- (:position light) point))
        ambient (v* effective-color (:ambient material))
        light-dot-normal (dot lightv normalv)
        diffuse  (c* effective-color (* (:diffuse material) light-dot-normal))
        reflectv (reflect (neg lightv) normalv)
        reflect-dot-eye (dot reflectv eyev)
        factor (Math/pow reflect-dot-eye (:shininess material))
        specular (c* (:intensity light) (* (:specular material) factor))]
    (cond
      (true? in-shadow?) ambient
      (and (>= light-dot-normal 0) (> reflect-dot-eye 0)) (c+ ambient (c+ diffuse specular))
      (>= light-dot-normal 0) (c+ ambient (c+ diffuse (make-color 0 0 0)))
      :else (c+ ambient (c+ (make-color 0 0 0) (make-color 0 0 0))))))
