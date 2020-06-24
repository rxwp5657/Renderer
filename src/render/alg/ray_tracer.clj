(ns render.alg.ray-tracer
  (:use [render.comp.data-structures]
        [la-math.vector]
        [la-math.matrix]
        [render.patterns.pattern]
        [render.primitives.shape]
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

(defn world-to-object
  "Transform a point from world space to object space"
  [shape point]
  (let [pn (if (parent? shape)
             (world-to-object (:parent (:shape shape)) point)
             point)]
    (m*v (inverse (:transform (:shape shape))) pn)))

(defn normal-to-world
  "Transform the normal of an object to world space"
  [shape normal]
  (let [nl  (m*v (transpose (inverse (:transform (:shape shape)))) normal)
        nlw (make-vector (x nl) (y nl) (z nl))
        nlv (norm nlw)
        result (if (parent? shape) (normal-to-world (:parent (:shape shape)) nlv) nlv)]
    result))

(defn normal-at
  "Return a normal vector from a point in a shape"
  ^doubles
  [shape point]
  (let [local-point  (world-to-object shape point)
        local-normal ((:local-normal-at shape) shape local-point)]
    (normal-to-world shape local-normal)))

(defn pattern-at-object
  "Return the correct transformed pattern"
  [pattern object world-point]
  (let [object-point  (world-to-object object world-point)
        pattern-point (m*v (inverse (:transform (:pattern pattern))) object-point)]
    ((:pattern-at pattern) pattern pattern-point)))

(defn reflect
  "Reflect a vector over another"
  ^doubles
  [v n]
  (v- v (v* n (* 2 (dot v n)))))

(defn lighting
  "Phong Reflection model"
  [material object light point eyev normalv in-shadow?]
  (let [color (if (nil? (:pattern material)) (:color material) (pattern-at-object (:pattern material) object point))
        effective-color (c*c color (:intensity light))
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
