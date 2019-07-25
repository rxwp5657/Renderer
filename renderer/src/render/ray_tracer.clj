(ns render.ray-tracer
  (:use [la-math.vector]
        [la-math.matrix]
        [canvas.color]))

(defn make-ray
  "Create a ray given its position point and direction vector"
  [^doubles o ^doubles p]
  {:origin o :direction p})

(defn make-light-point
  "Make a light source"
  [position intensity]
  {:position position :intensity intensity})

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

(defn normal-at
  "Return a normal vector from a point in a sphere"
  ^doubles
  [sphere point]
  (let [object-point  (m*v (inverse (:transform sphere)) point)
        object-normal (v- object-point (make-point  0 0 0))
        world-normal  (m*v (transpose (inverse (:transform sphere))) object-normal)]
    (norm (make-vector (x world-normal) (y world-normal) (z world-normal)))))

(defn reflect
  "Reflect a vector over another"
  ^doubles
  [v n]
  (v- v (v* n (* 2 (dot v n)))))

(defn set-ambient
  "Change ambient value of material"
  [material ambient]
  (assoc material :ambient ambient))

(defn set-diffuse
  "Change diffuse value of material"
  [material diffuse]
  (assoc material :diffuse diffuse))

(defn set-spectacular
  "Change spectacular value of material"
  [material spectacular]
  (assoc material :spectacular spectacular))

(defn set-shininess
  "Change spectacular value of material"
  [material shininess]
  (assoc material :shininess shininess))

(defn set-color
  "Change color value of material"
  [material color]
  (assoc material :color color))

(defn make-material
  "Make a material"
  ([]
   {:color (make-color 1 1 1) :ambient 0.1 :diffuse 0.9 :spectacular 0.9 :shininess 200})
  ([color ambient diffuse spectacular shininess]
   {:color color :ambient ambient :diffuse diffuse :spectacular spectacular :shininess shininess}))

(defn set-transform
  "Add a transformation to a sphere"
   [obj transform]
   (assoc obj :transform transform))

(defn set-material
  "Add a material to the sphere"
   [obj material]
   (assoc obj :material material))

(defn make-sphere
  "make a sphere"
  []
  {:id "Something" :center (make-point 0 0 0) :radii 1 :transform (identity-m) :material (make-material)})

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
        specular (c* (:intensity light) (* (:spectacular material) factor))]
    (cond
      (true? in-shadow?) ambient
      (and (>= light-dot-normal 0) (> reflect-dot-eye 0)) (c+ ambient (c+ diffuse specular))
      (>= light-dot-normal 0) (c+ ambient (c+ diffuse (make-color 0 0 0)))
      :else (c+ ambient (c+ (make-color 0 0 0) (make-color 0 0 0))))))
