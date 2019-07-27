(ns render.comp.data-structures
  (:use [la-math.vector]
        [canvas.color]))

(defrecord Material [color ambient diffuse specular shininess pattern])

(defn make-material
  "Make a material"
  ([]
   (Material. (make-color 1 1 1) 0.1 0.9 0.9 200 nil))
  ([color ambient diffuse specular shininess pattern]
   (Material. color ambient diffuse specular shininess pattern)))

(defn set-color
  "Change color value of material"
  [material color]
  (assoc material :color color))

(defn set-ambient
  "Change ambient value of material"
  [material ambient]
  (assoc material :ambient ambient))

(defn set-diffuse
  "Change diffuse value of material"
  [material diffuse]
  (assoc material :diffuse diffuse))

(defn set-specular
  "Change specular value of material"
  [material specular]
  (assoc material :specular specular))

(defn set-shininess
  "Change specular value of material"
  [material shininess]
  (assoc material :shininess shininess))

(defn set-pattern
  "Set pattern to material"
  [material pattern]
  (assoc material :pattern pattern))

(defn mt=
 "Compare two materials"
 [m1 m2]
 (and (c= (:color m1) (:color m2))
      (=  (:ambient m1) (:ambient m2))
      (=  (:diffuse m1) (:diffuse m2))
      (=  (:specular m1) (:specular m2))
      (=  (:shininess m1) (:shininess m2))))

(defrecord Ray [origin direction])

(defn make-ray
  "Create a ray given its position point and direction vector"
  [^doubles o ^doubles p]
  (Ray. o p))

(defrecord LightPoint [position intensity])

(defn make-light-point
  "Make a light source"
  [position intensity]
  (LightPoint. position intensity))

(defn light=
  "Compare two light points"
  [l1 l2]
  (and (v= (:position l1)  (:position l2))
       (v= (:direction l1) (:direction l2))))

(defrecord Intersection [t object])

(defn make-intersection
  "Make a intersection data structure"
  [t obj]
  (Intersection. t obj))

(defn make-intersections
  "Make an aggregate collection of intersections"
  [& intersections]
  (vec (sort-by :t intersections)))
