(ns render.comp.world
  (:use [la-math.vector]
        [la-math.matrix]
        [canvas.color]
        [render.comp.data-structures]
        [render.primitives.shape]
        [render.primitives.sphere]))

(defrecord World [light objects])

(defn make-default-world
  "Make a world that contains a light source and two spheres"
  []
  (World. (make-light-point (make-point -10 10 -10) (make-color 1 1 1))
          [(set-material  (make-sphere) (set-color (set-diffuse (set-specular (make-material) 0.2) 0.7) (make-color 0.8 1.0 0.6)))
           (set-transform (make-sphere) (scaling 0.5 0.5 0.5))]))

(defn make-world
  "Make a world given a light source and some object"
  [light & objects]
  (World. light (into [] objects)))

(defn contains-obj
  "Check is obj is contained on the world"
  [world obj]
  (some #(s= obj %) (:objects world)))

(defn change-object
  "Change the object at the nth position of the world"
  [world index object]
  (assoc world :objects (assoc (:objects world) index object)))

(defn set-light
  "Add light point to the world"
  [world light]
  (assoc world :light light))

(defn add-shape
  "Add a shape to the world"
  [world shape]
  (assoc world :objects (conj (:objects world) shape)))
