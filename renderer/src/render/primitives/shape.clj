(ns render.primitives.shape
  (:use [la-math.matrix]
        [render.comp.data-structures]
        [canvas.color]))

(defrecord Shape [transform material parent])

(defn make-shape
 "Make a shape"
 ([]
  (Shape. (identity-m) (make-material) nil)))

(defn set-shape-transform
  "Add a transformation to a shape"
  [obj transform]
  (assoc obj :transform transform))

(defn set-shape-material
  "Add a material to the shape"
  [obj material]
  (assoc obj :material material))

(defn set-shape-parent
  "Set the parent of a shape"
  [obj parent]
  (assoc obj :parent parent))

(defn set-transform
  "Change the transform to an object"
  [obj transform]
  (assoc obj :shape (set-shape-transform (:shape obj) transform)))

(defn set-material
  "Change material of and object"
  [obj material]
  (assoc obj :shape (set-shape-material (:shape obj) material)))

(defn set-parent
  "Set the parent of any primitive"
  [obj parent]
  (assoc obj :shape (set-shape-parent (:shape obj) parent)))

(defn parent?
  "Check if shape has parent"
  [shape]
  (not (nil? (:parent (:shape shape)))))
