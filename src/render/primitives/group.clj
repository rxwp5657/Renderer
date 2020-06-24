(ns render.primitives.group
  (:use [la-math.matrix]
        [la-math.vector]
        [render.primitives.shape]
        [render.alg.ray-tracer]
        [render.comp.data-structures]))

(declare group-intersection)

(defrecord Group [shape shapes local-intersect])

(defn make-group
  "Make a group"
  ([]
   (Group. (make-shape) (java.util.ArrayList.) group-intersection))
  ([& shapes]
   (let [data (java.util.ArrayList.)]
     (do
       (doseq [s shapes] (.add data s))
       (Group. (make-shape) data group-intersection)))))

(defn add-child
  "Add a child to the group"
  [group child]
  (let [shape (set-parent child group)]
    (do
      (.add (:shapes group) shape)
      shape)))

(defn- get-intersections
  "Get the intersections from the shapes and the ray"
  [group ray]
  (map #(intersect % ray) (:shapes group)))

(defn group-intersection
  "Ray - group intersection"
  [group ray]
  (if (empty? (:shapes group))
    []
    (let [intersections (get-intersections group ray)]
      (apply make-intersections (reduce #(concat %2 %1) intersections)))))
