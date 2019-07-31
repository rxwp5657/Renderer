(ns render.alg.scene
  (:use [la-math.vector]
        [la-math.matrix]
        [render.comp.data-structures]
        [render.primitives.sphere]
        [canvas.color]
        [canvas.canvas]
        [render.alg.ray-tracer]))

(declare reflected-color)

;; Helper data structure that encapsulates intersection data

(defrecord Computations [t object point eyev normalv inside over-point under-point reflectv n1 n2])

(defn make-computations
  "Return a computations data-structure"
  [t object point eyev normalv inside over-point under-point reflectv n1 n2]
  (Computations. t object point eyev normalv inside over-point under-point reflectv n1 n2))

(defn- inside?
  "If ray is inside the object, return the negative of the normal and set inside to true"
  [normalv eyev]
  (if (< (dot normalv eyev) 0)
    {:inside true  :normal (neg normalv)}
    {:inside false :normal normalv}))

(defn- get-n
  "Get n1 or n2 value"
  [containers]
  (if (empty? containers)
    1.0
   (:refractive-index (:material (:shape (last containers))))))

(defn- update-container
  "Update container whether or not it contains the object"
  [containers intersection]
  (if (some #(= (:object intersection) %) containers)
    (remove #(= (:object intersection) %) containers)
    (conj containers (:object intersection))))

(defn get-ns
  "Get refractive indices of the materials on either side of a ray-object intersection"
  [intersections intersection]
  (loop [i (first intersections)
         r (rest  intersections)
         containers []]
    (if (= i intersection)
      {:n1 (get-n containers) :n2 (get-n (update-container containers i))}
      (recur (first r) (rest r) (update-container containers i)))))


(defn prepare-computations
  "Precompute information related to the intersection"
  ([intersection ray]
   (prepare-computations intersection ray [intersection]))
  ([intersection ray intersections]
   (let [t (:t intersection)
         object (:object intersection)
         point  (position ray t)
         eyev   (neg (:direction ray))
         normal (normal-at object point)
         inside (inside? normal eyev)
         over-point  (v+ point (v* (:normal inside) 0.00001))
         under-point (v- point (v* (:normal inside) 0.00001))
         reflectv (reflect (:direction ray) (:normal inside))
         n (get-ns intersections intersection)]
     (make-computations t object point eyev (:normal inside) (:inside inside) over-point under-point reflectv (:n1 n) (:n2 n)))))

;; View transform matrix

(defn make-view-transform
  "Get the view transform"
  [from to up]
  (let [forward (norm (v- to from))
        left    (cross forward (norm up))
        true-up (cross left forward)
        orientation (make-matrix (x left) (y left) (z left) 0
                                 (x true-up) (y true-up) (z true-up) 0
                                 (* -1 (x forward)) (* -1 (y forward)) (* -1 (z forward)) 0
                                 0 0 0 1)]
    (m*m orientation (translation (* -1 (x from)) (* -1 (y from)) (* -1 (z from))))))

;; scene algorithms

(defn- get-intersections
  "Get the intersections from the world objects and the ray"
  [world ray]
  (pmap #(intersect % ray) (:objects world)))

(defn intersect-world
  "Iterate over all the objects in the world checking if the ray r intersects with them"
  [world ray]
  (let [intersections (get-intersections world ray)]
    (apply make-intersections (reduce #(concat %2 %1) intersections))))

(defn shadowed?
  "Check if the point is a shadow"
  [world point]
  (let [v (v- (:position (:light world)) point)
        distance  (mag  v)
        direction (norm v)
        ray (make-ray point direction)
        intersections (intersect-world world ray)
        hit (hit intersections)]
    (if (and (not (nil? hit)) (<  (:t hit) distance))
      true
      false)))

(defn shade-hit
  "Get the color at the intersection encapsulated on the precomputations"
  [world comps remaining]
  (let [surface   (lighting (:material (:shape (:object comps))) (:object comps) (:light world) (:point comps) (:eyev comps) (:normalv comps) (shadowed? world (:over-point comps)))
        reflected (reflected-color world comps remaining)]
    (c+ surface reflected)))

(defn color-at
  "Get the color on an intersection if there isn't an intersection, return black color"
  [world ray remaining]
  (let [intersections (intersect-world world ray)
        hit (hit intersections)]
    (if (nil? hit)
      (make-color 0 0 0)
      (shade-hit world (prepare-computations hit ray intersections) remaining))))

(defn reflected-color
  "Compute reflection color"
  [world comps remaining]
  (if (or (= 0.0 (:reflective (:material (:shape (:object comps))))) (<= remaining 0))
    black
    (let [reflect-ray (make-ray (:over-point comps) (:reflectv comps))
          color (color-at world reflect-ray (dec remaining))]
      (c* color (:reflective (:material (:shape (:object comps))))))))

(defn refracted-color
  "Compute refracted color"
  [world comps remaining]
  (if (or (= 0.0 (:transparency (:material (:object comps)))) (= 0 remaining))
    black
    white))

(defn ray-for-pixel
  "Create rays that can pass throught any given pixel on canvas"
  [camera x y]
  (let [xoffset (* (+ 0.5 x) (:pixel-size camera))
        yoffset (* (+ 0.5 y) (:pixel-size camera))
        world-x (- (:half-width camera)  xoffset)
        world-y (- (:half-height camera) yoffset)
        pixel   (m*v (inverse (:transform camera)) (make-point world-x world-y -1))
        origin  (m*v (inverse (:transform camera)) (make-point 0 0 0))
        direction (norm (v- pixel origin))]
    (make-ray origin direction)))

(defn- get-pixel-data
  "Get the color of each pixel on the camera"
  [camera world]
  (let [coordinates (for [x (range (:hsize camera))
                          y (range (:vsize camera))] {:x x :y y})]
    (pmap (fn
            [coordinate]
            (let [ray   (ray-for-pixel camera (:x coordinate) (:y coordinate))
                  color (color-at world ray 5)]
              {:x (:x coordinate) :y (:y coordinate) :color color})) coordinates)))

(defn render
  "Render an image of the given world"
  [camera world]
  (let [coordinates (get-pixel-data camera world)]
    (loop [coordinate (first coordinates)
           rest-coordinates (rest coordinates)
           image (make-canvas (:hsize camera) (:vsize camera))]
      (if (empty? rest-coordinates)
        image
        (do
          (write-pixel image (:x coordinate) (:y coordinate) (:color coordinate))
          (recur (first rest-coordinates) (rest rest-coordinates) image))))))
