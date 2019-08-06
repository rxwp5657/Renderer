(ns render.primitives.cube
  (:use [render.primitives.shape]
        [render.comp.data-structures]
        [la-math.vector]))
      

(declare intersect-cube cube-normal-at)

(defrecord Cube [shape local-intersect local-normal-at])

(defn make-cube
  "make a cube"
  []
  (Cube. (make-shape) intersect-cube cube-normal-at))

(defn- check-axis
  [origin direction]
  (let [tmin-numerator (- -1 origin)
        tmax-numerator (-  1 origin)
        res (if (>= (Math/abs direction) 0.00001)
              {:min (/ tmin-numerator direction) :max (/ tmax-numerator direction)}
              {:min (* tmin-numerator Double/POSITIVE_INFINITY) :max (* tmax-numerator Double/POSITIVE_INFINITY)})]
    (if (> (:min res) (:max res))
      {:min (:max res) :max (:min res)}
      res)))

(defn intersect-cube
  "Check if ray intersects with a cube"
  [cube ray]
  (let [xt (check-axis (x (:origin ray)) (x (:direction ray)))
        yt (check-axis (y (:origin ray)) (y (:direction ray)))
        zt (check-axis (z (:origin ray)) (z (:direction ray)))
        tmin (apply max [(:min xt) (:min yt) (:min zt)])
        tmax (apply min [(:max xt) (:max yt) (:max zt)])]
    (if (> tmin tmax)
      []
      (make-intersections (make-intersection tmin cube) (make-intersection tmax cube)))))

(defn cube-normal-at
  "Return a normal vector from a point in a cube"
  ^doubles
  [cube point]
  (let [maxc (apply max [(Math/abs (x point)) (Math/abs (y point)) (Math/abs (z point))])]
    (cond
      (= maxc (Math/abs (x point))) (make-vector (x point) 0 0)
      (= maxc (Math/abs (y point))) (make-vector 0 (y point) 0)
      :else (make-vector 0 0  (z point)))))
