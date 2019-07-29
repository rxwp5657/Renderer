(ns render.patterns.gradient
  (:use [render.patterns.pattern]
        [la-math.vector]
        [la-math.matrix]
        [canvas.color]))

(declare gradient-at)

(defrecord Gradient [pattern pattern-at])

(defn make-gradient
 "Make a stripe pattern"
 [ca cb]
 (Gradient. (make-pattern ca cb) gradient-at))

(defn gradient-at
  "Return gradient color"
  [pattern point]
  (let [distance (c- (:b (:pattern pattern)) (:a (:pattern pattern)))
        fraction (- (x point) (Math/floor (x point)))]
    (c+ (:a (:pattern pattern)) (c* distance fraction))))
