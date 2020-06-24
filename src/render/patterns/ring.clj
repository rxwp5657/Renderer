(ns render.patterns.ring
  (:use [render.patterns.pattern]
        [la-math.vector]
        [canvas.color]))

(declare ring-at)

(defrecord Ring [pattern pattern-at])

(defn make-ring
 "Make a stripe pattern"
 [ca cb]
 (Ring. (make-pattern ca cb) ring-at))

(defn ring-at
  "Return ring color"
  [pattern point]
  (let [val (mod (Math/floor (Math/sqrt (+ (Math/pow (x point) 2) (Math/pow (z point) 2)))) 2)]
    (if (= 0.0 val)
      (:a (:pattern pattern))
      (:b (:pattern pattern)))))
