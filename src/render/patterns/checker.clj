(ns render.patterns.checker
  (:use [render.patterns.pattern]
        [la-math.vector]
        [canvas.color]))

(declare checker-at)

(defrecord Checker [pattern pattern-at])

(defn make-checker
 "Make a checker pattern"
 [ca cb]
 (Checker. (make-pattern ca cb) checker-at))

(defn checker-at
  "Return checker color"
  [pattern point]
  (let [val (mod (+ (Math/floor (x point))
                    (Math/floor (y point))
                    (Math/floor (z point))) 2)]
    (if (= 0.0 val)
      (:a (:pattern pattern))
      (:b (:pattern pattern)))))
