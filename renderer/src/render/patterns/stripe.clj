(ns render.patterns.stripe
  (:use [render.patterns.pattern]
        [la-math.vector]
        [la-math.matrix]
        [canvas.color]))

(declare stripe-at)

(defrecord Stripe [pattern pattern-at])

(defn make-stripe-pattern
 "Make a stripe pattern"
 [ca cb]
 (Stripe. (make-pattern ca cb) stripe-at))

(defn stripe-at
  "Alternate pattern between two colors"
  [pattern point]
  (let [val (mod (Math/floor (x point)) 2)]
    (if (= 0.0 val)
      (:a (:pattern pattern))
      (:b (:pattern pattern)))))
