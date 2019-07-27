(ns render.patterns.stripe
  (:use [la-math.vector]
        [canvas.color]))

(defrecord Stripe [a b])

(defn make-stripe-pattern
 "Make a stripe pattern"
 [ca cb]
 (Stripe. ca cb))

(defn stripe-at
  "Alternate pattern between two colors"
  [pattern point]
  (let [val (mod (Math/floor (x point)) 2)]
    (if (= 0.0 val)
      (:a pattern)
      (:b pattern))))
