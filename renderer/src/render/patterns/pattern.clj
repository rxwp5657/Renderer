(ns render.patterns.pattern
  (:use [la-math.matrix]))

(defrecord Pattern [a b transform])

(defn make-pattern
 "Make a stripe pattern"
 [ca cb]
 (Pattern. ca cb (identity-m)))

(defn set-pattern-p-transform
  "Change transform from pattern data structure"
  [pattern transform]
  (assoc pattern :transform transform))

(defn set-pattern-transform
  "Change transform from pattern"
  [pattern transform]
  (assoc pattern :pattern (set-pattern-p-transform (:pattern pattern) transform)))

(defn pattern-at-object
  "Return the correct transformed pattern"
  [pattern object world-point]
  (let [object-point  (m*v (inverse (:transform (:shape object))) world-point)
        pattern-point (m*v (inverse (:transform (:pattern pattern))) object-point)]
    ((:pattern-at pattern) pattern pattern-point)))
