(ns render.primitives.group
  (:use [la-math.matrix]))

(defrecord Group [transform shapes])

(defn make-group
  "Make a group"
  ([]
   (Group. (identity-m) (java.util.ArrayList.)))
  ([transform & shapes]
   (let [data (java.util.ArrayList.)]
     (do
       (doseq [s shapes] (.add data s))
       (Group. transform data)))))
