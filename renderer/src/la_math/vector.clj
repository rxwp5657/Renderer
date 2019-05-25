(ns la-math.vector
  (:use [uncomplicate.fluokitten core jvm]))

(defn make-tuple
  "Create a Java double array of size 4"
  ^doubles
  [x y z w]
  (double-array [x y z w]))

(defn make-point
  "Create a Java double array of size 4 where the last value is 1 (represents a point)"
  ^doubles
  [x y z]
  (make-tuple x y z 1))

(defn make-vector
 "Create a Java double array of size 4 where the last value is 0 (represents a vector)"
 ^doubles
 [x y z]
 (make-tuple x y z 0))

(defn x
 "Get the 'x' component on the Java double array t"
 ^double
 [^doubles t]
 (aget t 0))

(defn y
 "Get the 'y' component on the Java double array t"
 ^double
 [^doubles t]
 (aget t 1))

(defn z
 "Get the 'z' component on the Java double array t"
 ^double
 [^doubles t]
 (aget t 2))

(defn w
 "Get the 'w' component on the Java double array t"
 ^double
 [^doubles t]
 (aget t 3))

(defn vec?
 "Check if the given Java array t represents a vector"
 [t]
 (= (w t) 0.0))

(defn point?
 "Check if the given Java array t represents a point"
 [t]
 (= (w t) 1.0))

(defn v=
 "Check if two tuples are the same"
 [^doubles a ^doubles b]
 (= (seq a) (seq b)))

(defn e+
 "Define component-component addition"
 ^double
 [^double a ^double b]
 (+ a b))

(defn e*
 "Define component-component multiplication"
 ^double
 [^double a ^double b]
 (* a b))

(defn e-
 "Define component-component substraction"
 ^double
 [^double a ^double b]
 (- a b))

(defn neg
 "Negate vector"
 ^doubles
 [^doubles t]
 (fmap #(e* -1 %) t))

(defn v+
 "Vector addition"
 ^doubles
 [^doubles a ^doubles b]
 (fmap e+ a b))

(defn v-
 "Vector substraction"
 ^doubles
 [^doubles a ^doubles b]
 (fmap e- a b))

(defn v*
 "Vector-salar multiplication"
 ^doubles
 [^doubles a ^double s]
 (fmap #(e* s %) a))

(defn vd
 "Vector-scalar divition"
 ^doubles
 [^doubles a ^double s]
 (v* a (/ 1 s)))

(defn mag
  "Get the magnitude of the vector v"
  ^double
  [^doubles v]
  (Math/sqrt (fold #(+ (Math/pow %2 2) %1) 0.0 v)))

(defn norm
  "Normalize the vector v"
  ^doubles
  [^doubles v]
  (vd v (mag v)))

(defn dot
  "Vector dot product"
  ^double
  [^doubles a ^doubles b]
  (foldmap e+ 0.0 e* a b))

(defn cross
  "Vector cross product"
  ^doubles
  [^doubles a ^doubles b]
  (make-vector (- (e* (y a) (z b)) (e* (y b) (z a)))
               (- (e* (z a) (x b)) (e* (x a) (z b)))
               (- (e* (x a) (y b)) (e* (y a) (x b)))))
