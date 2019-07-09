(ns la-math.matrix
  (:use [la-math.vector]
        [uncomplicate.fluokitten core jvm]))

(defn make-matrix
  "Make a 4x4, 3x3 or 2x2 matrix"
  ^doubles
  ([w ht a b c d
         e f g h
         i j k l
         m n o p]
   {:width w :heigth ht :m (double-array [a b c d e f g h i j k l m n o p])})
  ([w ht a b c
         d e f
         g h i]
   {:width w :heigth ht :m (double-array [a b c d e f g h i])})
  ([w h a b
        c d]
   {:width w :heigth h  :m (double-array [a b c d])}))

(defn- get-rc
  "Transform 2D coordinate to 1D"
  [w r c]
  (+ (* w r) c))

(defn m-rc
  "Get the value on the row r and column c"
  ^double
  [^doubles m r c]
  (aget (:m m) (get-rc (:width m) r c)))

(defn m=
  "Check if matrices m1 and m2 are the same"
  [^doubles m1 ^doubles m2]
  (let [res (for [r (range (:width m1))
                  c (range (:width m2))
                  :let  [y 1]
                  :when (= (m-rc m1 r c) (m-rc m2 r c))] y)]
    (if (= (* (:width m1) (:width m1))
           (alength (to-array res)))
      true
      false)))

(defn- get-r
  "Get row of matrix m"
  ^doubles
  [^doubles m r]
  (double-array (fmap #(m-rc m r %) (range (:width m)))))

(defn- get-c
  "Get column of matrix m"
  ^doubles
  [^doubles m c]
  (double-array (fmap #(m-rc m % c) (range (:width m)))))

(defn m*m
  "Matrix - Matrix multiplication"
  ^doubles
  [^doubles m1 ^doubles m2]
  (apply make-matrix (:width m1) (:heigth m2)
    (for [r (range (:width  m1))
          c (range (:heigth m2))]
      (dot (get-r m1 r) (get-c m2 c)))))

(defn transpose
  "Transpoe Matrix m"
  ^doubles
  [^doubles m]
  (apply make-matrix (:width m) (:heigth m)
    (for [r (range (:width  m))
          c (range (:heigth m))]
      (m-rc m c r))))

(defn determinant
  "Calculate 2D matrix determinant"
  ^double
  [^doubles m]
  (- (* (m-rc m 0 0) (m-rc m 1 1)) (* (m-rc m 0 1) (m-rc m 1 0))))

(defn submatrix
  "Getting submatrices"
  ^doubles
  [^doubles m dr dc]
  (apply make-matrix (dec (:width m)) (dec (:heigth m))
    (for [r (range (:width  m))
          c (range (:heigth m))
          :when (not (or (= dr r) (= dc c)))]
      (m-rc m r c))))

(defn minor
  "Getting 3x3 matrix minor"
  [^doubles m r c]
  (determinant (submatrix m r c)))

(defn cofactor
  "Calculate 3x3 matrix cofactor"
  [^doubles m r c]
  (let [minor (minor m r c)]
    (if (= 1 (mod (+ r c) 2))
      (* -1 minor)
      minor)))
