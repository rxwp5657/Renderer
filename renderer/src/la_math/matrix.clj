(ns la-math.matrix
  (:use [la-math.vector]
        [uncomplicate.fluokitten core jvm]))

(declare determinant cofactor minor)

(defn make-matrix
  "Make a matrix"
  ^doubles
  [& data]
  (double-array (into [] data)))

(defn- width
  [^doubles m]
  (let [len (alength m)]
    (cond
      (= len 16) 4
      (= len 9)  3
      (= len 4)  2
      :else 0)))

(defn- get-rc
  "Transform 2D coordinate to 1D"
  [w r c]
  (+ (* w r) c))

(defn m-rc
  "Get the value on the row r and column c"
  ^double
  [^doubles m r c]
  (aget m (get-rc (width m) r c)))

(defn m=
  "Check if matrices m1 and m2 are the same"
  [^doubles m1 ^doubles m2]
  (v= m1 m2))

(defn- get-r
  "Get row of matrix m"
  ^doubles
  [^doubles m r]
  (double-array (fmap #(m-rc m r %) (range (width m)))))

(defn- get-c
  "Get column of matrix m"
  ^doubles
  [^doubles m c]
  (double-array (fmap #(m-rc m % c) (range (width m)))))

(defn m*m
  "Matrix - Matrix multiplication"
  ^doubles
  [^doubles m1 ^doubles m2]
  (apply make-matrix
    (for [r (range (width m1))
          c (range (width m2))]
      (dot (get-r m1 r) (get-c m2 c)))))

(defn transpose
  "Transpoe Matrix m"
  ^doubles
  [^doubles m]
  (apply make-matrix
    (for [r (range (width  m))
          c (range (width  m))]
      (m-rc m c r))))


(defn- determinant-2D
  "Calculate 2D matrix determinant"
  ^double
  [^doubles m]
  (- (* (m-rc m 0 0) (m-rc m 1 1)) (* (m-rc m 0 1) (m-rc m 1 0))))

(defn- determinant-M
  "Calculate any matrix determinant"
  ^double
  [^doubles m]
  (let [cofactors (fmap #(cofactor m 0 %) (range (width m)))]
    (foldmap + 0.0 * cofactors (get-r m 0))))

(defn determinant
  "Calculate matrix determinant"
  ^double
  [^doubles m]
  (if (= (width m) 2)
    (determinant-2D m)
    (determinant-M m)))

(defn submatrix
  "Getting submatrices"
  ^doubles
  [^doubles m dr dc]
  (apply make-matrix
    (for [r (range (width m))
          c (range (width m))
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

(defn invertible?
  "Is matrix m invertible?"
  [^doubles m]
  (not (= 0.0 (determinant m))))

(defn- m-cofactor
  "Calculate cofactor matrix"
  ^doubles
  [^doubles m]
  (apply make-matrix
    (for [r (range (width  m))
          c (range (width m))]
      (cofactor m r c))))

(defn div-mc
  "Divide each matrix entry by a constant"
  ^doubles
  [^doubles m c]
  (apply make-matrix (fmap #(/ % c) m)))

(defn inverse
  "Calculate the matrix inverse"
  ^doubles
  [^doubles m]
  (when (invertible? m)
    (div-mc (transpose (m-cofactor m)) (determinant m))))
