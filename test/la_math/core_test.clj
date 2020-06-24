(ns la-math.core-test
  (:require [clojure.test :refer :all]
            [la-math.vector :refer :all]
            [la-math.matrix :refer :all]))

(deftest v-test
 (testing "Vector Component and creation"
  (let [v (make-tuple 4.3 -4.2 3.1 1.0)]
   (are [vt t] (= t vt)
    4.3  (x v)
    -4.2 (y v)
    3.1  (z v)
    1.0  (w v)))))

(deftest type-test
 (testing "Vector? or Point?"
  (let [p (make-point 4.3 -4.2 3.1)
        v (make-vector 4.3 -4.2 3.1)]
       (is (vec?  v))
       (is (not (vector? p)))
       (is (point? p))
       (is (not (point? v))))))

(deftest arithmetic-A
 (testing "Tuple arithmetic: + -"
  (let [a (make-tuple 3.0 -2.0 5.0 1.0)
        b (make-tuple -2.0 3.0 1.0 0.0)]
   (are [o rs] (v= rs o)
    (make-tuple 1.0  1.0 6.0 1.0) (v+ a b)
    (make-tuple 5.0 -5.0 4.0 1.0) (v- a b)))))

(deftest arithmetic-B
 (testing "Tuple arithmetic: neg, s, /"
  (let [a (make-tuple 1.0 -2.0 3.0 -4.0)]
   (is (v= (neg a)    (make-tuple -1.0 2.0 -3.0 4.0)))
   (is (v= (v* a 3.5) (make-tuple 3.5 -7.0 10.5 -14.0)))
   (is (v= (vd a 2.0) (make-tuple 0.5 -1.0 1.5 -2.0))))))

(deftest arithmetic-C
  (testing "Vector arithmetic: magnitude and normalization"
    (let [m1 (make-vector 0 1 0)
          m2 (make-vector -1 -2 -3)
          n1 (make-vector 4 0 0)
          n2 (make-vector 1 2 3)]
      (is (=  (mag m1) 1.0))
      (is (=  (mag m2)  (Math/sqrt 14)))
      (is (v= (norm n1) (make-vector 1 0 0)))
      (is (v= (norm n2) (make-vector
                         (/ 1 (Math/sqrt 14))
                         (/ 2 (Math/sqrt 14))
                         (/ 3 (Math/sqrt 14))))))))
(deftest arithmetic-D
  (testing "Vector arithmetic: Dot and cross product"
    (let [a  (make-vector 1 2 3)
          b  (make-vector 2 3 4)
          c1 (make-vector -1 2 -1)
          c2 (make-vector 1 -2 1)]
      (is (=  (dot a b) 20.0))
      (is (v= (cross a b) c1))
      (is (v= (cross b a) c2)))))

(deftest m4-test
  (testing "4x4 Matrix creation and access"
    (let [m (make-matrix 1 2 3 4
                         5.5 6.5 7.5 8.5
                         9 10 11 12
                         13.5 14.5 15.5 16.5)]
      (are [t v] (= v t)
           1.0  (m-rc m 0 0)
           4.0  (m-rc m 0 3)
           5.5  (m-rc m 1 0)
           7.5  (m-rc m 1 2)
           11.0 (m-rc m 2 2)
           13.5 (m-rc m 3 0)
           15.5 (m-rc m 3 2)))))

(deftest m3-test
  (testing "3x3 Matrix creation and access"
    (let [m (make-matrix -3 5 0
                         1 -2 -7
                         0  0 1)]
      (are [t v] (= v t)
           -3.0 (m-rc m 0 0)
           -2.0 (m-rc m 1 1)
            1.0 (m-rc m 2 2)))))

(deftest m2-test
  (testing "2x2 Matrix creation and access"
    (let [m (make-matrix -3 5
                          1 -2)]
      (are [t v] (= v t)
           -3.0 (m-rc m 0 0)
            5.0 (m-rc m 0 1)
            1.0 (m-rc m 1 0)
           -2.0 (m-rc m 1 1)))))

(deftest m-equality
  (testing "Matrix equality"
    (let [m1 (make-matrix 1 2 3 4
                          5 6 7 8
                          9 8 7 6
                          5 4 3 2)
          m2 (make-matrix 1 2 3 4
                          5 6 7 8
                          9 8 7 6
                          5 4 3 2)
          m3 (make-matrix 2 3 4 5
                          6 7 8 9
                          8 7 6 5
                          4 3 2 1)]
      (is (m= m1 m2))
      (is (not (m= m1 m3))))))

(deftest m-arithmeticA
  (testing "Matrix - Matrix multiplication"
    (let [m1 (make-matrix 1 2 3 4
                          5 6 7 8
                          9 8 7 6
                          5 4 3 2)
          m2 (make-matrix -2 1 2 3
                           3 2 1 -1
                           4 3 6 5
                           1 2 7 8)
          r  (make-matrix 20.0 22.0 50.0 48.0
                          44.0 54.0 114.0 108.0
                          40.0 58.0 110.0 102.0
                          16.0 26.0 46.0 42.0)]
      (is (m= (m*m m1 m2) r)))))

(deftest transpose-test
  (testing "Transpoe Matrix M"
    (let [m (make-matrix 0 9 3 0
                         9 8 0 8
                         1 8 5 3
                         0 0 5 8)
          transposed (make-matrix 0 9 1 0
                                  9 8 8 0
                                  3 0 5 5
                                  0 8 3 8)]
      (is (m= (transpose m) transposed)))))

(deftest determinant-2D
  (testing "Calculating 2D determinant"
    (let [m (make-matrix 1 5
                        -3 2)]
      (is (= (determinant m) 17.0)))))

(deftest submatrix-test
  (testing "Getting submatrices"
    (let [m1 (make-matrix 1 5 0
                         -3 2 7
                          0 6 -3)
          m2 (make-matrix -6 1 1 6
                          -8 5 8 6
                          -1 0 8 2
                          -7 1 -1 1)
          sub1 (make-matrix -3 2
                             0 6)
          sub2 (make-matrix -6 1 6
                            -8 8 6
                            -7 -1 1)]
      (are [t r] (m= t r)
           (submatrix m1 0 2) sub1
           (submatrix m2 2 1) sub2))))

(deftest minors-test
  (testing "Manipulating minors"
    (let [m (make-matrix 3 5 0
                         2 -1 -7
                         6 -1  5)]
      (is (= (minor m 1 0) 25.0)))))

(deftest cofactor-test
  (testing "Computing cofactors"
    (let [m (make-matrix 3 5 0
                         2 -1 -7
                         6 -1  5)]
      (are [t r] (= t r)
           (cofactor m 0 0) -12.0
           (cofactor m 1 0) -25.0))))

(deftest cofactors-determinant
  (testing "Calculating determinants"
    (let [m1 (make-matrix 1 2 6
                         -5 8 -4
                          2 6 4)
          m2 (make-matrix -2 -8 3 5
                          -3 1 7 3
                           1 2 -9 6
                          -6 7 7 -9)]
      (are [t r] (= t r)
           (cofactor m1 0 0) 56.0
           (cofactor m1 0 1) 12.0
           (cofactor m1 0 2) -46.0
           (determinant m1) -196.0
           (cofactor m2 0 0) 690.0
           (cofactor m2 0 1) 447.0
           (cofactor m2 0 2) 210.0
           (cofactor m2 0 3) 51.0
           (determinant m2) -4071.0))))

(deftest invertible
  (testing "Test if is invertible"
    (let [m1 (make-matrix 6 4 4 4
                          5 5 7 6
                          4 -9 3 -7
                          9  1 7 -6)
          m2 (make-matrix -4 2 -2 -3
                           9 6  2 6
                           0 -5 1 -5
                           0 0 0 0)]
      (is (not (invertible? m2)))
      (is (invertible? m1)))))

(deftest inverse-test
  (testing "Inverse function"
    (let [m1 (make-matrix -5 2 6 -8
                           1 -5 1 8
                           7  7 -6 -7
                           1 -3 7 4)
          inv1 (make-matrix 0.21804511278195488 0.45112781954887216 0.24060150375939848 -0.045112781954887216
                           -0.8082706766917294  -1.4567669172932332 -0.44360902255639095 0.5206766917293233
                           -0.07894736842105263 -0.2236842105263158 -0.05263157894736842 0.19736842105263158
                           -0.5225563909774437 -0.8139097744360902 -0.3007518796992481 0.30639097744360905)]
      (is (m= (inverse m1) inv1)))))
