(ns la-math.core-test
  (:require [clojure.test :refer :all]
            [la-math.vector :refer :all]))

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
