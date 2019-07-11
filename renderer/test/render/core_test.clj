(ns render.core-test
  (:require [clojure.test :refer :all]
            [la-math.vector :refer :all]
            [la-math.matrix :refer :all]
            [render.core :refer :all]))

(deftest translation-test
  (testing "translation"
    (let [transform (translation 5 -3 2)
          p      (make-point -3 4 5)
          result (make-point 2 1 7)
          inv    (inverse transform)
          p2     (make-point -8 7 3)
          v      (make-vector -3 4 5)]
      (is (v= (m*v transform p) result))
      (is (point? (m*v transform p)))
      (is (v= (m*v inv p) p2))
      (is (vec? (m*v transform v))))))

(deftest scaling-test
  (testing "scaling"
    (let [transform (scaling 2 3 4)
          p       (make-point -4 6 8)
          result  (make-point -8 18 32)
          inv     (inverse transform)
          v       (make-vector -4 6 8)
          result2 (make-vector -8 18 32)
          result3 (make-vector -2 2 2)]
      (is (v= (m*v transform p) result))
      (is (v= (m*v transform v) result2))
      (is (v= (m*v inv v) result3)))))

(deftest rotation-x-test
  (testing "rotation on x axis"
    (let [p (make-point 0 1 0)
          half_quarter (rotation-x (/ Math/PI 4))
          full_quarter (rotation-x (/ Math/PI 2))
          inv (inverse half_quarter)
          result  (make-point 0 (/ (Math/sqrt 2) 2) 0.7071067811865475)
          result2 (make-point 0 6.123233995736766E-17 1)
          result3 (make-point 0 (/ (Math/sqrt 2) 2) (* -1 0.7071067811865475))]
      (is (v= (m*v half_quarter p) result))
      (is (v= (m*v full_quarter p) result2))
      (is (v= (m*v inv p) result3)))))

(deftest rotation-y-test
  (testing "rotation on y axis"
    (let [p (make-point 0 0 1)
          half_quarter (rotation-y (/ Math/PI 4))
          full_quarter (rotation-y (/ Math/PI 2))
          result  (make-point 0.7071067811865475  0 (/ (Math/sqrt 2) 2))
          result2 (make-point 1 0 6.123233995736766E-17)]
      (is (v= (m*v half_quarter p) result))
      (is (v= (m*v full_quarter p) result2)))))

(deftest rotation-z-test
  (testing "rotation on z axis"
    (let [p (make-point 0 1 0)
          half_quarter (rotation-z (/ Math/PI 4))
          full_quarter (rotation-z (/ Math/PI 2))
          result  (make-point -0.7071067811865475 (/ (Math/sqrt 2) 2) 0)
          result2 (make-point -1 6.123233995736766E-17  0)]
      (is (v= (m*v half_quarter p) result))
      (is (v= (m*v full_quarter p) result2)))))

(deftest shearing-test
  (testing "shearing test"
    (let [transform1 (shearing 1 0 0 0 0 0)
          transform2 (shearing 0 1 0 0 0 0)
          transform3 (shearing 0 0 1 0 0 0)
          transform4 (shearing 0 0 0 1 0 0)
          transform5 (shearing 0 0 0 0 1 0)
          transform6 (shearing 0 0 0 0 0 1)
          p (make-point 2 3 4)
          r1 (make-point 5 3 4)
          r2 (make-point 6 3 4)
          r3 (make-point 2 5 4)
          r4 (make-point 2 7 4)
          r5 (make-point 2 3 6)
          r6 (make-point 2 3 7)]
      (is (v= (m*v transform1 p) r1))
      (is (v= (m*v transform2 p) r2))
      (is (v= (m*v transform3 p) r3))
      (is (v= (m*v transform4 p) r4))
      (is (v= (m*v transform5 p) r5))
      (is (v= (m*v transform6 p) r6)))))

(deftest transform-test
  (testing "chaining transformations test")
  (let [p (make-point 1 0 1)
        rotation (rotation-x (/ Math/PI 2))
        scaling  (scaling 5 5 5)
        translation (translation 10 5 7)
        result (make-point 15 0 7)]
    (is (v= result (transform p rotation scaling translation)))))
