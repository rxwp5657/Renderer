(ns render.core-test
  (:require [clojure.test :refer :all]
            [la-math.vector :refer :all]
            [la-math.matrix :refer :all]
            [render.ray-tracer :refer :all]
            [render.core :refer :all]
            [canvas.color :refer :all]))

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

(deftest ray-test
  (testing "ray creation test"
    (let [origin   (make-point  1 2 3)
          position (make-vector 4 5 6)
          ray (make-ray origin position)]
      (is (v= (:origin ray) origin))
      (is (v= (:direction ray) position)))))

(deftest ray-position
  (testing "ray position test")
  (let [ray (make-ray (make-point 2 3 4) (make-vector 1 0 0))
        p1  (make-point 2 3 4)
        p2  (make-point 3 3 4)
        p3  (make-point 1 3 4)
        p4  (make-point 4.5 3 4)]
    (is (v= p1 (position ray 0)))
    (is (v= p2 (position ray 1)))
    (is (v= p3 (position ray -1)))
    (is (v= p4 (position ray 2.5)))))

(deftest ray-spheres-test
  (testing "ray intersects with a sphere at ..."
    (let [r1 (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
          r2 (make-ray (make-point 0 1 -5) (make-vector 0 0 1))
          r3 (make-ray (make-point 0 2 -5) (make-vector 0 0 1))
          r4 (make-ray (make-point 0 0  0) (make-vector 0 0 1))
          r5 (make-ray (make-point 0 0 5) (make-vector 0 0 1))
          s  (make-sphere)
          xs1 (intersect s r1)
          xs2 (intersect s r2)
          xs3 (intersect s r3)
          xs4 (intersect s r4)
          xs5 (intersect s r5)]
      (is (= 4.0 (:t (first xs1))))
      (is (= 6.0 (:t (last  xs1))))
      (is (= 5.0 (:t (first xs2))))
      (is (= 5.0 (:t (last  xs2))))
      (is (= 0 (count xs3)))
      (is (= -1.0 (:t (first xs4))))
      (is (= 1.0  (:t (last  xs4))))
      (is (= -6.0 (:t (first xs5))))
      (is (= -4.0 (:t (last  xs5)))))))

(deftest intersections-test
  (testing "intersections data structure test"
    (let [s (make-sphere)
          i (make-intersection 3.5 s)]
      (is (s= s  (:object i)))
      (is (= 3.5 (:t i))))))

(deftest aggregate-intersections-test
  (testing "aggregate intersection test"
    (let [s  (make-sphere)
          i1 (make-intersection 1 s)
          i2 (make-intersection 2 s)
          xs (make-intersections i1 i2)]
      (is (= 2 (count xs)))
      (is (s= s (:object (first xs))))
      (is (s= s (:object (last xs)))))))

(deftest hits-test
  (testing "Hit testing"
    (let [s  (make-sphere)
          i1 (make-intersection 1 s)
          i2 (make-intersection 2 s)
          i3 (make-intersection -1 s)
          i4 (make-intersection 1 s)
          i5 (make-intersection -2 s)
          i6 (make-intersection -1 s)
          i7 (make-intersection 5 s)
          i8 (make-intersection 7 s)
          i9 (make-intersection -3 s)
          i10 (make-intersection 2 s)
          xs1 (make-intersections i1 i2)
          xs2 (make-intersections i3 i4)
          xs3 (make-intersections i5 i6)
          xs4 (make-intersections i7 i8 i9 i10)]
      (is (= i1 (hit xs1)))
      (is (= i4 (hit xs2)))
      (is (nil? (hit xs3)))
      (is (= i10 (hit xs4))))))

(deftest ray-transform-test
  (testing "Ray transfomation"
    (let [r  (make-ray (make-point 1 2 3) (make-vector 0 1 0))
          mt (translation 3 4 5)
          st (scaling 2 3 4)
          r2 (ray-transform r mt)
          r3 (ray-transform r st)]
      (is (v= (:origin r2)    (make-point 4 6 8)))
      (is (v= (:direction r2) (make-vector 0 1 0)))
      (is (v= (:origin r3)    (make-point 2 6 12)))
      (is (v= (:direction r3) (make-vector 0 3 0))))))

(deftest set-sphere-transform-test
  (testing "Set sphere transform"
    (let [s  (make-sphere)
          t  (translation 2 3 4)
          s2 (set-transform s t)]
      (is (m= t (:transform s2)))
      (is (m= (identity-m) (:transform s))))))

(deftest intersection-scaled-sphere-test
  (testing "Ray intersection with scaled sphere"
    (let [r  (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
          s  (set-transform (make-sphere) (scaling 2 2 2))
          s2 (set-transform (make-sphere) (translation 5 0 0))
          xs (intersect s r)
          xs2 (intersect s2 r)]
      (is (= 3.0 (:t (first xs))))
      (is (= 7.0 (:t (last  xs))))
      (is (= 0 (count xs2))))))

(deftest normal-sphere-test
  (testing "Normal vector of a sphere"
   (let [s  (make-sphere)
         n1 (normal-at s (make-point 1 0 0))
         n2 (normal-at s (make-point 0 1 0))
         n3 (normal-at s (make-point 0 0 1))
         n4 (normal-at s (make-point (/ (Math/sqrt 3) 3) (/ (Math/sqrt 3) 3) (/ (Math/sqrt 3) 3)))]
      (is (v= n1 (make-vector 1 0 0)))
      (is (v= n2 (make-vector 0 1 0)))
      (is (v= n3 (make-vector 0 0 1)))
      (is (v= n4 (make-vector (/ (Math/sqrt 3) 3) (/ (Math/sqrt 3) 3) (/ (Math/sqrt 3) 3))))
      (is (v= n4 (norm (make-vector (/ (Math/sqrt 3) 3) (/ (Math/sqrt 3) 3) (/ (Math/sqrt 3) 3))))))))

(deftest normal-displaced-sphere-test
  (testing "Normal vector of a sphere that is not centered on the origin"
    (let [s  (set-transform (make-sphere) (translation 0 1 0))
          s2 (set-transform (make-sphere) (m*m (scaling 1 0.5 1) (rotation-z (/ Math/PI 5))))
          n1 (normal-at s (make-point 0 1.70711 -0.70711))
          n2 (normal-at s2 (make-point 0 (/ (Math/sqrt 2) 2) (* -1 (/ (Math/sqrt 2) 2))))]
      (is (v= n1 (make-vector 0 0.7071067811865475 -0.7071067811865476)))
      (is (v= n2 (make-vector 0 0.9701425001453319 -0.2425356250363329))))))

(deftest reflection-test
  (testing "Testing reflection"
    (let [v1 (make-vector 1 -1 0)
          n1 (make-vector 0 1 0)
          r1 (reflect v1 n1)
          v2 (make-vector 0 -1 0)
          n2 (make-vector (/ (Math/sqrt 2) 2) (/ (Math/sqrt 2) 2) 0)
          r2 (reflect v2 n2)]
      (is (v= r1 (make-vector 1 1 0)))
      (is (v= r2 (make-vector 1.0000000000000002  2.220446049250313E-16 0))))))

(deftest light-source-test
  (testing "light source testing"
    (let [intensity (make-color 1 1 1)
          pos (make-point 0 0 0)
          light (make-light-point pos intensity)]
      (is (v= pos (:position  light)))
      (is (c= intensity (:intensity light))))))

(deftest material-test
  (testing "material creation test"
    (let [material (make-material (make-color 1 1 1) 0.1 0.9 0.9 200)]
      (is (c= (make-color 1 1 1) (:color material)))
      (is (= 0.1 (:ambient material)))
      (is (= 0.9 (:diffuse material)))
      (is (= 0.9 (:spectacular material)))
      (is (= 200 (:shininess material))))))

(deftest sphere-material-test
  (testing "adding material to sphere"
    (let [s  (make-sphere)
          m  (make-material)
          m2 (set-ambient m 1)
          s2 (set-material s m2)]
      (is (= (:ambient (:material s2)) 1)))))

(deftest light-test-1
  (testing "lighting with the eye between the light and the surface"
    (let [m    (make-material)
          pos  (make-point 0 0 0)
          eyev (make-vector 0 0 -1)
          normalv (make-vector 0 0 -1)
          light (make-light-point (make-point 0 0 -10) (make-color 1 1 1))
          result (lighting m light pos eyev normalv)]
      (is (c= result (make-color 1.9000000000000001 1.9000000000000001 1.9000000000000001))))))

(deftest light-test-2
  (testing "lighting with the eye between the light and the surface, eye offset at 45 degree"
    (let [m    (make-material)
          pos  (make-point 0 0 0)
          eyev (make-vector 0 (/ (Math/sqrt 2) 2) (* -1 (/ (Math/sqrt 2) 2)))
          normalv (make-vector 0 0 -1)
          light (make-light-point (make-point 0 0 -10) (make-color 1 1 1))
          result (lighting m light pos eyev normalv)]
      (is (c= result (make-color 1.0 1.0 1.0))))))

(deftest light-test-3
  (testing "lighting with the eye opoosite to the surface, light offset at 45 degree"
    (let [m    (make-material)
          pos  (make-point 0 0 0)
          eyev (make-vector 0 0 -1)
          normalv (make-vector 0 0 -1)
          light (make-light-point (make-point 0 10 -10) (make-color 1 1 1))
          result (lighting m light pos eyev normalv)]
      (is (c= result (make-color 0.7363961030678927 0.7363961030678927 0.7363961030678927))))))

(deftest light-test-4
  (testing "lighting with the eye in the path of reflection vector"
    (let [m    (make-material)
          pos  (make-point 0 0 0)
          eyev (make-vector 0 (* -1 (/ (Math/sqrt 2) 2)) (* -1 (/ (Math/sqrt 2) 2)))
          normalv (make-vector 0 0 -1)
          light (make-light-point (make-point 0 10 -10) (make-color 1 1 1))
          result (lighting m light pos eyev normalv)]
      (is (c= result (make-color 1.6363961030678928 1.6363961030678928 1.6363961030678928))))))

(deftest light-test-5
  (testing "lighting with the lght behind the surface"
    (let [m    (make-material)
          pos  (make-point 0 0 0)
          eyev (make-vector 0 0 -1)
          normalv (make-vector 0 0 -1)
          light (make-light-point (make-point 0 0 10) (make-color 1 1 1))
          result (lighting m light pos eyev normalv)]
      (is (c= result (make-color 0.1 0.1 0.1))))))
