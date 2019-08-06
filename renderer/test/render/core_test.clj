(ns render.core-test
  (:require [clojure.test :refer :all]
            [la-math.vector :refer :all]
            [la-math.matrix :refer :all]
            [render.alg.ray-tracer :refer :all]
            [render.comp.data-structures   :refer :all]
            [render.primitives.shape  :refer :all]
            [render.primitives.sphere :refer :all]
            [render.primitives.cube   :refer :all]
            [render.primitives.cylinder :refer :all]
            [render.primitives.plane :refer :all]
            [render.primitives.cone :refer :all]
            [render.comp.camera :refer :all]
            [render.comp.world :refer :all]
            [render.core  :refer :all]
            [canvas.color :refer :all]
            [canvas.canvas :refer :all]
            [render.alg.scene :refer :all]
            [render.patterns.pattern :refer :all]
            [render.patterns.stripe  :refer :all]
            [render.patterns.gradient :refer :all]
            [render.patterns.ring :refer :all]
            [render.patterns.checker :refer :all]))

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

;; Ray tests

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

(deftest set-shape-transform-test
  (testing "Set shape transform"
    (let [s  (make-shape)
          t  (translation 2 3 4)
          s2 (set-shape-transform s t)]
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

;; Phong reflection algorithm

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
    (let [material (make-material (make-color 1 1 1) 0.1 0.9 0.9 200 nil 0.0 0.0 1.0)]
      (is (c= (make-color 1 1 1) (:color material)))
      (is (= 0.1 (:ambient material)))
      (is (= 0.9 (:diffuse material)))
      (is (= 0.9 (:specular material)))
      (is (= 200 (:shininess material))))))

(deftest shape-material-test
  (testing "adding material to shape"
    (let [s  (make-shape)
          m  (make-material)
          m2 (set-ambient m 1)
          s2 (set-shape-material s m2)]
      (is (= (:ambient (:material s2)) 1)))))

(deftest light-test-1
  (testing "lighting with the eye between the light and the surface"
    (let [m    (make-material)
          pos  (make-point 0 0 0)
          eyev (make-vector 0 0 -1)
          normalv (make-vector 0 0 -1)
          light (make-light-point (make-point 0 0 -10) (make-color 1 1 1))
          result (lighting m (make-sphere) light pos eyev normalv false)]
      (is (c= result (make-color 1.9000000000000001 1.9000000000000001 1.9000000000000001))))))

(deftest light-test-2
  (testing "lighting with the eye between the light and the surface, eye offset at 45 degree"
    (let [m    (make-material)
          pos  (make-point 0 0 0)
          eyev (make-vector 0 (/ (Math/sqrt 2) 2) (* -1 (/ (Math/sqrt 2) 2)))
          normalv (make-vector 0 0 -1)
          light (make-light-point (make-point 0 0 -10) (make-color 1 1 1))
          result (lighting m (make-sphere) light pos eyev normalv false)]
      (is (c= result (make-color 1.0 1.0 1.0))))))

(deftest light-test-3
  (testing "lighting with the eye opoosite to the surface, light offset at 45 degree"
    (let [m    (make-material)
          pos  (make-point 0 0 0)
          eyev (make-vector 0 0 -1)
          normalv (make-vector 0 0 -1)
          light (make-light-point (make-point 0 10 -10) (make-color 1 1 1))
          result (lighting m (make-sphere) light pos eyev normalv false)]
      (is (c= result (make-color 0.7363961030678927 0.7363961030678927 0.7363961030678927))))))

(deftest light-test-4
  (testing "lighting with the eye in the path of reflection vector"
    (let [m    (make-material)
          pos  (make-point 0 0 0)
          eyev (make-vector 0 (* -1 (/ (Math/sqrt 2) 2)) (* -1 (/ (Math/sqrt 2) 2)))
          normalv (make-vector 0 0 -1)
          light (make-light-point (make-point 0 10 -10) (make-color 1 1 1))
          result (lighting m (make-sphere) light pos eyev normalv false)]
      (is (c= result (make-color 1.6363961030678928 1.6363961030678928 1.6363961030678928))))))

(deftest light-test-5
  (testing "lighting with the light behind the surface"
    (let [m    (make-material)
          pos  (make-point 0 0 0)
          eyev (make-vector 0 0 -1)
          normalv (make-vector 0 0 -1)
          light (make-light-point (make-point 0 0 10) (make-color 1 1 1))
          result (lighting m (make-sphere) light pos eyev normalv false)]
      (is (c= result (make-color 0.1 0.1 0.1))))))

(deftest light-test-6
  (testing "lighting with the surface in shadow"
    (let [m    (make-material)
          pos  (make-point 0 0 0)
          eyev (make-vector 0 0 -1)
          normalv (make-vector 0 0 -1)
          light (make-light-point (make-point 0 0 -10) (make-color 1 1 1))
          result (lighting m (make-sphere) light pos eyev normalv true)]
      (is (c= result (make-color 0.1 0.1 0.1))))))

;; Scene tests

(deftest default-world-creation-test
  (testing "Default World creation"
    (let [light (make-light-point (make-point -10 10 -10) (make-color 1 1 1))
          s1 (set-material  (make-sphere) (set-color (set-diffuse (set-specular (make-material) 0.2) 0.7) (make-color 0.8 1.0 0.6)))
          s2 (set-transform (make-sphere) (scaling 0.5 0.5 0.5))
          w  (make-default-world)]
      (is (contains-obj w s1))
      (is (contains-obj w s2))
      (is (light= light (:light w))))))

(deftest intersect-world-test
  (testing "testing intersect world function"
    (let [w   (make-default-world)
          ray (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
          xs  (intersect-world w ray)]
      (is (= 4 (count xs)))
      (is (= 4.0 (:t (get xs 0))))
      (is (= 4.5 (:t (get xs 1))))
      (is (= 5.5 (:t (get xs 2))))
      (is (= 6.0 (:t (get xs 3)))))))

(deftest prepare-computations-test
  (testing "Prepate computations test"
    (let [ray   (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
          shape (make-sphere)
          intersection (make-intersection 4 shape)
          comps (prepare-computations intersection ray)]
      (is (= (:t intersection) (:t comps)))
      (is (s= (:object intersection) (:object comps)))
      (is (v= (:point comps) (make-point 0 0 -1)))
      (is (v= (:eyev  comps) (make-vector 0 0 -1)))
      (is (v= (:normalv comps) (make-vector 0 0 -1))))))

(deftest prepare-computations-test
  (testing "Prepate computations test 2"
    (let [ray   (make-ray (make-point 0 0 0) (make-vector 0 0 1))
          shape (make-sphere)
          intersection (make-intersection 1 shape)
          comps (prepare-computations intersection ray)]
      (is (v= (:point comps) (make-point 0 0 1)))
      (is (v= (:eyev  comps) (make-vector 0 0 -1)))
      (is (v= (:normalv comps) (make-vector 0 0 -1)))
      (is (:inside comps)))))

(deftest shading-intersection-1
  (testing "color intersection test 1 "
    (let [world (make-default-world)
          ray   (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
          shape (first (:objects world))
          intersection (make-intersection 4 shape)
          comps (prepare-computations intersection ray)
          res (shade-hit world comps 0)]
      (is (c= (make-color 0.38066119308103435 0.47582649135129296 0.28549589481077575) res)))))

(deftest shading-intersection-2
  (testing "color intersection test 2"
    (let [w     (make-default-world)
          world (set-light w (make-light-point (make-point 0 0.25 0) (make-color 1 1 1)))
          ray   (make-ray (make-point 0 0 0) (make-vector 0 0 1))
          shape (last (:objects world))
          intersection (make-intersection 0.5 shape)
          comps (prepare-computations intersection ray)
          res (shade-hit world comps 0)]
      (is (c= (make-color 0.9049844720832575 0.9049844720832575 0.9049844720832575) res)))))

(deftest world-feature-case-1
  (testing "The color when the ray misses"
    (let [world (make-default-world)
          ray   (make-ray (make-point 0 0 -5) (make-vector 0 1 0))
          res   (color-at world ray 0)]
      (is (c= (make-color 0 0 0) res)))))

(deftest world-feature-case-2
  (testing "The color when the ray hits"
    (let [world (make-default-world)
          ray   (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
          res   (color-at world ray 0)]
      (is (c= (make-color 0.38066119308103435 0.47582649135129296 0.28549589481077575) res)))))

(deftest world-feature-case-3
  (testing "The color whith an intersection behind the ray"
    (let [w1 (make-default-world)
          w2 (change-object w1 0 (set-material (first (:objects w1)) (set-ambient (:material (:shape (first (:objects w1)))) 1)))
          w3 (change-object w2 1 (set-material (last (:objects w2))  (set-ambient (:material (:shape (last  (:objects w2)))) 1)))
          ray   (make-ray (make-point 0 0 0.75) (make-vector 0 0 -1))
          res   (color-at w3 ray 0)]
      (is (c= (:color (:material (:shape (last (:objects w3))))) res)))))

(deftest view-transform-1
  (testing "The transformation matrix for the default orientation"
    (let [from (make-point 0 0 0)
          to   (make-point 0 0 -1)
          up   (make-vector 0 1 0)
          t    (make-view-transform from to up)]
      (is (m= (identity-m) t)))))

(deftest view-transform-2
  (testing "A view transformation matrix looking in positive z direction"
    (let [from (make-point 0 0 0)
          to   (make-point 0 0 1)
          up   (make-vector 0 1 0)
          t    (make-view-transform from to up)]
      (is (m= (scaling -1 1 -1) t)))))

(deftest view-transform-3
  (testing "The view transformation moves the world"
    (let [from (make-point 0 0 8)
          to   (make-point 0 0 0)
          up   (make-vector 0 1 0)
          t    (make-view-transform from to up)]
      (is (m= (translation 0 0 -8) t)))))

(deftest view-transform-4
  (testing "An arbitrary view transform"
    (let [from (make-point 1 2 3)
          to   (make-point 4 -2 8)
          up   (make-vector 1 1 0)
          t    (make-view-transform from to up)]
      (is (m= (make-matrix -0.4999999999999999  0.4999999999999999  0.7 -2.5999999999999996
                            0.7495331880577403  0.6505382386916236 0.07071067811865475 -2.2627416997969516
                           -0.4242640687119285  0.565685424949238 -0.7071067811865475 1.414213562373095
                            0.00000 0.00000  0.00000 1.00000) t)))))

(deftest camera-feature-test-1
  (testing "Camera creation"
    (let [hsize 160
          vsize 120
          fov   (/ Math/PI 2)
          c (make-camera hsize vsize fov)]
      (is (= hsize (:hsize c)))
      (is (= vsize (:vsize c)))
      (is (= fov   (:fov c)))
      (is (m= (identity-m) (:transform c))))))

(deftest camera-feature-test-2
  (testing "Camara pixel size"
    (let [c1 (make-camera 200 125 (/ Math/PI 2))
          c2 (make-camera 125 200 (/ Math/PI 2))]
      (is (= 0.009999999999999998 (:pixel-size c1)))
      (is (= 0.009999999999999998 (:pixel-size c2))))))

(deftest camera-feature-test-3
  (testing "Testing ray for pixel"
    (let [c  (make-camera 201 101 (/ Math/PI 2))
          c2 (set-camera-transform c (m*m (rotation-y (/ Math/PI 4)) (translation 0 -2 5)))
          r1 (ray-for-pixel c 100 50)
          r2 (ray-for-pixel c 0 0)
          r3 (ray-for-pixel c2 100 50)]
      (is (v= (make-point  0 0 0)  (:origin r1)))
      (is (v= (make-vector -0.40689086109769085  0.40689086109769096  -0.8178506308063589) (:direction r1)))
      (is (v= (make-point  0 0 0)  (:origin r2)))
      (is (v= (make-vector 0.3325932130597254  0.6651864261194508  -0.6685123582500481) (:direction r2)))
      (is (v= (make-point  0 2 -5)  (:origin r3)))
      (is (v= (make-vector 0.2905924399558609  0.4068908610976908  -0.8660230141258828)  (:direction r3))))))

(deftest render-function-test
  (testing "Testing render camera"
    (let [w (make-default-world)
          c (make-camera 11 11 (/ Math/PI 2))
          from (make-point 0 0 -5)
          to   (make-point 0 0 0)
          up   (make-vector 0 1 0)
          c2   (set-camera-transform c (make-view-transform from to up))
          image (render c2 w)]
      (is (c= (make-color 0.38066119308103435 0.47582649135129296 0.28549589481077575) (get-pixel image 5 5))))))

;; Shadows

(deftest shadows-test
  (testing "shadowed? function"
    (let [w  (make-default-world)
          p1 (make-point 0 10 0)
          p2 (make-point 10 -10 10)
          p3 (make-point -20 20 -20)
          p4 (make-point -2 2 -2)]
      (is (not (shadowed? w p1)))
      (is (shadowed? w p2))
      (is (not (shadowed? w p3)))
      (is (not (shadowed? w p4))))))

(deftest shadow-in-scene
  (testing "shadow-hit is hiven an intersection in shadow"
    (let [s1 (make-sphere)
          s2 (set-transform (make-sphere) (translation 0 0 10))
          w  (make-world (make-light-point (make-point 0 0 -10) (make-color 1 1 1)) s1 s2)
          r  (make-ray (make-point 0 0 5) (make-vector 0 0 1))
          i  (make-intersection 4 s2)
          comps (prepare-computations i r)
          res (shade-hit w comps 0)]
      (is (c= (make-color 0.1 0.1 0.1) res)))))

(deftest shadow-surface-correction
  (testing "the hit should offset the point"
    (let [r (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
          shape (set-transform (make-sphere) (translation 0 0 1))
          i (make-intersection 5 shape)
          comps (prepare-computations i r)]
      (is (< (z (:over-point comps)) (/ (* -1 0.00001) 2)))
      (is (> (z (:point comps)) (z (:over-point comps)))))))


;; Plane

(deftest plane-construction-norma
  (testing "Plane construction and normal function"
    (let [plane (make-plane)
          n1 ((:local-normal-at plane) plane (make-point 0 0 0))
          n2 ((:local-normal-at plane) plane (make-point 10 0 -10))
          n3 ((:local-normal-at plane) plane (make-point -5 0 150))]
      (is (v= n1 (make-vector 0 1 0)))
      (is (v= n2 (make-vector 0 1 0)))
      (is (v= n3 (make-vector 0 1 0))))))

(deftest plane-ray-intersection
  (testing "Plane - ray intersection"
    (let [plane (make-plane)
          r1  (make-ray (make-point 0 10 0) (make-vector 0 0 1))
          r2  (make-ray (make-point 0 0 0)  (make-vector 0 0 1))
          r3  (make-ray (make-point 0 1 0) (make-vector  0 -1 0))
          r4  (make-ray (make-point 0 -1 0) (make-vector 0 1 0))
          xs1 ((:local-intersect plane) plane r1)
          xs2 ((:local-intersect plane) plane r2)
          xs3 ((:local-intersect plane) plane r3)
          xs4 ((:local-intersect plane) plane r4)]
      (is (empty? xs1))
      (is (empty? xs2))
      (is (= 1.0 (:t (first xs3))))
      (is (= plane (:object (first xs3))))
      (is (= 1.0 (:t (first xs4))))
      (is (= plane (:object (first xs4)))))))

;; Patterns

(deftest patterns-feature-1
  (testing "Pattern creation"
    (let [pattern (make-stripe-pattern white black)]
      (is (c= white (:a (:pattern pattern))))
      (is (c= black (:b (:pattern pattern)))))))

(deftest patterns-feature-2
  (testing "Stripe pattern constant in y and z but alternates in x"
    (let [pattern (make-stripe-pattern white black)
          y1 (stripe-at pattern (make-point 0 0 0))
          y2 (stripe-at pattern (make-point 0 1 0))
          y3 (stripe-at pattern (make-point 0 2 0))
          z1 (stripe-at pattern (make-point 0 0 0))
          z2 (stripe-at pattern (make-point 0 0 1))
          z3 (stripe-at pattern (make-point 0 0 2))
          x1 (stripe-at pattern (make-point 0 0 0))
          x2 (stripe-at pattern (make-point 0.9 0 0))
          x3 (stripe-at pattern (make-point 1 0 0))
          x4 (stripe-at pattern (make-point -0.1 0 0))
          x5 (stripe-at pattern (make-point -1 0 0))
          x6 (stripe-at pattern (make-point -1.1 0 0))]
      (is (c= y1 white))
      (is (c= y2 white))
      (is (c= y3 white))
      (is (c= z1 white))
      (is (c= z2 white))
      (is (c= z3 white))
      (is (c= x1 white))
      (is (c= x2 white))
      (is (c= x3 black))
      (is (c= x4 black))
      (is (c= x5 black))
      (is (c= x6 white)))))

(deftest material-pattern
  (testing "Pattern addition to material"
    (let [material (set-ambient (set-diffuse (set-specular (set-pattern (make-material) (make-stripe-pattern white black)) 0) 0) 1)
          eyev (make-vector 0 0 -1)
          normalv (make-vector 0 0 -1)
          light (make-light-point (make-point 0 0 -10) (make-color 1 1 1))
          c1 (lighting material (make-sphere) light (make-point 0.9 0 0) eyev normalv false)
          c2 (lighting material (make-sphere) light (make-point 1.1 0 0) eyev normalv false)]
      (is (c= c1 white))
      (is (c= c2 black)))))

(deftest pattern-transformation-1
  (testing "Stripes with an object transformation"
    (let [obj1 (make-sphere)
          obj2 (set-transform obj1 (scaling 2 2 2))
          pattern (make-stripe-pattern white black)
          res (pattern-at-object pattern obj2 (make-point 1.5 0 0))]
      (is (c= res white)))))

(deftest pattern-transfomation-2
  (testing "Stripes with pattern transformation"
    (let [obj (make-sphere)
          pattern (set-pattern-transform (make-stripe-pattern white black) (scaling 2 2 2))
          res (pattern-at-object pattern obj (make-point 1.5 0 0))]
      (is (c= white res)))))

(deftest pattern-transfomation-3
  (testing "Stripes with both object and pattern transformation"
    (let [obj (set-transform (make-sphere) (scaling 2 2 2))
          pattern (set-pattern-transform (make-stripe-pattern white black) (translation 0.5 0 0))
          res (pattern-at-object pattern obj (make-point 2.5 0 0))]
      (is (c= white res)))))

(deftest gradient-pattern-test
  (testing "gradient creation"
    (let [pattern (make-gradient white black)
          r1 ((:pattern-at pattern) pattern (make-point 0 0 0))
          r2 ((:pattern-at pattern) pattern (make-point 0.25 0 0))
          r3 ((:pattern-at pattern) pattern (make-point 0.5 0 0))
          r4 ((:pattern-at pattern) pattern (make-point 0.75 0 0))]
      (is (c= r1 (make-color 1 1 1)))
      (is (c= r2 (make-color 0.75 0.75 0.75)))
      (is (c= r3 (make-color 0.5 0.5 0.5)))
      (is (c= r4 (make-color 0.25 0.25 0.25))))))

(deftest ring-pattern-test
  (testing "ring creation"
    (let [pattern (make-ring white black)
          r1 ((:pattern-at pattern) pattern (make-point 0 0 0))
          r2 ((:pattern-at pattern) pattern (make-point 1 0 0))
          r3 ((:pattern-at pattern) pattern (make-point 0 0 1))
          r4 ((:pattern-at pattern) pattern (make-point 0.708 0 0.708))]
      (is (c= r1 white))
      (is (c= r2 black))
      (is (c= r3 black))
      (is (c= r4 black)))))

(deftest checker-pattern-test-x
  (testing "checker creation"
    (let [pattern (make-checker white black)
          r1 ((:pattern-at pattern) pattern (make-point 0 0 0))
          r2 ((:pattern-at pattern) pattern (make-point 0.99 0 0))
          r3 ((:pattern-at pattern) pattern (make-point 1.01 0 0))]
      (is (c= r1 white))
      (is (c= r2 white))
      (is (c= r3 black)))))

(deftest checker-pattern-test-y
  (testing "checker creation"
    (let [pattern (make-checker white black)
          r1 ((:pattern-at pattern) pattern (make-point 0 0 0))
          r2 ((:pattern-at pattern) pattern (make-point 0 0.99 0))
          r3 ((:pattern-at pattern) pattern (make-point 0 1.01 0))]
      (is (c= r1 white))
      (is (c= r2 white))
      (is (c= r3 black)))))

(deftest checker-pattern-test-z
  (testing "checker creation"
    (let [pattern (make-checker white black)
          r1 ((:pattern-at pattern) pattern (make-point 0 0 0))
          r2 ((:pattern-at pattern) pattern (make-point 0 0 0.99))
          r3 ((:pattern-at pattern) pattern (make-point 0 0 1.01))]
      (is (c= r1 white))
      (is (c= r2 white))
      (is (c= r3 black))))

;; Reflection and Refraction

 (deftest material-reflectivity
   (testing "material reflection field"
     (let [material (make-material)]
       (is (= 0.0 (:reflective material)))))))

(deftest precompute-reflectv
  (testing "Prepare computations reflective vector"
    (let [shape (make-plane)
          ray   (make-ray (make-point 0 1 -1) (make-vector 0 (* -1 (/ (Math/sqrt 2) 2)) (/ (Math/sqrt 2) 2)))
          intersection (make-intersection (Math/sqrt 2) shape)
          comps (prepare-computations intersection ray)]
      (is (v= (make-vector 0 (/ (Math/sqrt 2) 2) (/ (Math/sqrt 2) 2)) (:reflectv comps))))))

(deftest strike-nonreflective-surface
  (testing "Test the strike of an none reflective surface"
    (let [world (make-default-world)
          ray   (make-ray (make-point 0 0 0) (make-vector 0 0 1))
          shape (set-material (last (:objects world)) (set-ambient (:material (:shape (last (:objects world)))) 1))
          intersection (make-intersection 1 shape)
          comps (prepare-computations intersection ray)
          res   (reflected-color world comps)]
      (is (c= black res)))))

(deftest strike-nonreflective-surface
  (testing "Test the strike of an reflective surface"
    (let [plane (make-plane)
          shape1 (set-material  plane  (set-reflectiveness (:material (:shape plane)) 0.5))
          shape2 (set-transform shape1 (translation 0 -1 0))
          w (add-shape (make-default-world) shape2)
          ray (make-ray (make-point 0 0 -3) (make-vector 0 (* -1 (/ (Math/sqrt 2) 2)) (/ (Math/sqrt 2) 2)))
          i (make-intersection (Math/sqrt 2) shape2)
          comps (prepare-computations i ray)
          color (reflected-color w comps 1)]
      (is (c= (make-color 0.1903323203795347 0.23791540047441834 0.142749240284651) color)))))


(deftest avoid-infinite-recursion
  (testing "Color-at with mutually reflective surfaces"
    (let [plane (make-plane)
          lower (set-transform (set-material plane (set-reflectiveness (:material (:shape plane)) 1)) (translation 0 -1 0))
          upper (set-transform (set-material plane (set-reflectiveness (:material (:shape plane)) 1)) (translation 0 1 0))
          w (make-world (make-light-point (make-point 0 0 0) (make-color 1 1 1)) lower upper)
          ray (make-ray (make-point 0 0 0) (make-vector 0 1 0))
          res (color-at w ray 0)]
      (is (c= (make-color 1.9000000000000001 1.9000000000000001 1.9000000000000001) res)))))

(deftest material-refraction-test
  (testing "Material refractive index"
    (let [material (make-material)]
      (is (= 0.0 (:transparency material)))
      (is (= 1.0 (:refractive-index material))))))

(deftest glass-sphere-test
  (testing "Making a sphere with a glassy material"
    (let [sphere (make-glass-sphere)]
      (is (m= (identity-m) (:transform (:shape sphere))))
      (is (= 1.0 (:transparency (:material (:shape sphere)))))
      (is (= 1.5 (:refractive-index (:material (:shape sphere))))))))

(deftest prepare-computations-n1-n2
  (testing "Calculating n1 and n2"
    (let [sa  (set-transform (make-glass-sphere) (scaling 2 2 2))
          sb  (set-transform (set-material (make-glass-sphere) (set-refractive-index (make-material) 2.0)) (translation 0 0 -0.25))
          sc  (set-transform (set-material (make-glass-sphere) (set-refractive-index (make-material) 2.5)) (translation 0 0 0.25))
          ray (make-ray (make-point 0 0 -4) (make-vector 0 0 1))
          xs1 (make-intersection 2 sa)
          xs2 (make-intersection 2.75 sb)
          xs3 (make-intersection 3.25 sc)
          xs4 (make-intersection 4.75 sb)
          xs5 (make-intersection 5.25 sc)
          xs6 (make-intersection 6 sa)
          xs  (make-intersections xs1 xs2 xs3 xs4 xs5 xs6)
          comps1 (prepare-computations xs1 ray xs)
          comps2 (prepare-computations xs2 ray xs)
          comps3 (prepare-computations xs3 ray xs)
          comps4 (prepare-computations xs4 ray xs)
          comps5 (prepare-computations xs5 ray xs)
          comps6 (prepare-computations xs6 ray xs)]
      (is (= 1.0 (:n1 comps1)))
      (is (= 1.5 (:n2 comps1)))
      (is (= 1.5 (:n1 comps2)))
      (is (= 2.0 (:n2 comps2)))
      (is (= 2.0 (:n1 comps3)))
      (is (= 2.5 (:n2 comps3)))
      (is (= 2.5 (:n1 comps4)))
      (is (= 2.5 (:n2 comps4)))
      (is (= 2.5 (:n1 comps5)))
      (is (= 1.5 (:n2 comps5)))
      (is (= 1.5 (:n1 comps6)))
      (is (= 1.0 (:n2 comps6))))))

(deftest under-point-test
  (testing "Compute under-point"
    (let [r (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
          shape (set-transform (make-glass-sphere) (translation 0 0 1))
          i  (make-intersection 5 shape)
          xs (make-intersections i)
          comps (prepare-computations i r xs)]
      (is (> (z (:under-point comps)) (/ 0.00001 2)))
      (is (< (z (:point comps)) (z (:under-point comps)))))))

(deftest refracted-color-opaque-object
  (testing "The reflacted color with an opaque surface"
    (let [w     (make-default-world)
          shape (first (:objects w))
          r  (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
          xs (make-intersections (make-intersection 4 shape) (make-intersection 6 shape))
          comps (prepare-computations (first xs) r xs)
          c (refracted-color w comps 5)]
      (c= black c))))

(deftest refracted-color-opaque-object-2
  (testing "The reflacted color with an opaque surface"
    (let [w     (make-default-world)
          shape (set-material (first (:objects w)) (make-material (make-color 1 1 1) 0.1 0.9 0.9 200 nil 0.0 1.0 1.5))
          r  (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
          xs (make-intersections (make-intersection 4 shape) (make-intersection 6 shape))
          comps (prepare-computations (first xs) r xs)
          c (refracted-color w comps 0)]
      (c= black c))))

(deftest refracted-color-3
  (testing "refracted color under total internal reflection"
    (let [w (make-default-world)
          shape (set-material (first (:objects w)) (make-material (make-color 1 1 1) 0.1 0.9 0.9 200 nil 0.0 1.0 1.5))
          r  (make-ray (make-point 0 0 (/ (Math/sqrt 2) 2)) (make-vector 0 1 0))
          xs (make-intersections (make-intersection (* -1 (/ (Math/sqrt 2) 2)) shape) (make-intersection (/ (Math/sqrt 2) 2) shape))
          comps (prepare-computations (last xs) r xs)
          c (refracted-color w comps 5)]
      (is (c= c black)))))

(deftest refracted-color-4
  (testing "Spawn a ray inside the innermost sphere"
    (let [w  (make-default-world)
          sa (set-material (first (:objects w)) (make-material (make-color 1 1 1) 1.0 0.9 0.9 200 (make-pattern white black) 0.0 1.0 1.5))
          sb (set-material (last  (:objects w)) (make-material (make-color 1 1 1) 0.1 0.9 0.9 200 nil 0.0 1.0 1.5))
          r  (make-ray (make-point 0 0 0.1) (make-vector 0 1 0))
          xs (make-intersections (make-intersection -0.9899 sa) (make-intersection -0.4899 sb) (make-intersection 0.4899 sb) (make-intersection 0.9899 sa))
          comps (prepare-computations (get xs 2) r xs)
          c (refracted-color w comps 5)]
      (is (c= c (make-color 0.08000000000000002 0.1 0.06))))))

(deftest shading-test-transparent
  (testing "Shade hit with a transparent material"
    (let [plane  (set-material (make-plane) (make-material (make-color 1 1 1) 0.1 0.9 0.9 200 nil 0.0 0.5 1.5))
          floor  (set-transform plane (translation 0 -1 0))
          sphere (set-material (make-sphere) (make-material (make-color 1 0 0) 0.5 0.9 0.9 200 nil 0.0 0.0 1.0))
          ball   (set-transform sphere (translation 0 -3.5 -0.5))
          w  (add-shape (add-shape (make-default-world) floor) ball)
          r  (make-ray (make-point 0 0 -3) (make-vector 0 (* -1 (/ (Math/sqrt 2) 2)) (/ (Math/sqrt 2) 2)))
          xs (make-intersections (make-intersection (Math/sqrt 2) floor))
          comps (prepare-computations (first xs) r xs)
          color (shade-hit w comps 5)]
      (is (c= color (make-color 0.9364253889815014 0.6864253889815014 0.6864253889815014))))))

(deftest schlick-test-1
  (testing "Schlick reflectance under total internal reflection"
    (let [shape (make-glass-sphere)
          r     (make-ray (make-point 0 0 (Math/sqrt 2)) (make-vector 0 1 0))
          xs    (make-intersections (make-intersection (* -1 (/ (Math/sqrt 2) 2)) shape)
                                    (make-intersection (Math/sqrt 2) shape))
          comps (prepare-computations (last xs) r xs)
          reflectance (schlick comps)]
      (is (= 1.0 reflectance)))))

(deftest schlick-test-2
  (testing "Schlick reflectance of a perpendicular ray"
    (let [shape (make-glass-sphere)
          r     (make-ray (make-point 0 0 0) (make-vector 0 1 0))
          xs    (make-intersections (make-intersection -1 shape)
                                    (make-intersection  1 shape))
          comps (prepare-computations (last xs) r xs)
          reflectance (schlick comps)]
      (is (= 0.04000000000000001 reflectance)))))

(deftest schlick-test-3
  (testing "Schlick aproximation with small angle"
    (let [shape (make-glass-sphere)
          r     (make-ray (make-point 0 0.99 -2) (make-vector 0 0 1))
          xs    (make-intersections (make-intersection 1.8589 shape))
          comps (prepare-computations (first xs) r xs)
          reflectance (schlick comps)]
      (is (= 0.48873081012212183 reflectance)))))

(deftest shading-test-reflective
  (testing "Shade hit with a reflective, transparent material"
    (let [plane  (set-material (make-plane) (make-material (make-color 1 1 1) 0.1 0.9 0.9 200 nil 0.5 0.5 1.5))
          floor  (set-transform plane (translation 0 -1 0))
          sphere (set-material (make-sphere) (make-material (make-color 1 0 0) 0.5 0.9 0.9 200 nil 0.0 0.0 1.0))
          ball   (set-transform sphere (translation 0 -3.5 -0.5))
          w  (add-shape (add-shape (make-default-world) floor) ball)
          r  (make-ray (make-point 0 0 -3) (make-vector 0 (* -1 (/ (Math/sqrt 2) 2)) (/ (Math/sqrt 2) 2)))
          xs (make-intersections (make-intersection (Math/sqrt 2) floor))
          comps (prepare-computations (first xs) r xs)
          color (shade-hit w comps 5)]
      (is (c= color (make-color 0.9339152130708506 0.6964343169445542  0.692430745759333))))))

;; Cube

(deftest cube-feature-1
  (testing "A ray intersects a cube "
    (let [c  (make-cube)
          r1 (make-ray (make-point 5 0.5  0) (make-vector -1 0 0))
          r2 (make-ray (make-point -5 0.5 0) (make-vector 1 0 0))
          r3 (make-ray (make-point 0.5 5  0) (make-vector  0 -1 0))
          r4 (make-ray (make-point 0.5 -5 0) (make-vector  0 1 0))
          r5 (make-ray (make-point 0.5 0  5) (make-vector  0 0 -1))
          r6 (make-ray (make-point 0.5 0 -5) (make-vector  0 0 1))
          inside (make-ray (make-point 0 0.5 0) (make-vector  0 0 1))
          xs1 ((:local-intersect c) c r1)
          xs2 ((:local-intersect c) c r2)
          xs3 ((:local-intersect c) c r3)
          xs4 ((:local-intersect c) c r4)
          xs5 ((:local-intersect c) c r5)
          xs6 ((:local-intersect c) c r6)
          xs7 ((:local-intersect c) c inside)]
      (is (= 4.0 (:t (first xs1))))
      (is (= 6.0 (:t (last  xs1))))
      (is (= 4.0 (:t (first xs2))))
      (is (= 6.0 (:t (last  xs2))))
      (is (= 4.0 (:t (first xs3))))
      (is (= 6.0 (:t (last  xs3))))
      (is (= 4.0 (:t (first xs4))))
      (is (= 6.0 (:t (last  xs4))))
      (is (= 4.0 (:t (first xs5))))
      (is (= 6.0 (:t (last  xs5))))
      (is (= 4.0 (:t (first xs6))))
      (is (= 6.0 (:t (last  xs6))))
      (is (= -1.0 (:t (first xs7))))
      (is (=  1.0 (:t (last  xs7)))))))

(deftest cube-feature-2
  (testing "A ray doesn`t intersect a cube"
    (let [c  (make-cube)
          r1 (make-ray (make-point -2 0 0) (make-vector 0.2673 0.5345 0.8018))
          r2 (make-ray (make-point 0 -2 0) (make-vector 0.8018 0.2673 0.5345))
          r3 (make-ray (make-point 0 0 -2) (make-vector 0.5345 0.8018 0.2673))
          r4 (make-ray (make-point 2 0 2) (make-vector 0 0 -1))
          r5 (make-ray (make-point 0 2 2) (make-vector 0 -1 0))
          r6 (make-ray (make-point 2 2 0) (make-vector -1 0 0))
          xs1 ((:local-intersect c) c r1)
          xs2 ((:local-intersect c) c r2)
          xs3 ((:local-intersect c) c r3)
          xs4 ((:local-intersect c) c r4)
          xs5 ((:local-intersect c) c r5)
          xs6 ((:local-intersect c) c r6)]
      (is (empty? xs1))
      (is (empty? xs2))
      (is (empty? xs3))
      (is (empty? xs4))
      (is (empty? xs5))
      (is (empty? xs6)))))

(deftest cube-feature-3
  (testing "Normal of the face of a cube"
    (let [c  (make-cube)
          p1 (make-point 1 0.5 -0.8)
          p2 (make-point -1 -0.2 0.9)
          p3 (make-point -0.4 1 -0.1)
          p4 (make-point 0.3 -1 -0.7)
          p5 (make-point -0.6 0.3 1)
          p6 (make-point 0.4 0.4 -1)
          p7 (make-point 1 1 1)
          p8 (make-point -1 -1 -1)
          r1 ((:local-normal-at c) c p1)
          r2 ((:local-normal-at c) c p2)
          r3 ((:local-normal-at c) c p3)
          r4 ((:local-normal-at c) c p4)
          r5 ((:local-normal-at c) c p5)
          r6 ((:local-normal-at c) c p6)
          r7 ((:local-normal-at c) c p7)
          r8 ((:local-normal-at c) c p8)]
      (is (v= r1 (make-vector  1 0 0)))
      (is (v= r2 (make-vector -1 0 0)))
      (is (v= r3 (make-vector  0 1 0)))
      (is (v= r4 (make-vector  0 -1 0)))
      (is (v= r5 (make-vector  0 0 1)))
      (is (v= r6 (make-vector  0 0 -1)))
      (is (v= r7 (make-vector  1 0 0)))
      (is (v= r8 (make-vector -1 0 0))))))

;; Cylinder

(deftest cylinder-feature-1
  (testing "Ray misses cylinder"
    (let [cyl (make-cylinder)
          dir1 (norm (make-vector 0 1 0))
          dir2 (norm (make-vector 0 1 0))
          dir3 (norm (make-vector 0 0 0))
          r1 (make-ray (make-point 1 0 0) dir1)
          r2 (make-ray (make-point 0 0 0) dir2)
          r3 (make-ray (make-point 0 0 -5) dir3)
          xs1 ((:local-intersect cyl) cyl r1)
          xs2 ((:local-intersect cyl) cyl r2)
          xs3 ((:local-intersect cyl) cyl r3)]
      (is (empty? xs1))
      (is (empty? xs2))
      (is (empty? xs3)))))

(deftest cylinder-feature-2
  (testing "Ray - cylinder intersection"
    (let [cyl  (make-cylinder 1 2 false)
          dir1 (norm (make-vector 0.1 1 0))
          dir2 (norm (make-vector 0 0 1))
          r1 (make-ray (make-point 0 1.5 0) dir1)
          r2 (make-ray (make-point 0 3 -5) dir2)
          r3 (make-ray (make-point 0 0 -5) dir2)
          r4 (make-ray (make-point 0 2 -5) dir2)
          r5 (make-ray (make-point 0 1 -5) dir2)
          r6 (make-ray (make-point 0 1.5 -2) dir2)
          xs1 ((:local-intersect cyl) cyl r1)
          xs2 ((:local-intersect cyl) cyl r2)
          xs3 ((:local-intersect cyl) cyl r3)
          xs4 ((:local-intersect cyl) cyl r4)
          xs5 ((:local-intersect cyl) cyl r5)
          xs6 ((:local-intersect cyl) cyl r6)]
      (is (= 0 (count xs1)))
      (is (= 0 (count xs2)))
      (is (= 0 (count xs3)))
      (is (= 0 (count xs4)))
      (is (= 0 (count xs5)))
      (is (= 2 (count xs6))))))

(deftest cylinder-feature-3
  (testing "Intersecting the caps of a closed cylinder"
    (let [cyl  (make-cylinder 1 2 true)
          dir1 (norm (make-vector 0 -1 0))
          dir2 (norm (make-vector 0 -1 2))
          dir3 (norm (make-vector 0 -1 1))
          dir4 (norm (make-vector 0 1 2))
          dir5 (norm (make-vector 0 1 1))
          r1 (make-ray (make-point 0 3 0)   dir1)
          r2 (make-ray (make-point 0 3 -2)  dir2)
          r3 (make-ray (make-point 0 4 -2)  dir3)
          r4 (make-ray (make-point 0 0 -2)  dir4)
          r5 (make-ray (make-point 0 -1 -2) dir5)
          xs1 ((:local-intersect cyl) cyl r1)
          xs2 ((:local-intersect cyl) cyl r2)
          xs3 ((:local-intersect cyl) cyl r3)
          xs4 ((:local-intersect cyl) cyl r4)
          xs5 ((:local-intersect cyl) cyl r5)]
      (is (= 2 (count xs1)))
      (is (= 2 (count xs2)))
      (is (= 2 (count xs3)))
      (is (= 2 (count xs4)))
      (is (= 2 (count xs5))))))

(deftest cylinder-feature-4
  (testing "Intersecting a cylinder 2"
    (let [cyl  (make-cylinder)
          dir1 (norm (make-vector 0 0 1))
          dir2 (norm (make-vector 0 0 1))
          dir3 (norm (make-vector 0.1 1 1))
          r1 (make-ray (make-point 1 0 -5)   dir1)
          r2 (make-ray (make-point 0 0 -5)   dir2)
          r3 (make-ray (make-point 0.5 0 -5) dir3)
          xs1 ((:local-intersect cyl) cyl r1)
          xs2 ((:local-intersect cyl) cyl r2)
          xs3 ((:local-intersect cyl) cyl r3)]
      (is (= 5.0 (:t (first xs1))))
      (is (= 5.0 (:t (last  xs1))))
      (is (= 4.0 (:t (first xs2))))
      (is (= 6.0 (:t (last  xs2))))
      (is (= 6.80798191702732  (:t (first xs3))))
      (is (= 7.088723439378861 (:t (last  xs3)))))))

(deftest cube-feature-5
  (testing "Normal of a capped cube"
    (let [c  (make-cylinder 1 2 true)
          p1 (make-point 0 1 0)
          p2 (make-point 0.5 1 0)
          p3 (make-point 0 1 0.5)
          p4 (make-point 0 2 0)
          p5 (make-point 0.5 2 0)
          p6 (make-point 0 2 0.5)
          r1 ((:local-normal-at c) c p1)
          r2 ((:local-normal-at c) c p2)
          r3 ((:local-normal-at c) c p3)
          r4 ((:local-normal-at c) c p4)
          r5 ((:local-normal-at c) c p5)
          r6 ((:local-normal-at c) c p6)]
      (is (v= r1 (make-vector  0 -1 0)))
      (is (v= r2 (make-vector  0 -1 0)))
      (is (v= r3 (make-vector  0 -1 0)))
      (is (v= r4 (make-vector  0 1 0)))
      (is (v= r5 (make-vector  0 1 0)))
      (is (v= r6 (make-vector  0 1 0))))))

;; Cone

(deftest cone-feature-1
  (testing "Cone - ray intersection"
    (let [cone (make-cone)
          dir1 (norm (make-vector 0 0 1))
          dir2 (norm (make-vector 1 1 1))
          dir3 (norm (make-vector -0.5 -1 1))
          dir4 (norm (make-vector 0 1 1))
          r1 (make-ray (make-point 0 0 -5) dir1)
          r2 (make-ray (make-point 0 0 -5) dir2)
          r3 (make-ray (make-point 1 1 -5) dir3)
          r4 (make-ray (make-point 0 0 -1) dir4)
          xs1 ((:local-intersect cone) cone r1)
          xs2 ((:local-intersect cone) cone r2)
          xs3 ((:local-intersect cone) cone r3)
          xs4 ((:local-intersect cone) cone r4)]
      (is (= 5.0 (:t (first xs1))))
      (is (= 5.0 (:t (last xs1))))
      (is (= 8.660254037844386 (:t (first xs2))))
      (is (= 8.660254037844386 (:t (last  xs2))))
      (is (= 4.550055679356354 (:t (first xs3))))
      (is (= 49.44994432064364 (:t (last  xs3))))
      (is (= 0.3535533905932738 (:t (first xs4)))))))

(deftest cone-feature-2
  (testing "Capped cone - ray intersection"
    (let [cone (make-cone -0.5 0.5 true)
          dr1  (norm (make-vector 0 1 0))
          dr2  (norm (make-vector 0 1 1))
          dr3  (norm (make-vector 0 1 0))
          r1 (make-ray (make-point 0 0 -5) dr1)
          r2 (make-ray (make-point 0 0 -0.25) dr2)
          r3 (make-ray (make-point 0 0 -0.25) dr3)
          xs1 ((:local-intersect cone) cone r1)
          xs2 ((:local-intersect cone) cone r2)
          xs3 ((:local-intersect cone) cone r3)]
      (is (= 0 (count xs1)))
      (is (= 2 (count xs2)))
      (is (= 4 (count xs3))))))

(deftest cone-feature-3
  (testing "Computing normal vector cone"
    (let [cone (make-cone)
          p1 (make-point 0 0 0)
          p2 (make-point 1 1 1)
          p3 (make-point -1 -1 0)
          r1 ((:local-normal-at cone) cone p1)
          r2 ((:local-normal-at cone) cone p2)
          r3 ((:local-normal-at cone) cone p3)]
      (is (v= (make-vector 0 0 0)  r1))
      (is (v= (make-vector 1 -1.4142135623730951 1)  r2))
      (is (v= (make-vector -1 1 0) r3)))))
