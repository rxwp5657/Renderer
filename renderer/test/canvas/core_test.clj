(ns canvas.core-test
  (:require [clojure.test :refer :all]
            [canvas.color :refer :all]
            [canvas.canvas :refer :all]))

(deftest c-test
  (testing "Color type test")
  (let [c (make-color -0.5 0.4 1.7)]
    (are [t r] (= r t)
         -0.5 (r c)
         0.4  (g c)
         1.7  (b c))))

(deftest arithmetic-C
  (testing "Color arithmetic"
    (let [c1 (make-color 0.9 0.6 0.75)
          c2 (make-color 0.7 0.1 0.25)
          c3 (make-color 0.2 0.3 0.4)
          c4 (make-color 1 0.2 0.4)
          c5 (make-color 0.9 1 0.1)]
      (is (c= (c+ c1 c2)  (make-color 1.6 0.7 1.0)))
      (is (c= (c- c1 c2)  (make-color 0.20000000000000007 0.5 0.5)))
      (is (c= (c* c3 2)   (make-color 0.4 0.6 0.8)))
      (is (c= (c*c c4 c5) (make-color 0.9 0.2 0.04000000000000001))))))

(deftest canvas-test
  (testing "Canvas creation test"
    (let [canvas (make-canvas 10 8)]
      (is (= (:width  canvas) 10))
      (is (= (:height canvas) 8))
      (is (c= (first (:canvas canvas)) (make-color 0.0 0.0 0.0))))))

(deftest pixel-test
  (testing "Canvas pixel writing"
    (let [cv  (make-canvas 10 8)
          red (make-color 1 0 0)
          p (write-pixel cv 2 3 red)]
      (is (c= (get-pixel cv 2 3) red)))))

(deftest pixel-255
  (testing "Convert pixel 0 - 1 to 255"
    (let [pixel (make-color 1.5 0.5 0.32)]
      (is (c= (pixel->255 pixel) (make-color 255 128 82))))))

(deftest s-canvas
  (testing "Saving canvas"
    (let [cv (make-canvas 10 10)]
      (save-canvas cv))))
