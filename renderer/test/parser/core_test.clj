(ns parser.core-test
  (:use [clojure.test  :refer :all]
        [parser.parser :refer :all]
        [la-math.vector :refer :all] [la-math.matrix :refer :all]))

(deftest parser-feature-1
  (testing "Ignoring lines"
    (let [parser (parse-obj-file "/Users/charles/Projects/Renderer/Renderer/renderer/test/parser/gibberish.txt")]
      (is (= 5 (:ignored parser))))))

(deftest parser-feature-2
  (testing "Reading vertex lines"
    (let [parser (parse-obj-file "/Users/charles/Projects/Renderer/Renderer/renderer/test/parser/vertex.txt")]
      (is (v= (make-point -1 1 0)   (get (:vertex parser) 0)))
      (is (v= (make-point -1 0.5 0) (get (:vertex parser) 1)))
      (is (v= (make-point  1 0 0)   (get (:vertex parser) 2)))
      (is (v= (make-point  1 1 0)   (get (:vertex parser) 3))))))

(deftest parser-feature-2
  (testing "Reading vertex lines"
    (let [parser (parse-obj-file "/Users/charles/Projects/Renderer/Renderer/renderer/test/parser/faces.txt")
          t1 (first (:shapes (get (:groups parser) :default)))
          t2 (last  (:shapes (get (:groups parser) :default)))]
      (is (v= (:p1 t1) (get (:vertex parser) 0)))
      (is (v= (:p2 t1) (get (:vertex parser) 1)))
      (is (v= (:p3 t1) (get (:vertex parser) 2)))
      (is (v= (:p1 t2) (get (:vertex parser) 0)))
      (is (v= (:p2 t2) (get (:vertex parser) 2)))
      (is (v= (:p3 t2) (get (:vertex parser) 3))))))

(deftest parser-feature-3
  (testing "Triangulating polygons"
    (let [parser (parse-obj-file "/Users/charles/Projects/Renderer/Renderer/renderer/test/parser/polygon.txt")
          t1 (first   (:shapes (get (:groups parser) :default)))
          t2 (second  (:shapes (get (:groups parser) :default)))
          t3 (last    (:shapes (get (:groups parser) :default)))]
      (is (v= (:p1 t1) (get (:vertex parser) 0)))
      (is (v= (:p2 t1) (get (:vertex parser) 1)))
      (is (v= (:p3 t1) (get (:vertex parser) 2)))
      (is (v= (:p1 t2) (get (:vertex parser) 0)))
      (is (v= (:p2 t2) (get (:vertex parser) 2)))
      (is (v= (:p3 t2) (get (:vertex parser) 3)))
      (is (v= (:p1 t3) (get (:vertex parser) 0)))
      (is (v= (:p2 t3) (get (:vertex parser) 3)))
      (is (v= (:p3 t3) (get (:vertex parser) 4))))))

(deftest parser-feature-4
  (testing "Reading vertex lines"
    (let [parser (parse-obj-file "/Users/charles/Projects/Renderer/Renderer/renderer/test/parser/group.txt")
          t1 (first (:shapes (get (:groups parser) :FirstGroup)))
          t2 (first (:shapes (get (:groups parser) :SecondGroup)))]
      (is (v= (:p1 t1) (get (:vertex parser) 0)))
      (is (v= (:p2 t1) (get (:vertex parser) 1)))
      (is (v= (:p3 t1) (get (:vertex parser) 2)))
      (is (v= (:p1 t2) (get (:vertex parser) 0)))
      (is (v= (:p2 t2) (get (:vertex parser) 2)))
      (is (v= (:p3 t2) (get (:vertex parser) 3))))))

(deftest parser-feature-5
  (testing "making a group"
    (let [parser (parse-obj-file "/Users/charles/Projects/Renderer/Renderer/renderer/test/parser/group.txt")
          g (obj-to-group parser)]
      (is (= 2 (count (:shapes g)))))))
