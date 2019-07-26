(ns render.camera
  (:use [la-math.matrix]
        [canvas.color]))

(defrecord Camera [hsize vsize fov transform half-width half-height pixel-size])

(defn- calc-pixel-size
  "Calculate the pixel size given the height and width"
  [hsize vsize fov]
  (let [half-view (Math/tan (/ fov 2))
        aspect (/ hsize vsize)]
    (if (>= aspect 1)
      {:hh (/ half-view aspect) :hw half-view :ps (/ (* 2 half-view) hsize)}
      {:hh half-view :hw (* half-view aspect) :ps (/ (* 2 (* half-view aspect)) hsize)})))

(defn make-camera
 "Create a camera record"
 [hsize vsize fov]
 (let [data (calc-pixel-size hsize vsize fov)]
   (Camera. hsize vsize fov (identity-m) (:hh data) (:hw data) (:ps data))))

(defn set-camera-transform
  "Change the transform of the camera"
  [camera transform]
  (assoc camera :transform transform))
