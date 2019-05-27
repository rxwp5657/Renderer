(ns canvas.canvas
  (:use [canvas.color]))

(defn make-canvas
  "Make a new canvas"
  [width height]
  {:width  width
   :height height
   :canvas (into-array (for [w (range width) h (range height)] (make-color 0 0 0)))})

(defn- get-coordinate
  "Map 2D index to 1D index"
  [w r c]
  (+ (* w r) c))

(defn get-pixel
  "Get the pixel at the r row and c column"
  [cv r c]
  (aget (:canvas cv) (get-coordinate (:width cv) r c)))

(defn write-pixel
  "Set pixel color"
  [cv r c color]
  (aset (:canvas cv) (get-coordinate (:width cv) r c) color))

(defn- write-header
  "Write ppm header to file name"
  [cv name]
  (let [id "P3\n"
        w (apply str (:width cv) " ")
        h (apply str (:height cv) "\n")
        mcv "255\n"]
    (spit name (str id w h mcv))))


(defn- get-pixels
  "Returns vector with pixel r g b components converted to 0 - 255 format"
  [pixel t]
  (let [np (pixel->255 pixel)]
    [(r np) " "
     (g np) " "
     (b np) t]))

(defn- write-ppm
  "Write pixel data to ppm file"
  [f p-data]
  (with-open [w (clojure.java.io/writer f :append true)]
    (.write w (apply str p-data))))

(defn- write-content
  "Write pixel data to file"
  [cv f]
  (loop [d (:canvas cv)
         num-pixels 0
         acc []]
    (cond
      (nil? (first d)) (write-ppm f acc)
      (= num-pixels 16) (recur (rest d) 0 (into acc (get-pixels (first d) "\n")))
      :else (recur (rest d) (inc num-pixels) (into acc (get-pixels (first d) " "))))))

(defn save-canvas
  "Create ppm file and save pixel data to it"
  ([cv]
   (save-canvas cv "render"))
  ([cv name]
   (let [f (str name ".ppm")]
     (do
       (write-header  cv f)
       (write-content cv f)))))
